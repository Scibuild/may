open! Core

module Id = struct
  module Global = Resolved_ident.Global.Id
  module Class = Type.Class_id
  module Interface = Type.Interface_id

  module T = struct
    type t =
      | Global of Global.t
      | Class of Class.t
      | Interface of Interface.t
    [@@deriving sexp_of, hash, equal, compare]
  end

  include T
  include Hashable.Make_plain (T)

  let create = function
    | `Global -> Global (Global.create ())
    | `Class -> Class (Class.create ())
    | `Interface -> Interface (Interface.create ())
  ;;
end

module Row = struct
  type t =
    { id : Id.t
    ; fully_qualified_name : Ast.Path.t
    }
  [@@deriving fields, sexp_of]

  let create ~fqn ~id_type = { id = Id.create id_type; fully_qualified_name = fqn }
  let fqn = fully_qualified_name
end

module Tree = struct
  type t =
    { modules : t Ast.Ident.Table.t
    ; declarations : Row.t Ast.Ident.Table.t
    }
  [@@deriving sexp_of]

  let create () =
    { modules = Ast.Ident.Table.create (); declarations = Ast.Ident.Table.create () }
  ;;

  let find_module t = Hashtbl.find t.modules
  let find_module_exn t = Hashtbl.find_exn t.modules
  let find_decl t = Hashtbl.find t.declarations

  let rec find_path t path =
    match path with
    | [ d ] -> find_decl t d
    | m :: sub_path ->
      let%bind.Option sub_tree = find_module t m in
      find_path sub_tree sub_path
    | [] -> failwith "cannot find empty path"
  ;;

  let find_module_path_exn t scope = List.fold scope ~init:t ~f:find_module_exn

  let trees_to_search t ~scope =
    List.fold
      scope
      ~init:Nonempty_list.[ t ]
      ~f:(fun acc m ->
        let t = Nonempty_list.hd acc in
        Nonempty_list.cons (find_module_exn t m) acc)
  ;;

  let rec search_trees ~f Nonempty_list.(scope :: rest) =
    match f scope with
    | Some tree -> Some tree
    | None ->
      (match Nonempty_list.of_list rest with
       | None -> None
       | Some rest -> search_trees ~f rest)
  ;;

  let find_module_in_scope t ~scope m =
    let surrounding_trees = trees_to_search t ~scope in
    search_trees ~f:(Fn.flip find_module m) surrounding_trees
  ;;

  let find_decl_in_scope t ~scope d =
    let surrounding_trees = trees_to_search t ~scope in
    search_trees ~f:(Fn.flip find_decl d) surrounding_trees
  ;;

  let add_decl_exn t ~scope ~name ~row =
    let sub_tree = find_module_path_exn t scope in
    Hashtbl.add_exn sub_tree.declarations ~key:name ~data:row
  ;;

  let add_module t ~scope ~name =
    let sub_tree = find_module_path_exn t scope in
    Hashtbl.add sub_tree.modules ~key:name ~data:(create ())
  ;;

  let add_alias t ~scope ~name ~module_path =
    let scoped_tree = find_module_path_exn t scope in
    let aliased_tree = find_module_path_exn t module_path in
    Hashtbl.add scoped_tree.modules ~key:name ~data:aliased_tree
  ;;
end

type t =
  { by_id : (Id.t, Row.t) Hashtbl.t
  ; by_fqn : (Ast.Path.t, Row.t) Hashtbl.t
  ; tree : Tree.t
  }
[@@deriving sexp_of]

let create () =
  { by_id = Hashtbl.create (module Id)
  ; by_fqn = Hashtbl.create (module Ast.Path)
  ; tree = Tree.create ()
  }
;;

let insert t ~scope ~name ~is =
  let row = Row.create ~id_type:is ~fqn:(Ast.Path.of_name_in_scope ~name ~scope) in
  match Hashtbl.add t.by_fqn ~key:row.fully_qualified_name ~data:row with
  | `Duplicate -> `Already_defined
  | `Ok ->
    Hashtbl.add_exn t.by_id ~key:row.id ~data:row;
    Tree.add_decl_exn t.tree ~scope ~name ~row;
    `Ok row.id
;;

let insert_global t ~scope ~name =
  match insert t ~scope ~name ~is:`Global with
  | `Already_defined -> `Already_defined
  | `Ok (Global id) -> `Ok id
  | `Ok _ -> failwith "unreachable"
;;

let insert_class t ~scope ~name =
  match insert t ~scope ~name ~is:`Class with
  | `Already_defined -> `Already_defined
  | `Ok (Class id) -> `Ok id
  | `Ok _ -> failwith "unreachable"
;;

let insert_interface t ~scope ~name =
  match insert t ~scope ~name ~is:`Interface with
  | `Already_defined -> `Already_defined
  | `Ok (Interface id) -> `Ok id
  | `Ok _ -> failwith "unreachable"
;;

let insert_module t ~scope ~name = Tree.add_module t.tree ~scope ~name

let insert_module_alias t ~scope ~name ~module_path =
  Tree.add_alias t.tree ~scope ~name ~module_path
;;

let find_class_name t ~id = Row.fqn (Hashtbl.find_exn t.by_id (Id.Class id))
let find_global_name t ~id = Row.fqn (Hashtbl.find_exn t.by_id (Id.Global id))
let find_interface_name t ~id = Row.fqn (Hashtbl.find_exn t.by_id (Id.Interface id))

let find_id t ~scope ~qualified_name =
  let row =
    match Ast.Path.to_nonempty_list qualified_name with
    | [ d ] -> Tree.find_decl_in_scope t.tree ~scope d
    | m :: rest_of_path ->
      let%bind.Option subtree = Tree.find_module_in_scope t.tree ~scope m in
      Tree.find_path subtree rest_of_path
  in
  Option.map ~f:Row.id row
;;

let%expect_test "testing paths" =
  print_s [%sexp (Ast.Path.of_string "A.B.C.x" : Ast.Path.t)];
  [%expect {| (A B C x) |}];
  let ident = Ast.Ident.of_string in
  let path = Ast.Path.of_string in
  let assert_ok = function
    | `Ok -> ()
    | `Duplicate -> failwith "duplicate module name detected"
  in
  let env = create () in
  assert_ok (insert_module env ~scope:[] ~name:(ident "A"));
  let id_x = insert_global env ~scope:[] ~name:(ident "x") in
  let scope_a = [ ident "A" ] in
  let id_y = insert_global env ~scope:scope_a ~name:(ident "y") in
  let id_search_x = find_id env ~scope:[] ~qualified_name:(path "x") in
  let id_search_y = find_id env ~scope:scope_a ~qualified_name:(path "y") in
  let id_search_x_in_a = find_id env ~scope:scope_a ~qualified_name:(path "x") in
  print_s [%sexp (env : t)];
  [%expect
    {|
    ((by_id
      (((Global 0) ((id (Global 0)) (fully_qualified_name (x))))
       ((Global 1) ((id (Global 1)) (fully_qualified_name (A y))))))
     (by_fqn
      (((A y) ((id (Global 1)) (fully_qualified_name (A y))))
       ((x) ((id (Global 0)) (fully_qualified_name (x))))))
     (tree
      ((modules
        ((A
          ((modules ())
           (declarations ((y ((id (Global 1)) (fully_qualified_name (A y))))))))))
       (declarations ((x ((id (Global 0)) (fully_qualified_name (x)))))))))
    |}];
  print_s
    [%sexp
      { id_x : [ `Already_defined | `Ok of Id.Global.t ]
      ; id_y : [ `Already_defined | `Ok of Id.Global.t ]
      ; id_search_x : Id.t option
      ; id_search_y : Id.t option
      ; id_search_x_in_a : Id.t option
      }];
  [%expect
    {|
    ((id_x (Ok 0)) (id_y (Ok 1)) (id_search_x ((Global 0)))
     (id_search_y ((Global 1))) (id_search_x_in_a ((Global 0))))
    |}];
  assert_ok (insert_module env ~scope:[ ident "A" ] ~name:(ident "B"));
  assert_ok (insert_module env ~scope:[] ~name:(ident "B"));
  let id_a_b_x = insert_global env ~scope:[ ident "A"; ident "B" ] ~name:(ident "x") in
  let id_b_x = insert_global env ~scope:[ ident "B" ] ~name:(ident "x") in
  let id_b_y = insert_global env ~scope:[ ident "B" ] ~name:(ident "y") in
  let id_search_b_x_in_a =
    find_id env ~scope:[ ident "A" ] ~qualified_name:(path "B.x")
  in
  let id_search_a_b_x = find_id env ~scope:[] ~qualified_name:(path "A.B.x") in
  let id_search_b_y_in_a =
    find_id env ~scope:[ ident "A" ] ~qualified_name:(path "B.y")
  in
  let id_search_b_y = find_id env ~scope:[] ~qualified_name:(path "B.y") in
  Sexp.to_string_hum
    [%sexp
      { id_a_b_x : [ `Already_defined | `Ok of Id.Global.t ]
      ; id_b_x : [ `Already_defined | `Ok of Id.Global.t ]
      ; id_b_y : [ `Already_defined | `Ok of Id.Global.t ]
      ; id_search_b_x_in_a : Id.t option
      ; id_search_a_b_x : Id.t option
      ; id_search_b_y_in_a : Id.t option
      ; id_search_b_y : Id.t option
      }]
  |> print_endline;
  ();
  [%expect
    {|
    ((id_a_b_x (Ok 2)) (id_b_x (Ok 3)) (id_b_y (Ok 4))
     (id_search_b_x_in_a ((Global 2))) (id_search_a_b_x ((Global 2)))
     (id_search_b_y_in_a ()) (id_search_b_y ((Global 4))))
    |}]
;;
