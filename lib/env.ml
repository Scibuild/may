open! Core
open Env_intf

module type S = S

module Make (Id : Unique_id.Id) = struct
  module Id = Id

  module Row = struct
    type t =
      { id : Id.t
      ; fully_qualified_name : Ast.Path.t
      }
    [@@deriving fields, sexp_of]

    let create ~fqn = { id = Id.create (); fully_qualified_name = fqn }
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
    let find_decl t = Hashtbl.find t.declarations

    let rec find_path t path =
      match path with
      | [ d ] -> find_decl t d
      | m :: sub_path ->
        let%bind.Option sub_tree = find_module t m in
        find_path sub_tree sub_path
      | [] -> failwith "cannot find empty path"
    ;;

    let find_or_create_module t m =
      find_module t m
      |> Option.value_or_thunk ~default:(fun () ->
        let new_tree = create () in
        Hashtbl.add_exn t.modules ~key:m ~data:new_tree;
        new_tree)
    ;;

    let find_or_create_module_path t scope =
      List.fold scope ~init:t ~f:find_or_create_module
    ;;

    let find_module_in_scope t ~scope m =
      let aux (tree, old_best_m) = Option.first_some (find_module tree m) old_best_m in
      scope
      |> List.fold
           ~init:(t, find_module t m)
           ~f:(fun acc scope_m -> find_or_create_module (fst acc) scope_m, aux acc)
      |> aux
    ;;

    let find_decl_in_scope t ~scope d =
      let aux (tree, old_best_d) = Option.first_some (find_decl tree d) old_best_d in
      scope
      |> List.fold ~init:(t, None) ~f:(fun acc scope_m ->
        find_or_create_module (fst acc) scope_m, aux acc)
      |> aux
    ;;

    let add_exn t ~scope ~name ~row =
      let sub_tree = find_or_create_module_path t scope in
      Hashtbl.add_exn sub_tree.declarations ~key:name ~data:row
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

  let insert t ~scope ~name =
    let row = Row.create ~fqn:(Ast.Path.of_name_in_scope ~name ~scope) in
    match Hashtbl.add t.by_fqn ~key:row.fully_qualified_name ~data:row with
    | `Duplicate -> `Already_defined
    | `Ok ->
      Hashtbl.add_exn t.by_id ~key:row.id ~data:row;
      Tree.add_exn t.tree ~scope ~name ~row;
      `Ok row.id
  ;;

  let find_name t ~id = Row.fqn (Hashtbl.find_exn t.by_id id)

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
end

let%expect_test "testing paths" =
  let module Id = Unique_id.Int63 () in
  let module E = Make (Id) in
  print_s [%sexp (Ast.Path.of_string "A.B.C.x" : Ast.Path.t)];
  [%expect {| (A B C x) |}];
  let ident = Ast.Ident.of_string in
  let path = Ast.Path.of_string in
  let env = E.create () in
  let id_x = E.insert env ~scope:[] ~name:(ident "x") in
  let scope_a = [ Ast.Ident.of_string "A" ] in
  let id_y = E.insert env ~scope:scope_a ~name:(ident "y") in
  let id_search_x = E.find_id env ~scope:[] ~qualified_name:(path "x") in
  let id_search_y = E.find_id env ~scope:scope_a ~qualified_name:(path "y") in
  let id_search_x_in_a = E.find_id env ~scope:scope_a ~qualified_name:(path "x") in
  print_s [%sexp (env : E.t)];
  [%expect
    {|
    ((by_id
      ((0 ((id 0) (fully_qualified_name (x))))
       (1 ((id 1) (fully_qualified_name (A y))))))
     (by_fqn
      (((A y) ((id 1) (fully_qualified_name (A y))))
       ((x) ((id 0) (fully_qualified_name (x))))))
     (tree
      ((modules
        ((A
          ((modules ())
           (declarations ((y ((id 1) (fully_qualified_name (A y))))))))))
       (declarations ((x ((id 0) (fully_qualified_name (x)))))))))
    |}];
  print_s
    [%sexp
      { id_x : [ `Already_defined | `Ok of Id.t ]
      ; id_y : [ `Already_defined | `Ok of Id.t ]
      ; id_search_x : Id.t option
      ; id_search_y : Id.t option
      ; id_search_x_in_a : Id.t option
      }];
  [%expect
    {|
    ((id_x (Ok 0)) (id_y (Ok 1)) (id_search_x (0)) (id_search_y (1))
     (id_search_x_in_a (0)))
    |}];
  let id_a_b_x = E.insert env ~scope:[ ident "A"; ident "B" ] ~name:(ident "x") in
  let id_b_x = E.insert env ~scope:[ ident "B" ] ~name:(ident "x") in
  let id_b_y = E.insert env ~scope:[ ident "B" ] ~name:(ident "y") in
  let id_search_b_x_in_a =
    E.find_id env ~scope:[ ident "A" ] ~qualified_name:(path "B.x")
  in
  let id_search_a_b_x = E.find_id env ~scope:[] ~qualified_name:(path "A.B.x") in
  let id_search_b_y_in_a =
    E.find_id env ~scope:[ ident "A" ] ~qualified_name:(path "B.y")
  in
  let id_search_b_y = E.find_id env ~scope:[] ~qualified_name:(path "B.y") in
  Sexp.to_string_hum
    [%sexp
      { id_a_b_x : [ `Already_defined | `Ok of Id.t ]
      ; id_b_x : [ `Already_defined | `Ok of Id.t ]
      ; id_b_y : [ `Already_defined | `Ok of Id.t ]
      ; id_search_b_x_in_a : Id.t option
      ; id_search_a_b_x : Id.t option
      ; id_search_b_y_in_a : Id.t option
      ; id_search_b_y : Id.t option
      }]
  |> print_endline;
  ();
  [%expect
    {|
    ((id_a_b_x (Ok 2)) (id_b_x (Ok 3)) (id_b_y (Ok 4)) (id_search_b_x_in_a (2))
     (id_search_a_b_x (2)) (id_search_b_y_in_a ()) (id_search_b_y (4)))
    |}]
;;
