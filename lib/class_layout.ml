open! Core

module Row = struct
  type t =
    { fields : (int * int) Ast.Ident.Map.t
    ; mutable max_fields_size : int
    ; methods : (int * int) Ast.Ident.Map.t
    }
  [@@deriving fields ~getters, sexp_of]

  let size map =
    match
      map |> Map.data |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:fst)
    with
    | Some (max, size) -> max + size
    | None -> 0
  ;;

  let fields_size row = size row.fields
  let methods_size row = size row.methods
end

type t = Row.t Type.Class_id.Table.t [@@deriving sexp_of]

(* let fields_size t ~class_id = Hashtbl.find_exn t class_id |> Row.fields_size *)
let max_fields_size t ~class_id = Hashtbl.find_exn t class_id |> Row.max_fields_size
let methods_size t ~class_id = Hashtbl.find_exn t class_id |> Row.methods_size

let rec propogate_max_fields_size
          t
          ~(class_sig : Type.Class.t)
          ~(signatures : Type.Class.t Type.Class_id.Table.t)
          ~size
  =
  match class_sig.super with
  | None -> ()
  | Some super_id ->
    let super_layout : Row.t = Hashtbl.find_exn t super_id in
    (match Option.is_some class_sig.evolver && super_layout.max_fields_size < size with
     | true ->
       super_layout.max_fields_size <- size;
       let super_sig = Hashtbl.find_exn signatures super_id in
       propogate_max_fields_size t ~class_sig:super_sig ~signatures ~size
     | false -> ())
;;

let of_signatures ~sizeof (signatures : Type.Class.t Type.Class_id.Table.t) =
  let to_indexes ~overrides ~sizeof map ~start =
    let i = ref start in
    Map.filter_map map ~f:(fun x ->
      match overrides x with
      | true -> None
      | false ->
        (* TODO: maybe hard code this less? *)
        let size = sizeof x in
        let align = min size 8 in
        let n = (!i + (align - 1)) land -align in
        i := n + size;
        Some (n, size))
  in
  let to_field_indexes =
    to_indexes ~overrides:Type.Class.Field.overrides ~sizeof:(fun field ->
      sizeof field.ty)
  in
  let to_method_indexes =
    to_indexes ~overrides:Type.Class.Method.overrides ~sizeof:(fun _ -> 1)
  in
  let rec aux t ~id =
    let class_sig = Hashtbl.find_exn signatures id in
    match Hashtbl.find t id with
    | Some x -> x
    | None ->
      (match class_sig.super with
       | None ->
         let fields = to_field_indexes class_sig.fields ~start:0 in
         let methods = to_method_indexes class_sig.methods ~start:0 in
         let row : Row.t = { fields; methods; max_fields_size = Row.size fields } in
         Hashtbl.add_exn t ~key:id ~data:row;
         row
       | Some super_id ->
         let super_layout = aux t ~id:super_id in
         let fields =
           Map.merge_skewed
             ~combine:(fun ~key:_ _ x -> x)
             super_layout.fields
             (to_field_indexes class_sig.fields ~start:(Row.fields_size super_layout))
         in
         let methods =
           Map.merge_skewed
             ~combine:(fun ~key:_ _ x -> x)
             super_layout.methods
             (to_method_indexes class_sig.methods ~start:(Row.methods_size super_layout))
         in
         let size = Row.size fields in
         let row : Row.t = { fields; methods; max_fields_size = size } in
         propogate_max_fields_size t ~class_sig ~signatures ~size;
         Hashtbl.add_exn t ~key:id ~data:row;
         row)
  in
  let t = Type.Class_id.Table.create () in
  Hashtbl.iter_keys signatures ~f:(fun id -> ignore (aux t ~id));
  t
;;

let field_offset (t : t) ~class_id ~field =
  let fields = Hashtbl.find_exn t class_id |> Row.fields in
  Map.find_exn fields field |> fst
;;

let method_offset (t : t) ~class_id ~method_ =
  let methods = Hashtbl.find_exn t class_id |> Row.methods in
  Map.find_exn methods method_ |> fst
;;
