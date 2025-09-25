open! Core

type t =
  { uninitialised_fields :
      [ `Uninit | `Uninit_needs_owned | `Semi_init | `Init ] Ast.Ident.Map.t
  ; needs_super : bool
  ; vtable_initialised : bool
  ; evolving_fields : Ast.Ident.Set.t
  }
[@@deriving equal, sexp_of]

let create
      ~(class_signature : Type.Class.t)
      ~(super_class_signature : Type.Class.t option)
      ~needs_super
  =
  ref
    { needs_super
    ; vtable_initialised = false
    ; uninitialised_fields =
        class_signature.fields
        |> Map.map ~f:(fun field ->
          match field.evolves with
          | true -> `Uninit_needs_owned
          | false -> `Uninit)
    ; evolving_fields =
        (match super_class_signature with
         | None -> Ast.Ident.Set.empty
         | Some c -> c.fields |> Map.filter ~f:Type.Class.Field.evolves |> Map.key_set)
    }
;;

let set_try_remove set v =
  match Set.mem set v with
  | true -> Some (Set.remove set v)
  | false -> None
;;

let can_initialise_field (constructor_state_ref : t ref) ~field =
  let cs = !constructor_state_ref in
  let init_field () =
    constructor_state_ref
    := { cs with
         uninitialised_fields =
           Map.set cs.uninitialised_fields ~key:field ~data:`Semi_init
       }
  in
  match Map.find cs.uninitialised_fields field with
  | None (* Tried to initialise field we do not need to initialise*)
  | Some (`Semi_init | `Init) -> `No
  | Some `Uninit_needs_owned ->
    init_field ();
    `With_ownership
  | Some `Uninit ->
    init_field ();
    `Yes
;;

let finish_initialising_field (cs_ref : t ref) ~field =
  let cs = !cs_ref in
  let evolving_fields =
    set_try_remove cs.evolving_fields field |> Option.value ~default:cs.evolving_fields
  in
  let uninitialised_fields =
    Map.update cs.uninitialised_fields field ~f:(function
      | Some `Semi_init -> `Init
      | None | Some (`Uninit | `Uninit_needs_owned | `Init) ->
        failwith "internal compiler error")
  in
  cs_ref := { cs with evolving_fields; uninitialised_fields }
;;

let read_field_ownership cs_ref ~field =
  let cs = !cs_ref in
  match set_try_remove cs.evolving_fields field with
  | Some evolving_fields ->
    cs_ref := { cs with evolving_fields };
    Type.Ownership.Owned
  | None -> Type.Ownership.Shared
;;

let is_complete = function
  | { uninitialised_fields = _; needs_super = true; _ } -> false
  | { uninitialised_fields; needs_super = false; _ } ->
    Map.for_all uninitialised_fields ~f:(function
      | `Init -> true
      | `Uninit | `Uninit_needs_owned | `Semi_init -> false)
;;

let initialise_vtable cs = cs := { !cs with vtable_initialised = true }
let initialise_super cs = cs := { !cs with needs_super = false }
