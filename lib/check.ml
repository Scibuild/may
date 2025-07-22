open! Core
open Result.Let_syntax

let create_type_error = Comp_error.create ~stage:Type_checker

let create_type_error_result ~range ~msg =
  Comp_error.create ~stage:Type_checker ~range ~msg |> Error
;;

module List = struct
  include List

  (** Like [List.map ~f >> Result.all] but does not compute terms after the 
      error. *)
  let map_result =
    let rec aux t f result =
      match t with
      | [] -> Ok (List.rev result)
      | hd :: tl ->
        (match f hd with
         | Error _ as err -> err
         | Ok v -> (aux [@tailcall]) tl f (v :: result))
    in
    fun t ~f -> aux t f []
  ;;

  let iter_result =
    let rec aux t f =
      match t with
      | [] -> Ok ()
      | hd :: tl ->
        (match f hd with
         | Error _ as err -> err
         | Ok () -> (aux [@tailcall]) tl f)
    in
    fun t ~f -> aux t f
  ;;
end

module Global_id = Resolved_ident.Global.Id
module Global_env = Env.Make (Global_id)
module Class_env = Env.Make (Type.Class_id)

module Constructor_state : sig
  type t = private
    { uninitialised_fields : Ast.Ident.Set.t
    ; needs_super : bool
    ; vtable_initialised : bool
    }
  [@@deriving equal, sexp_of]

  val create : class_signature:Type.Class.t -> needs_super:bool -> t ref
  val try_initialise_field : t ref -> field:Ast.Ident.t -> bool
  val is_complete : t -> bool
  val initialise_vtable : t ref -> unit
  val initialise_super : t ref -> unit
end = struct
  type t =
    { uninitialised_fields : Ast.Ident.Set.t
    ; needs_super : bool
    ; vtable_initialised : bool
    }
  [@@deriving equal, sexp_of]

  let create ~(class_signature : Type.Class.t) ~needs_super =
    ref
      { uninitialised_fields = Map.key_set class_signature.fields
      ; needs_super
      ; vtable_initialised = false
      }
  ;;

  (** Returns true if the field was successfully initialised. *)
  let try_initialise_field constructor_state_ref ~field =
    let cs = !constructor_state_ref in
    match Set.mem cs.uninitialised_fields field with
    | false -> false
    | true ->
      let new_uninit = Set.remove cs.uninitialised_fields field in
      constructor_state_ref := { cs with uninitialised_fields = new_uninit };
      true
  ;;

  let is_complete = function
    | { uninitialised_fields = _; needs_super = true; _ } -> false
    | { uninitialised_fields; needs_super = false; _ } ->
      Set.is_empty uninitialised_fields
  ;;

  let initialise_vtable cs = cs := { !cs with vtable_initialised = true }
  let initialise_super cs = cs := { !cs with needs_super = false }
end

module Function_check = struct
  type t =
    { local_env : (Resolved_ident.Local.t * int) Ast.Ident.Map.t
    ; scope_level : int
    ; return_type : Type.t
    ; local_types : Type.t Resolved_ident.Local.Map.t ref
      (* I'm not really sure what the best way to do this is, the idea of the 
     constructor state is that it keeps track of the initialised fields in a 
     constructor. It should be possible to write to read-only fields if they 
     aren't initialised yet. We also track if a super constructor call is needed*)
    ; constructor_state : Constructor_state.t ref option
    ; this_id : Type.Class_id.t option
    }

  let create ~return_type ?this_id ?constructor_state () =
    { local_env = Ast.Ident.Map.empty
    ; scope_level = 0
    ; return_type
    ; local_types = ref Resolved_ident.Local.Map.empty
    ; this_id
    ; constructor_state
    }
  ;;

  let insert_local t ~range ~ident ~ty =
    let%bind () =
      match Map.find t.local_env ident with
      | Some (_, existing_scope_level) when existing_scope_level = t.scope_level ->
        create_type_error_result
          ~range
          ~msg:
            [ Text "Identifier defined multiple times within the same scope "
            ; Ident ident
            ]
      | None | Some _ -> Ok ()
    in
    let local_types = !(t.local_types) in
    let fresh_local =
      Resolved_ident.Local.create ~name:ident ~id:(Map.length local_types)
    in
    let new_local_env =
      Map.set t.local_env ~key:ident ~data:(fresh_local, t.scope_level)
    in
    t.local_types := Map.add_exn local_types ~key:fresh_local ~data:ty;
    Ok ({ t with local_env = new_local_env }, fresh_local)
  ;;

  let with_new_scope t = { t with scope_level = t.scope_level + 1 }

  let resolve_local t ~ident =
    let resolved = Map.find t.local_env ident in
    Option.map resolved ~f:(fun (resolved, _) ->
      resolved, Map.find_exn !(t.local_types) resolved)
  ;;

  let update_type t ~local ~ty =
    t.local_types := Map.set !(t.local_types) ~key:local ~data:ty
  ;;
end

type t =
  { function_check : Function_check.t option
  ; global_env : Global_env.t
  ; global_types : Type.t Global_id.Table.t
  ; class_env : Class_env.t
  ; class_table : Type.Class.t Type.Class_id.Table.t
  ; scope : Ast.Ident.t list
  ; mode : Mode.t
  }
[@@deriving fields ~getters]

let empty ~mode =
  { function_check = None
  ; global_env = Global_env.create ()
  ; global_types = Global_id.Table.create ()
  ; class_env = Class_env.create ()
  ; class_table = Type.Class_id.Table.create ()
  ; scope = []
  ; mode
  }
;;

let with_new_module_scope t ~name = { t with scope = t.scope @ [ name ] }
let with_module_scope t ~scope = { t with scope }
let resolve_class t ~class_id = Hashtbl.find_exn t.class_table class_id

module Type = struct
  include Type

  let to_string t =
    Type.to_string ~resolve_class_name:(fun class_id ->
      let fqn = Class_env.find_name t.class_env ~id:class_id in
      Ast.Path.to_string fqn)
  ;;

  let code_str t ty = ty |> to_string t |> Comp_error.Msg_part.Code_str

  let rec is_subtype t ~sub ~super =
    match sub, super with
    | Bottom, _ -> true
    (* Mutable arrays are invariant, but Immutable arrays are covariant. 
       Additionally, immutable arrays are a subtype of mutable arrays.

       Since we have transitivity this gives the rule that if A < B then 
       []A < []_ B for _ either mut or not.
    *)
    | Array { mut = _; elt = sub_elt }, Array { mut = false; elt = super_elt } ->
      is_subtype t ~sub:sub_elt ~super:super_elt
    (* Functions obey standard function subtyping rules, covariant in the return
       and contravariant in the arguments. There has to be the same number of 
       arguments. *)
    | Fun { args = sub_args; ret = sub_ret }, Fun { args = super_args; ret = super_ret }
      ->
      is_subtype t ~sub:sub_ret ~super:super_ret
      &&
      let args_check =
        List.for_all2 sub_args super_args ~f:(fun sub_arg super_arg ->
          is_subtype t ~sub:super_arg ~super:sub_arg)
      in
      (match args_check with
       | Ok v -> v
       | Unequal_lengths -> false)
    | (Object subclass_id | Owned_object subclass_id), Object superclass_id ->
      (match Class_id.equal subclass_id superclass_id with
       | true -> true
       | false ->
         let subclass = resolve_class t ~class_id:subclass_id in
         (match subclass.super with
          | Some subclass_super_id -> is_subtype t ~sub:(Object subclass_super_id) ~super
          | None -> false))
    | (Object _ | Owned_object _), Top_object -> true
    | Top_object, Top_object -> true
    | Option sub, Option super | sub, Option super -> is_subtype t ~sub ~super
    | _ -> equal sub super
  ;;

  let check_subtype t ~range ~sub ~super =
    match is_subtype t ~sub ~super with
    | true -> Ok ()
    | false ->
      create_type_error_result
        ~range
        ~msg:
          [ Text "Type "
          ; code_str t sub
          ; Text " is not included in type "
          ; code_str t super
          ]
  ;;

  (** Two types are comparable if either lhs < rhs, or rhs < lhs. This means we
      can test a == b or a != b. *)
  let check_comparable t ~range ~lhs ~rhs =
    match is_subtype t ~sub:lhs ~super:rhs || is_subtype t ~sub:rhs ~super:lhs with
    | true -> Ok ()
    | false ->
      create_type_error_result
        ~range
        ~msg:
          [ Text "Types "
          ; code_str t lhs
          ; Text " and "
          ; code_str t rhs
          ; Text " are not comparable, i.e. one is not a subtype of the other"
          ]
  ;;

  (** [join ~range a b] Finds the best type that is the super type of both of 
      [a] and [b], if it exists, returning an error if it does not. *)
  let join t ~range a b =
    match is_subtype t ~sub:a ~super:b with
    | true -> Ok b
    | false ->
      (match is_subtype t ~sub:b ~super:a with
       | true -> Ok a
       | false ->
         create_type_error_result
           ~range
           ~msg:
             [ Text "Cannot find a super type of both "
             ; code_str t a
             ; Text " and "
             ; code_str t b
             ])
  ;;

  let is_integral = function
    | Numeric n -> Numeric.is_integral n
    | _ -> false
  ;;
end

module Class = struct
  (* TODO: The find_method and find_field are basically the same except for 
     their types and the places they search. Maybe think about restructuring 
     classes to make this more overlapping. *)

  (** Finds the definition of a method in a class. If [recursive] is true then
      super classes are also checked. If [through_override] is true then we 
      also continue searching in super classes if the method is an override 
      method. If [recursive] is false then [through_override] does nothing. *)
  let rec find_method
            t
            ?(recursive = true)
            ?(through_override = false)
            ~class_id
            ~name
            ~visibility
            ()
    =
    let class_ = resolve_class t ~class_id in
    let method_in_class =
      Map.find class_.methods name
      |> Option.bind ~f:(fun method_in_class ->
        Option.some_if
          (Ast.Decl.Visibility.is_visible method_in_class.visibility ~to_:visibility
           && not (method_in_class.overrides && through_override))
          method_in_class)
    in
    let search_super super =
      Option.bind super ~f:(fun class_id ->
        find_method t ~recursive ~through_override ~class_id ~name ~visibility ())
    in
    match recursive with
    | false -> method_in_class
    | true ->
      (match method_in_class with
       | None -> search_super class_.super
       | Some _ -> method_in_class)
  ;;

  (** Finds the definition of a field in a class. If [recursive] is true then
      super classes are also checked. If [through_override] is true then we 
      also continue searching in super classes if the field is an override 
      field. If [recursive] is false then [through_override] does nothing.

      By default [recursive] is true and [through override] is false.
      *)
  let rec find_field
            t
            ?(recursive = true)
            ?(through_override = false)
            ~class_id
            ~name
            ~visibility
            ()
    =
    let class_ = resolve_class t ~class_id in
    let field_in_class =
      Map.find class_.fields name
      |> Option.bind ~f:(fun field_in_class ->
        Option.some_if
          (Ast.Decl.Visibility.is_visible field_in_class.visibility ~to_:visibility
           && not (field_in_class.overrides && through_override))
          field_in_class)
    in
    let search_super super =
      Option.bind super ~f:(fun class_id ->
        find_field t ~recursive ~through_override ~class_id ~name ~visibility ())
    in
    match recursive with
    | false -> field_in_class
    | true ->
      (match field_in_class with
       | None -> search_super class_.super
       | Some _ -> field_in_class)
  ;;

  let is_inheritance_acyclic t (class_signature : Type.Class.t) =
    let rec aux seen_ids = function
      | None -> true
      | Some class_id ->
        (match List.mem seen_ids class_id ~equal:Type.Class_id.equal with
         | true -> false
         | false ->
           let super_class = resolve_class t ~class_id in
           aux (class_id :: seen_ids) super_class.super)
    in
    aux [ class_signature.id ] class_signature.super
  ;;

  let super_type_of_class_id t ~class_id =
    match (resolve_class t ~class_id).super with
    | None -> Type.Top_object
    | Some super_id -> Type.Object super_id
  ;;

  let constructor_is_complete = function
    | None -> true
    | Some constructor_state -> Constructor_state.is_complete !constructor_state
  ;;

  let type_this t ~range =
    let msg =
      [ Comp_error.Msg_part.Text
          "Cannot use 'this' outside of a class method/constructor."
      ]
    in
    let%bind function_check =
      t.function_check |> Comp_error.Or_error.of_option ~stage:Type_checker ~range ~msg
    in
    let%map this_id =
      function_check.this_id
      |> Comp_error.Or_error.of_option ~stage:Type_checker ~range ~msg
    in
    match function_check.constructor_state with
    (* If we're not in a constructor, it's easy. *)
    | None -> Type.Object this_id
    | Some cs ->
      (match !cs.needs_super with
       | true ->
         (* If the super type is not initialised yet *)
         Type.Top_object
       | false ->
         (* If we have made the super call then we check if construction is 
            complete to decide between this and the super type.*)
         (match constructor_is_complete function_check.constructor_state with
          | true -> Type.Object this_id
          | false -> super_type_of_class_id t ~class_id:this_id))
  ;;

  let type_super t ~range =
    (* Typing super is much like typing this, except it never becomes the 'this'
       type. *)
    let msg =
      [ Comp_error.Msg_part.Text
          "Cannot use 'super' outside of a class method/constructor."
      ]
    in
    let%bind function_check =
      t.function_check |> Comp_error.Or_error.of_option ~stage:Type_checker ~range ~msg
    in
    let%map this_id =
      function_check.this_id
      |> Comp_error.Or_error.of_option ~stage:Type_checker ~range ~msg
    in
    match function_check.constructor_state with
    | None -> super_type_of_class_id t ~class_id:this_id
    | Some cs ->
      (match !cs.needs_super with
       | false -> super_type_of_class_id t ~class_id:this_id
       | true -> Type.Top_object)
  ;;

  let _ = type_super
end

module Typestate : sig
  type check := t
  type t

  val save : check -> t
  val restore : check -> t -> unit
  val assert_equal : check -> range:Range.t -> t -> t -> (unit, Comp_error.t) result
end = struct
  type t =
    | Not_in_function
    | Typestate of Constructor_state.t option * Type.t Resolved_ident.Local.Map.t

  let save t =
    match t.function_check with
    | None -> Not_in_function
    | Some function_check ->
      Typestate
        ( Option.map function_check.constructor_state ~f:( ! )
        , !(function_check.local_types) )
  ;;

  let restore t ts =
    match ts with
    | Not_in_function -> ()
    | Typestate (cs, tys) ->
      (* We must be in a function. *)
      let function_check = Option.value_exn t.function_check in
      (match cs with
       | None -> ()
       | Some cs ->
         (* Similarly, we should have a constructor state. *)
         Option.value_exn function_check.constructor_state := cs);
      let curr_tys = !(function_check.local_types) in
      function_check.local_types
      := Map.merge_skewed tys curr_tys ~combine:(fun ~key:_ ty1 _ -> ty1)
  ;;

  let assert_equal t ~range ts1 ts2 =
    match ts1, ts2 with
    | Not_in_function, Not_in_function -> Ok ()
    | Not_in_function, _ | _, Not_in_function ->
      failwith "compiler bug, comparing typestates from inside and outside a function"
    | Typestate (cs1, tys1), Typestate (cs2, tys2) ->
      let cs_eq = [%equal: Constructor_state.t option] cs1 cs2 in
      let tys_compatible =
        Map.fold_symmetric_diff
          tys1
          tys2
          ~data_equal:Type.equal
          ~init:[]
          ~f:(fun acc diff ->
            match snd diff with
            | `Left _ | `Right _ -> acc
            | `Unequal (ty1, ty2) -> (fst diff, ty1, ty2) :: acc)
      in
      let%bind.Result () =
        Result.ok_if_true
          cs_eq
          ~error:
            (create_type_error
               ~range
               ~msg:[ Text "Field initialisation does not match on diverging branches" ])
      in
      (match tys_compatible with
       | [] -> Ok ()
       | (local, ty1, ty2) :: _ ->
         create_type_error_result
           ~range
           ~msg:
             [ Text "Local variable "
             ; Text (Resolved_ident.Local.to_string local)
             ; Text " ends with type "
             ; Type.code_str t ty1
             ; Text " on one branch, but type "
             ; Type.code_str t ty2
             ; Text " on another"
             ])
  ;;
end

let with_local t ~range ~ident ~ty =
  match t.function_check with
  | None ->
    create_type_error_result
      ~range
      ~msg:[ Text "Cannot create local variables outside of function." ]
  | Some function_check ->
    let%bind new_function_check, resolved_local =
      Function_check.insert_local function_check ~range ~ident ~ty
    in
    Ok ({ t with function_check = Some new_function_check }, resolved_local)
;;

let with_new_scope t ~range =
  match t.function_check with
  | None ->
    create_type_error_result
      ~range
      ~msg:[ Text "Cannot create local scope outside of function." ]
  | Some function_check ->
    Ok { t with function_check = Some (Function_check.with_new_scope function_check) }
;;

let lookup_global t ~(path : Ast.Path.t) ~range =
  let%map global_id =
    Global_env.find_id t.global_env ~scope:t.scope ~qualified_name:path
    |> Comp_error.Or_error.of_option
         ~stage:Type_checker
         ~range
         ~msg:[ Text "Unknown identifier "; Path path ]
  in
  global_id, Hashtbl.find_exn t.global_types global_id
;;

let lookup_local_ident t ~(ident : Ast.Ident.t) =
  Option.bind t.function_check ~f:(fun function_check ->
    (* We check if the path contains just a single name. If it does, then this 
      could be a local. *)
    Function_check.resolve_local function_check ~ident)
;;

let lookup_local_path t ~path =
  match Ast.Path.to_nonempty_list path with
  | [ ident ] -> lookup_local_ident t ~ident
  | _ -> None
;;

let lookup_path t ~(path : Ast.Path.t) ~range =
  let resolved_local_opt = lookup_local_path t ~path in
  match resolved_local_opt with
  | Some (resolved_local, ty) -> Ok (Tast.Expr.Local resolved_local, ty)
  | None ->
    let%bind resolved_global, ty = lookup_global t ~path ~range in
    Ok (Tast.Expr.Global resolved_global, ty)
;;

let lookup_class t ~path ~range =
  Class_env.find_id t.class_env ~scope:t.scope ~qualified_name:path
  |> Comp_error.Or_error.of_option
       ~stage:Type_checker
       ~range
       ~msg:[ Text "Unknown type identifier "; Path path ]
;;

let update_local_type t ~local ~ty =
  Function_check.update_type (Option.value_exn t.function_check) ~local ~ty
;;

let rec check_type t (ast_ty : Ast.Type.t) =
  let Ast.Node.{ range; data } = ast_ty in
  match data with
  | Ident ident ->
    (match Ast.Ident.to_string ident with
     | "int" -> Type.Numeric Int |> Ok
     | "char" -> Type.Numeric Char |> Ok
     | "float" -> Type.Numeric Float |> Ok
     | "bool" -> Type.Bool |> Ok
     | "unit" -> Type.Unit |> Ok
     | "object" -> Type.Top_object |> Ok
     | _ ->
       create_type_error_result ~range ~msg:[ Text "Unknown type name "; Ident ident ])
  | Path path ->
    let%map class_id = lookup_class t ~path ~range in
    Type.Object class_id
  | Owned path ->
    let%bind () =
      match t.mode with
      | Mode.With_ownership -> Ok ()
      | Mode.Without ->
        create_type_error_result ~range ~msg:[ Text "Ownership types are disabled" ]
    in
    let%map class_id = lookup_class t ~path ~range in
    Type.Owned_object class_id
  | Array { mut; elt } ->
    let%bind elt = check_type t elt in
    return (Type.Array { mut; elt })
  | Option ty ->
    let%bind checked_ty = check_type t ty in
    (match Type.is_reference checked_ty with
     | true -> return (Type.Option checked_ty)
     | false ->
       create_type_error_result
         ~range
         ~msg:
           [ Type.code_str t checked_ty
           ; Text " is not a reference type and so cannot take option."
           ])
;;

let is_in_constructor t =
  match t.function_check with
  | None -> false
  | Some function_check ->
    (match function_check.constructor_state with
     | None -> false
     | Some _ -> true)
;;

let check_vtable_update t ~range ~(expr : Tast.Expr.t) =
  match t.function_check with
  | None -> Ok expr
  | Some function_check ->
    (match function_check.constructor_state with
     | None -> Ok expr
     | Some constructor_state ->
       (match
          (not !constructor_state.vtable_initialised)
          && Class.constructor_is_complete function_check.constructor_state
        with
        | false -> Ok expr
        | true ->
          let this_id = Option.value_exn function_check.this_id in
          Constructor_state.initialise_vtable constructor_state;
          Ok
            (Tast.Expr.create
               ~expr:(Update_this_vtable_after { expr; new_table = this_id })
               ~range
               ~ty:(Tast.Expr.ty expr))))
;;

let rec check_expr t ~ty ~(term : Ast.Expr.t) =
  let Ast.Node.{ data; range } = term in
  let check_infer () =
    let%bind Tast.Expr.{ ty = inferred_ty; kind; range } = infer_expr t ~term in
    let%bind () = Type.check_subtype t ~range ~sub:inferred_ty ~super:ty in
    return (Tast.Expr.create ~expr:kind ~range ~ty)
  in
  match data with
  | Lit_int _ | Lit_string _ | Bin_op _ | Un_op _ | Lit_bool _ | Lit_char _ | This | Super
  | If { cond = _; if_then = _; if_else = None }
  | If_option _
  | Or_else _
  | De_null _
  | While _
  | Field_subscript _
  | Array_subscript _
  | Array_subrange _
  | New_array _
  | Let _
  | Assign _
  | Function_call _
  | Method_call _
  | Block _
  | Return _
  | New _
  | Evolves _
  | Exchange _
  | Unit -> check_infer ()
  | Ident path ->
    let create_updated_local ~class_id ~ident_class_id ~local ~ty ~new_ty =
      match Type.Class_id.equal class_id ident_class_id with
      | true ->
        update_local_type t ~local ~ty:new_ty;
        return (Tast.Expr.create ~expr:(Tast.Expr.Local local) ~range ~ty)
      | false -> check_infer ()
    in
    (match ty, lookup_local_path t ~path with
     (* If we check against an owned typed [!A] then we can strip the permission from the
       local variable and pass it to the checking hole. *)
     | ( (Owned_object class_id | Option (Owned_object class_id))
       , Some (local, (Owned_object ident_class_id as ty)) ) ->
       create_updated_local
         ~class_id
         ~ident_class_id
         ~local
         ~ty
         ~new_ty:(Object ident_class_id)
     | ( Option (Owned_object class_id)
       , Some (local, (Option (Owned_object ident_class_id) as ty)) ) ->
       create_updated_local
         ~class_id
         ~ident_class_id
         ~local
         ~ty
         ~new_ty:(Option (Object ident_class_id))
     | _ -> check_infer ())
  | If { cond; if_then; if_else = Some if_else } ->
    let%bind checked_cond = check_expr t ~ty:Type.Bool ~term:cond in
    let old_typestate = Typestate.save t in
    let%bind checked_then = check_expr t ~ty ~term:if_then in
    let if_then_typestate = Typestate.save t in
    Typestate.restore t old_typestate;
    let%bind checked_else = check_expr t ~ty ~term:if_else in
    let if_else_typestate = Typestate.save t in
    let%bind () = Typestate.assert_equal t if_else_typestate if_then_typestate ~range in
    return
      (Tast.Expr.create
         ~expr:
           (If { cond = checked_cond; if_then = checked_then; if_else = checked_else })
         ~range
         ~ty)
  | Lit_array elts ->
    (match ty with
     | Array { mut = _; elt = elt_ty } ->
       let%bind checked_elts =
         elts |> List.map_result ~f:(fun term -> check_expr t ~ty:elt_ty ~term)
       in
       return (Tast.Expr.create ~expr:(Lit_array checked_elts) ~range ~ty)
     | invalid_ty ->
       create_type_error_result
         ~range
         ~msg:
           [ Text "Expected an expression of type "
           ; Type.code_str t invalid_ty
           ; Text " but found an array literal."
           ])
  | Null ->
    (match ty with
     | Option _ -> return (Tast.Expr.create ~expr:Null ~range ~ty)
     | invalid_ty ->
       create_type_error_result
         ~range
         ~msg:[ Text "Type "; Type.code_str t invalid_ty; Text " does not contain null" ])

and infer_expr t ~(term : Ast.Expr.t) =
  let Ast.Node.{ range; data } = term in
  match data with
  | Lit_int i -> return (Tast.Expr.create ~expr:(Lit_int i) ~range ~ty:(Type.Numeric Int))
  | Lit_char c ->
    return (Tast.Expr.create ~expr:(Lit_char c) ~range ~ty:(Type.Numeric Char))
  | Lit_bool b -> return (Tast.Expr.create ~expr:(Lit_bool b) ~range ~ty:Type.Bool)
  | Lit_string s ->
    return
      (Tast.Expr.create
         ~expr:(Lit_string s)
         ~range
         ~ty:(Type.Array { mut = false; elt = Type.Numeric Char }))
  | Unit -> return (Tast.Expr.create ~expr:Unit ~range ~ty:Type.Unit)
  | This ->
    let%bind ty = Class.type_this t ~range in
    return (Tast.Expr.create ~expr:This ~range ~ty)
  | Super ->
    let%bind ty = Class.type_super t ~range in
    return (Tast.Expr.create ~expr:Super ~range ~ty)
  (* let%bind ty = Class.type_this t ~range in
    return (Tast.Expr.create ~expr ~range ~ty) *)
  | Ident path ->
    let%bind resolved, ty = lookup_path t ~path ~range in
    return (Tast.Expr.create ~expr:resolved ~range ~ty:(Type.erase_ownership ty))
  | Bin_op { lhs; rhs; op } ->
    let%bind lhs_typed = infer_expr t ~term:lhs in
    let pre_rhs_cs = Typestate.save t in
    let%bind rhs_typed = infer_expr t ~term:rhs in
    let post_rhs_cs = Typestate.save t in
    let lhs_ty = Tast.Expr.ty lhs_typed in
    let rhs_ty = Tast.Expr.ty rhs_typed in
    let%bind result_type =
      match op, (lhs_ty, rhs_ty) with
      | (Add | Sub | Mul | Div), (Numeric lhs_num, Numeric rhs_num)
        when Type.Numeric.equal lhs_num rhs_num -> return lhs_ty
      | (Lt | Gt | Ge | Le), (Numeric lhs_num, Numeric rhs_num)
        when Type.Numeric.equal lhs_num rhs_num -> return Type.Bool
      | (And | Or), (Bool, Bool) ->
        let%bind () = Typestate.assert_equal t ~range pre_rhs_cs post_rhs_cs in
        return Type.Bool
      | (Eq | Ne), _ ->
        let%bind () = Type.check_comparable t ~range ~lhs:lhs_ty ~rhs:rhs_ty in
        return Type.Bool
      | _ ->
        create_type_error_result
          ~range
          ~msg:
            [ Text "Unexpected types "
            ; Type.code_str t lhs_ty
            ; Text " and "
            ; Type.code_str t rhs_ty
            ; Text " as operands to binary operator "
            ; Code_str (Ast.Expr.Bin_op.to_symbol op)
            ]
    in
    return
      (Tast.Expr.create
         ~expr:(Bin_op { lhs = lhs_typed; rhs = rhs_typed; op })
         ~range
         ~ty:result_type)
  | Un_op { op; rhs } ->
    let%bind rhs_typed = infer_expr t ~term:rhs in
    let rhs_ty = Tast.Expr.ty rhs_typed in
    let%bind result_type =
      match op, rhs_ty with
      | Neg, Numeric _ -> return rhs_ty
      | Not, Bool -> return Type.Bool
      | (Neg | Not), _ ->
        create_type_error_result
          ~range
          ~msg:
            [ Text "Unexpected type "
            ; Type.code_str t rhs_ty
            ; Text " as operand to unary operator "
            ; Code_str (Ast.Expr.Un_op.to_symbol op)
            ]
    in
    return (Tast.Expr.create ~expr:(Un_op { op; rhs = rhs_typed }) ~range ~ty:result_type)
  | Block exprs ->
    (* Type checking blocks is kind of weird at the moment based on how I parsed 
     them. 
     - An empty block {} is always Unit.
     - A block with a single expression [{ something }] is really the same as 
       just [something].
     - We have to treat let expressions/statements carefully. A let expression
       without any further expressions following it in the block does not bind
       anything, so [ {let x = f(1)} ] has type Unit and evaluates f(1), but we
       do not need to bind the x
     - If the let *does* proceed other statments then we do want to bind it,
       [{ let x = f(1); x}] has the type of [x] which is the type of [f(1)].
    *)
    let rec aux t (tail : Ast.Expr.t list) typed_exprs_rev =
      match tail with
      | [] ->
        return
          (Tast.Expr.create ~expr:(Block (List.rev typed_exprs_rev)) ~range ~ty:Type.Unit)
      | [ expr ] ->
        let%bind typed_ret_expr = infer_expr t ~term:expr in
        return
          (Tast.Expr.create
             ~expr:(Block (List.rev (typed_ret_expr :: typed_exprs_rev)))
             ~range
             ~ty:(Tast.Expr.ty typed_ret_expr))
      | ({ data; range } as expr) :: tail_exprs ->
        (match data with
         | Let { ident; annot; expr = let_expr } ->
           let%bind checked_let_expr = infer_with_opt_annot t ~term:let_expr ~annot in
           let expr_ty = Tast.Expr.ty checked_let_expr in
           let%bind t, local = with_local t ~range ~ident ~ty:expr_ty in
           let typed_let =
             Tast.Expr.create
               ~expr:(Let { local; expr = checked_let_expr })
               ~range
               ~ty:Type.Unit
           in
           aux t tail_exprs (typed_let :: typed_exprs_rev)
         | _ ->
           let%bind typed_expr = check_expr t ~ty:Type.Unit ~term:expr in
           aux t tail_exprs (typed_expr :: typed_exprs_rev))
    in
    let%bind t = with_new_scope t ~range in
    aux t exprs []
  | If { cond; if_then; if_else = None } ->
    let%bind checked_cond = check_expr t ~ty:Type.Bool ~term:cond in
    let old_typestate = Typestate.save t in
    let%bind checked_then = check_expr t ~ty:Type.Unit ~term:if_then in
    let new_typestate = Typestate.save t in
    let%bind () = Typestate.assert_equal t old_typestate new_typestate ~range in
    let else_unit = Tast.Expr.create ~expr:Unit ~range ~ty:Type.Unit in
    return
      (Tast.Expr.create
         ~expr:(If { cond = checked_cond; if_then = checked_then; if_else = else_unit })
         ~range
         ~ty:Type.Unit)
  | If { cond; if_then; if_else = Some if_else } ->
    let%bind checked_cond = check_expr t ~ty:Type.Bool ~term:cond in
    let old_typestate = Typestate.save t in
    let%bind checked_then = infer_expr t ~term:if_then in
    let if_then_typestate = Typestate.save t in
    Typestate.restore t old_typestate;
    let%bind checked_else = infer_expr t ~term:if_else in
    let if_else_typestate = Typestate.save t in
    let%bind () = Typestate.assert_equal t ~range if_then_typestate if_else_typestate in
    let%bind result_ty =
      (* TODO: Think about removing this and instead requiring all upcasts of this
         nature to be explicit. *)
      Type.join t ~range (Tast.Expr.ty checked_then) (Tast.Expr.ty checked_else)
    in
    return
      (Tast.Expr.create
         ~expr:
           (If { cond = checked_cond; if_then = checked_then; if_else = checked_else })
         ~range
         ~ty:result_ty)
  | While { cond; block } ->
    let%bind checked_cond = check_expr t ~ty:Type.Bool ~term:cond in
    let old_typestate = Typestate.save t in
    let%bind checked_block = check_expr t ~ty:Type.Unit ~term:block in
    let new_typestate = Typestate.save t in
    let%bind () = Typestate.assert_equal t ~range old_typestate new_typestate in
    return
      (Tast.Expr.create
         ~expr:(While { cond = checked_cond; block = checked_block })
         ~range
         ~ty:Type.Unit)
  | Let { ident; annot = _; expr = _ } ->
    create_type_error_result ~range ~msg:[ Text "Identifier is unused "; Ident ident ]
  | Assign { lhs; rhs } ->
    let%bind checked_lhs = infer_lvalue t ~lvalue:lhs in
    let%bind checked_rhs = check_expr t ~ty:(Tast.Expr.ty checked_lhs) ~term:rhs in
    let new_expr =
      Tast.Expr.create
        ~expr:(Assign { lhs = checked_lhs; rhs = checked_rhs })
        ~range
        ~ty:Type.Unit
    in
    check_vtable_update t ~range ~expr:new_expr
  | Array_subscript { expr; index } ->
    check_array_subscript t ~range ~expr ~index ~mode:`Read
  | Array_subrange { expr; from; to_ } ->
    let%bind checked_expr = infer_expr t ~term:expr in
    let%bind array_ty =
      match Tast.Expr.ty checked_expr with
      | Array _ as array_ty -> return array_ty
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot index non-array type "; Type.code_str t invalid_ty ]
    in
    let assert_integral ~expr =
      Result.ok_if_true
        (Type.is_integral (Tast.Expr.ty expr))
        ~error:
          (create_type_error
             ~range:(Tast.Expr.range expr)
             ~msg:
               [ Text "Cannot index array by non-integral type "
               ; Type.code_str t (Tast.Expr.ty expr)
               ])
    in
    let%bind checked_from = infer_expr t ~term:from in
    let%bind checked_to = infer_expr t ~term:to_ in
    let%bind () = assert_integral ~expr:checked_from in
    let%bind () = assert_integral ~expr:checked_to in
    return
      (Tast.Expr.create
         ~expr:
           (Array_subrange { expr = checked_expr; from = checked_from; to_ = checked_to })
         ~range
         ~ty:array_ty)
  | Return expr ->
    let%bind function_check =
      t.function_check
      |> Comp_error.Or_error.of_option
           ~stage:Type_checker
           ~range
           ~msg:[ Text "Cannot return from outside of function" ]
    in
    let%bind checked_expr = check_expr t ~ty:function_check.return_type ~term:expr in
    return (Tast.Expr.create ~expr:(Return checked_expr) ~range ~ty:Type.Bottom)
  | Lit_array [] ->
    create_type_error_result ~range ~msg:[ Text "Cannot infer type of empty array." ]
  | Lit_array (hd :: tl) ->
    let%bind checked_hd = infer_expr t ~term:hd in
    let elt_ty = Tast.Expr.ty checked_hd in
    let%bind checked_tl =
      tl |> List.map_result ~f:(fun term -> check_expr t ~ty:elt_ty ~term)
    in
    return
      (Tast.Expr.create
         ~expr:(Lit_array (checked_hd :: checked_tl))
         ~range
         ~ty:(Type.Array { mut = true; elt = elt_ty }))
  | New_array { size; ty; init } ->
    let%bind checked_ty = check_type t ty in
    let%bind checked_size = infer_expr t ~term:size in
    let%bind () =
      Result.ok_if_true
        (Type.is_integral (Tast.Expr.ty checked_size))
        ~error:
          (create_type_error
             ~range
             ~msg:
               [ Text "Cannot describe array size with non-integral type "
               ; Type.code_str t (Tast.Expr.ty checked_size)
               ])
    in
    let%bind checked_init = check_expr t ~ty:checked_ty ~term:init in
    return
      (Tast.Expr.create
         ~expr:(New_array { size = checked_size; init = checked_init })
         ~range
         ~ty:(Array { mut = true; elt = checked_ty }))
  | Field_subscript { expr; field } ->
    let%bind checked_expr = infer_expr t ~term:expr in
    let%bind ty = infer_field t ~range ~term:checked_expr ~field ~mode:`Read in
    return
      (Tast.Expr.create ~expr:(Field_subscript { expr = checked_expr; field }) ~range ~ty)
  | Function_call { expr = { data = Super; range = _ }; arguments } ->
    (* Super constructor *)
    (* Check that we still have to call the super constructor *)
    let msg = [ Comp_error.Msg_part.Text "Cannot call a super constructor here." ] in
    let%bind function_check =
      Comp_error.Or_error.of_option t.function_check ~stage:Type_checker ~range ~msg
    in
    let%bind constructor_state =
      Comp_error.Or_error.of_option
        function_check.constructor_state
        ~stage:Type_checker
        ~range
        ~msg
    in
    (* Cannot throw since we have already checked we're inside a constructor, 
       which must appear inside a class. *)
    let class_id = function_check.this_id |> Option.value_exn in
    let class_signature = resolve_class t ~class_id in
    (* This can't throw because we know a super constructor is needed. *)
    let super_class_id = class_signature.super |> Option.value_exn in
    let super_class_signature = resolve_class t ~class_id:super_class_id in
    let%bind super_constructor =
      super_class_signature.constructor
      |> Result.of_option
           ~error:(create_type_error ~range ~msg:[ Text "Super class lacks constructor" ])
    in
    let%bind checked_arguments =
      check_argument_list t ~range ~arg_tys:super_constructor.args ~args:arguments
    in
    (* We only check if we need a super after we've checked the arguments, to
       make sure none of them call the super constructor. *)
    let%bind () =
      !constructor_state.needs_super
      |> Result.ok_if_true ~error:(create_type_error ~range ~msg)
    in
    (* Once we've checked everything, we no longer need a super call. *)
    Constructor_state.initialise_super constructor_state;
    let new_expr =
      Tast.Expr.create
        ~expr:(Super_call { super_id = super_class_id; arguments = checked_arguments })
        ~range
        ~ty:Type.Unit
    in
    (* Check if we need to update the vtable *)
    check_vtable_update t ~range ~expr:new_expr
  | Function_call { expr; arguments } ->
    (* Normal function call *)
    let%bind checked_fn = infer_expr t ~term:expr in
    let%bind function_signature =
      match Tast.Expr.ty checked_fn with
      | Fun signature -> Ok signature
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot call non-function type "; Type.code_str t invalid_ty ]
    in
    let%bind checked_arguments =
      check_argument_list t ~range ~arg_tys:function_signature.args ~args:arguments
    in
    return
      (Tast.Expr.create
         ~expr:(Function_call { expr = checked_fn; arguments = checked_arguments })
         ~range
         ~ty:function_signature.ret)
  | Method_call { expr; method_; arguments } ->
    let%bind checked_expr = infer_expr t ~term:expr in
    let%bind class_id =
      match Tast.Expr.ty checked_expr with
      | Object class_id -> return class_id
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot call method on type "; Type.code_str t invalid_ty ]
    in
    let visibility =
      match Ast.Node.data expr with
      | This | Super -> Ast.Decl.Visibility.Private
      | _ -> Ast.Decl.Visibility.Public
    in
    let%bind method_signature =
      Class.find_method t ~class_id ~name:method_ ~visibility ()
      |> Comp_error.Or_error.of_option
           ~stage:Type_checker
           ~range
           ~msg:
             [ Text "Method "
             ; Ident method_
             ; Text " not present on class "
             ; checked_expr |> Tast.Expr.ty |> Type.code_str t
             ]
    in
    let%bind checked_arguments =
      check_argument_list
        t
        ~range
        ~arg_tys:method_signature.function_.args
        ~args:arguments
    in
    let expr =
      match Ast.Node.data expr with
      | Super ->
        Tast.Expr.Super_method_call
          { super_id = class_id; method_; arguments = checked_arguments }
      | _ ->
        Tast.Expr.Method_call
          { expr = checked_expr; method_; arguments = checked_arguments }
    in
    return (Tast.Expr.create ~expr ~range ~ty:method_signature.function_.ret)
  | New { class_; arguments } ->
    let%bind class_id = lookup_class t ~path:class_ ~range in
    let class_signature = resolve_class t ~class_id in
    let%bind constructor =
      class_signature.constructor
      |> Result.of_option
           ~error:
             (create_type_error
                ~range
                ~msg:
                  [ Text "Cannot construct instance of class without constructor "
                  ; Path class_
                  ])
    in
    let%bind checked_arguments =
      check_argument_list t ~range ~arg_tys:constructor.args ~args:arguments
    in
    let ret_ty =
      match t.mode with
      | With_ownership -> Type.Owned_object class_id
      | Without -> Type.Object class_id
    in
    return
      (Tast.Expr.create
         ~expr:(New { class_id; arguments = checked_arguments })
         ~range
         ~ty:ret_ty)
  | Evolves { expr; class_; arguments } ->
    (* TODO implement*)
    let%bind class_id = lookup_class t ~path:class_ ~range in
    let class_name = Class_env.find_name t.class_env ~id:class_id in
    let class_signature = resolve_class t ~class_id in
    let%bind evolver =
      class_signature.evolver
      |> Comp_error.Or_error.of_option
           ~stage:Type_checker
           ~range
           ~msg:
             [ Text "Class "
             ; Path class_name
             ; Text " does not have an evolves constructor"
             ]
    in
    (* Cannot fail since in order to have an evolves constructor, this must be
       a subclass. *)
    let super_id = class_signature.super |> Option.value_exn in
    let expected_expr_ty =
      match t.mode with
      | With_ownership -> Type.Owned_object super_id
      | Without -> Type.Object super_id
    in
    let%bind checked_expr = check_expr t ~ty:expected_expr_ty ~term:expr in
    let%bind checked_arguments =
      check_argument_list t ~range ~arg_tys:evolver.args ~args:arguments
    in
    let ret_ty =
      match t.mode with
      | With_ownership -> Type.Owned_object class_id
      | Without -> Type.Option (Type.Object class_id)
    in
    return
      (Tast.Expr.create
         ~expr:(Evolves { expr = checked_expr; class_id; arguments = checked_arguments })
         ~range
         ~ty:ret_ty)
  | Null -> create_type_error_result ~range ~msg:[ Text "Cannot infer type of null" ]
  | De_null { lhs } ->
    let%bind checked_expr = infer_expr t ~term:lhs in
    let%bind ty =
      match Tast.Expr.ty checked_expr with
      | Option ty -> return ty
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot remove optional part of type "; Type.code_str t invalid_ty ]
    in
    return (Tast.Expr.create ~expr:(De_null { lhs = checked_expr }) ~range ~ty)
  | Or_else { lhs; or_else } ->
    let%bind checked_lhs = infer_expr t ~term:lhs in
    let%bind ty =
      match Tast.Expr.ty checked_lhs with
      | Option ty -> return ty
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot check non-optional type "; Type.code_str t invalid_ty ]
    in
    let pre_or_else_cs = Typestate.save t in
    let%bind checked_or_else = check_expr t ~ty ~term:or_else in
    let post_or_else_cs = Typestate.save t in
    let%bind () = Typestate.assert_equal t ~range pre_or_else_cs post_or_else_cs in
    return
      (Tast.Expr.create
         ~expr:(Or_else { lhs = checked_lhs; or_else = checked_or_else })
         ~range
         ~ty)
  | If_option { expr; if_value; if_null; var; annot } ->
    (* Convert the annotation to the optional version *)
    let%bind checked_expr =
      match annot with
      | None -> infer_expr t ~term:expr
      | Some ast_ty ->
        let%bind ty = check_type t ast_ty in
        check_expr t ~ty:(Option ty) ~term:expr
    in
    let%bind ty =
      match Tast.Expr.ty checked_expr with
      | Option ty -> return ty
      | invalid_ty ->
        create_type_error_result
          ~range
          ~msg:[ Text "Cannot check non-optional type "; Type.code_str t invalid_ty ]
    in
    let%bind t_with_scope = with_new_scope t ~range in
    let%bind t_with_var, resolved_var = with_local t_with_scope ~range ~ident:var ~ty in
    let pre_if_cs = Typestate.save t in
    (match if_null with
     | None ->
       let%bind checked_if_value = check_expr t_with_var ~term:if_value ~ty:Unit in
       let post_if_cs = Typestate.save t in
       let%bind () = Typestate.assert_equal t ~range pre_if_cs post_if_cs in
       let if_null = Tast.Expr.create ~expr:Unit ~ty:Unit ~range in
       return
         (Tast.Expr.create
            ~expr:
              (If_option
                 { expr = checked_expr
                 ; if_value = checked_if_value
                 ; if_null
                 ; var = resolved_var
                 })
            ~range
            ~ty:Unit)
     | Some if_null ->
       let%bind checked_if_value = infer_expr t_with_var ~term:if_value in
       let post_if_value_cs = Typestate.save t in
       Typestate.restore t pre_if_cs;
       let%bind checked_if_null = infer_expr t ~term:if_null in
       let post_if_null_cs = Typestate.save t in
       let%bind () = Typestate.assert_equal t ~range post_if_value_cs post_if_null_cs in
       let%bind ty =
         Type.join t ~range (Tast.Expr.ty checked_if_value) (Tast.Expr.ty checked_if_null)
       in
       return
         (Tast.Expr.create
            ~expr:
              (If_option
                 { expr = checked_expr
                 ; if_value = checked_if_value
                 ; if_null = checked_if_null
                 ; var = resolved_var
                 })
            ~range
            ~ty))
  | Exchange { expr1; expr2 } ->
    let%bind () =
      match t.mode with
      | With_ownership -> Ok ()
      | Without ->
        create_type_error_result
          ~range
          ~msg:[ Text "The exchange operator is exclusive to the ownership mode." ]
    in
    let%bind checked_lhs = infer_lvalue t ~lvalue:expr1 in
    let%bind checked_rhs = infer_lvalue t ~lvalue:expr2 in
    let lhs_ty = Tast.Expr.ty checked_lhs in
    let rhs_ty = Tast.Expr.ty checked_rhs in
    let%bind () =
      Type.equal lhs_ty rhs_ty
      |> Result.ok_if_true
           ~error:
             (create_type_error
                ~range
                ~msg:
                  [ Text "Cannot exchange types "
                  ; Type.code_str t lhs_ty
                  ; Text " and "
                  ; Type.code_str t rhs_ty
                  ])
    in
    return
      (Tast.Expr.create
         ~range
         ~ty:Unit
         ~expr:(Exchange { lhs = checked_lhs; rhs = checked_rhs }))

and check_argument_list t ~range ~arg_tys ~args =
  match
    List.map2 arg_tys args ~f:(fun arg_ty arg -> check_expr t ~ty:arg_ty ~term:arg)
  with
  | Ok result -> Result.all result
  | Unequal_lengths ->
    create_type_error_result
      ~range
      ~msg:
        [ Text
            [%string
              "Expected %{List.length arg_tys#Int} arguments but found %{List.length \
               args#Int}"]
        ]

and check_array_subscript t ~range ~expr ~index ~mode =
  let%bind checked_expr = infer_expr t ~term:expr in
  let%bind elt_ty =
    match Tast.Expr.ty checked_expr with
    | Array { mut; elt } as array_ty ->
      (match mode, mut with
       | `Read, _ -> return (Type.erase_ownership elt)
       | `Write, true -> return elt
       | `Write, false ->
         create_type_error_result
           ~range
           ~msg:[ Text "Cannot write to immutable array "; Type.code_str t array_ty ])
    | invalid_ty ->
      create_type_error_result
        ~range
        ~msg:[ Text "Cannot index non-array type "; Type.code_str t invalid_ty ]
  in
  let%bind checked_index = infer_expr t ~term:index in
  let%bind () =
    Result.ok_if_true
      (Type.is_integral (Tast.Expr.ty checked_index))
      ~error:
        (create_type_error
           ~range
           ~msg:
             [ Text "Cannot index array by non-integral type "
             ; Type.code_str t (Tast.Expr.ty checked_index)
             ])
  in
  return
    (Tast.Expr.create
       ~expr:(Array_subscript { expr = checked_expr; index = checked_index })
       ~range
       ~ty:elt_ty)

and infer_with_opt_annot t ~(term : Ast.Expr.t) ~(annot : Ast.Type.t option) =
  match annot with
  | None -> infer_expr t ~term
  | Some ast_ty ->
    let%bind ty = check_type t ast_ty in
    check_expr t ~ty ~term

and infer_lvalue t ~(lvalue : Ast.Expr.t) =
  let Ast.Node.{ range; data } = lvalue in
  match data with
  | Ident path ->
    let%bind resolved, ty = lookup_path t ~path ~range in
    return (Tast.Expr.create ~expr:resolved ~range ~ty)
  | Array_subscript { expr; index } ->
    check_array_subscript t ~range ~expr ~index ~mode:`Write
  | Field_subscript { expr; field } ->
    (* Unlike when we infer the access of a subscript, we know that writing to
       a subscript requires that the expr is runtime representable. 
       
       We also don't need to do the module type check thing to check things are
       runtime representable. *)
    let%bind checked_expr =
      (* TODO: this is pretty yucky as a way of handling writing to this in a constructor. *)
      match Ast.Node.data expr, is_in_constructor t with
      | This, true ->
        let class_id = Option.value_exn (Option.value_exn t.function_check).this_id in
        Ok
          (Tast.Expr.create ~expr:This ~range:(Ast.Node.range expr) ~ty:(Object class_id))
      | _ -> infer_expr t ~term:expr
    in
    let%bind field_ty = infer_field t ~range ~field ~term:checked_expr ~mode:`Write in
    Tast.Expr.create
      ~expr:(Field_subscript { expr = checked_expr; field })
      ~range
      ~ty:field_ty
    |> return
  | Unit
  | Lit_int _
  | Lit_string _
  | Lit_bool _
  | This
  | Super
  | Bin_op _
  | Un_op _
  | If _
  | While _
  | Block _
  | Let _
  | Assign _
  | Return _
  | Lit_char _
  | Lit_array _
  | Method_call _
  | New _
  | Evolves _
  | Null
  | If_option _
  | De_null _
  | Or_else _
  | Array_subrange _
  | New_array _
  | Exchange _
  | Function_call _ ->
    create_type_error_result ~range ~msg:[ Text "Cannot assign to expression" ]

and infer_field t ~range ~(term : Tast.Expr.t) ~field ~mode =
  match Tast.Expr.ty term with
  | Object class_id ->
    let expr = Tast.Expr.kind term in
    let visibility =
      match expr with
      | This | Super -> Ast.Decl.Visibility.Private
      | _ -> Ast.Decl.Visibility.Public
    in
    let class_path = Class_env.find_name t.class_env ~id:class_id in
    let%bind class_field =
      Class.find_field t ~class_id ~name:field ~visibility ()
      |> Comp_error.Or_error.of_option
           ~stage:Type_checker
           ~range
           ~msg:
             [ Text "Unknown reference to field "
             ; Ident field
             ; Text " on class "
             ; Path class_path
             ]
    in
    let%bind () =
      match mode with
      | `Read -> Ok ()
      | `Write ->
        let can_initialise =
          match expr with
          | This ->
            (* Since we have type checked a 'this', we are inside a function. *)
            let function_check = t.function_check |> Option.value_exn in
            (match function_check.constructor_state with
             | None -> false
             | Some constructor_state ->
               Constructor_state.try_initialise_field constructor_state ~field)
          | _ -> false
        in
        Result.ok_if_true
          (can_initialise || class_field.mut)
          ~error:
            (create_type_error
               ~range
               ~msg:
                 [ Text "Attempted to modify immutable field "
                 ; Ident field
                 ; Text " of class "
                 ; Path class_path
                 ])
    in
    let ty =
      match mode with
      | `Read -> Type.erase_ownership class_field.ty
      | `Write -> class_field.ty
    in
    Ok ty
  | _ ->
    create_type_error_result
      ~range
      ~msg:
        [ Text "Type "
        ; Type.code_str t (Tast.Expr.ty term)
        ; Text " does not support field access."
        ]
;;

let transpose_result_option = function
  | None -> Ok None
  | Some (Ok s) -> Ok (Some s)
  | Some (Error e) -> Error e
;;

let true_or_check v ~f =
  match v with
  | true -> Ok ()
  | false -> f ()
;;

module Scope = struct
  type t = Ast.Ident.t list
end

module Precheck_decl = struct
  module Function = struct
    type t =
      { id : Global_id.t
      ; args : Ast.Decl.Function.Args.t
      ; ret_type : Ast.Type.t
      ; body : Ast.Expr.t
      ; range : Range.t
      ; scope : Scope.t
      }
  end

  module Extern_function = struct
    type t =
      { id : Global_id.t
      ; ret_type : Ast.Type.t
      ; range : Range.t
      ; arg_tys : Ast.Type.t list
      ; external_name : string
      ; scope : Scope.t
      }
    [@@disable_unused_warnings]
  end

  module Class = struct
    type t =
      { id : Type.Class_id.t
      ; super_type : Ast.Path.t option
      ; fields : Ast.Decl.Class.Field.t Ast.Node.t list
      ; constructors : Ast.Decl.Class.Constructor.t Ast.Node.t list
      ; methods : Ast.Decl.Class.Method.t Ast.Node.t list
      ; range : Range.t
      ; scope : Scope.t
      }
  end

  module Constant = struct
    type t =
      { id : Global_id.t
      ; annot : Ast.Type.t option
      ; value : Ast.Expr.t
      ; range : Range.t
      ; scope : Scope.t
      }
  end

  type t =
    { mutable classes : Class.t list
    ; mutable functions : Function.t list
    ; mutable extern_functions : Extern_function.t list
    ; mutable constants : Constant.t list
    }

  let create () = { classes = []; functions = []; constants = []; extern_functions = [] }

  let register_decls t ~decls =
    let rec aux t (precheck : t) ~(decl : Ast.Decl.t) =
      let Ast.Node.{ range; data } = decl in
      let insert_global name =
        match Global_env.insert t.global_env ~scope:t.scope ~name with
        | `Already_defined ->
          create_type_error_result
            ~range
            ~msg:[ Text "Identifier "; Ident name; Text " multiply defined" ]
        | `Ok id -> Ok id
      in
      let insert_class name =
        match Class_env.insert t.class_env ~scope:t.scope ~name with
        | `Already_defined ->
          create_type_error_result
            ~range
            ~msg:[ Text "Class "; Ident name; Text " multiply defined" ]
        | `Ok id -> Ok id
      in
      match data with
      | Function { name; args; ret_type; body } ->
        let%map id = insert_global name in
        precheck.functions
        <- Function.{ id; args; ret_type; body; range; scope = t.scope }
           :: precheck.functions
      | Constant { ident; annot; value } ->
        let%map id = insert_global ident in
        precheck.constants
        <- Constant.{ id; annot; value; range; scope = t.scope } :: precheck.constants
      | Class { name; super_type; fields; constructors; methods } ->
        let%map id = insert_class name in
        precheck.classes
        <- Class.{ id; super_type; fields; constructors; methods; range; scope = t.scope }
           :: precheck.classes
      | Module { name; decls } ->
        let new_t = with_new_module_scope t ~name in
        List.fold_result decls ~init:() ~f:(fun () decl -> aux new_t precheck ~decl)
      | Extern_function { name; arg_tys; ret_type; external_name } ->
        let%map id = insert_global name in
        precheck.extern_functions
        <- Extern_function.
             { id; arg_tys; ret_type; range; external_name; scope = t.scope }
           :: precheck.extern_functions
    in
    let precheck_decls = create () in
    let%map () =
      List.fold_result decls ~init:() ~f:(fun () decl -> aux t precheck_decls ~decl)
    in
    (* We reverse the lists to ensure that everything gets checked in the order 
     it's defined in the syntax tree *)
    ( List.rev precheck_decls.classes
    , List.rev precheck_decls.constants
    , List.rev precheck_decls.functions
    , List.rev precheck_decls.extern_functions )
  ;;
end

(** The type for a function after it has its signature computed, but has not has
    its body checked. *)
module Declared_function = struct
  module Args = struct
    type t = (Ast.Ident.t * Type.t) list

    let check t args =
      List.map_result args ~f:(fun (name, ast_ty) ->
        let%map ty = check_type t ast_ty in
        name, ty)
    ;;

    let types args = List.map args ~f:snd
  end

  type t =
    { id : Global_id.t
    ; args : Args.t
    ; ret_type : Type.t
    ; body : Ast.Expr.t
    ; range : Range.t
    ; scope : Scope.t
    }

  let fun_type_of ~args ~ret_type = Type.{ args = Args.types args; ret = ret_type }

  let check_function_signature
        t
        Precheck_decl.Function.{ id; args; ret_type; body; range; scope }
    =
    let t = with_module_scope t ~scope in
    let%bind args = Args.check t args in
    let%bind ret_type = check_type t ret_type in
    Ok ({ id; args; ret_type; body; range; scope }, fun_type_of ~args ~ret_type)
  ;;

  let declare_function t f =
    let%bind declared_function, ty = check_function_signature t f in
    Hashtbl.add_exn t.global_types ~key:f.id ~data:(Type.Fun ty);
    Ok declared_function
  ;;
end

module Declared_class = struct
  module Field = struct
    type t = Tast.Decl.Class.Field.t

    let check t field =
      let Ast.Node.{ range; data } = field in
      let Ast.Decl.Class.Field.{ name; ty; visibility; overrides; mut } = data in
      let%map checked_ty = check_type t ty in
      let signature =
        name, ({ ty = checked_ty; mut; visibility; overrides } : Type.Class.Field.t)
      in
      let declared : t = { name; ty = checked_ty; visibility; overrides; mut; range } in
      declared, signature
    ;;
  end

  module Constructor = struct
    type t =
      { args : Declared_function.Args.t
      ; evolves : Type.Class_id.t option
      ; body : Ast.Expr.t
      }

    let check t constructor =
      let Ast.Node.{ range; data } = constructor in
      let Ast.Decl.Class.Constructor.{ args; evolves; body } = data in
      let%bind checked_args = Declared_function.Args.check t args in
      let%bind checked_evolves =
        Option.map evolves ~f:(fun path -> lookup_class t ~range ~path)
        |> transpose_result_option
      in
      let declared =
        Ast.Node.create
          ~data:{ args = checked_args; evolves = checked_evolves; body }
          ~loc:range
      in
      let signature =
        Type.Class.Constructor.{ args = Declared_function.Args.types checked_args }
      in
      Ok (declared, signature)
    ;;
  end

  module Method = struct
    type t =
      { visibility : Ast.Decl.Visibility.t
      ; name : Ast.Ident.t
      ; args : Declared_function.Args.t
      ; ret_type : Type.t
      ; body : Ast.Expr.t
      ; overrides : bool
      }

    let check t method_ =
      let Ast.Node.{ range; data } = method_ in
      let Ast.Decl.Class.Method.
            { visibility; function_ = { name; args; ret_type; body }; overrides }
        =
        data
      in
      let%bind checked_args = Declared_function.Args.check t args in
      let%bind checked_ret_type = check_type t ret_type in
      let signature =
        ( name
        , ({ visibility
           ; overrides
           ; function_ =
               Type.
                 { args = Declared_function.Args.types checked_args
                 ; ret = checked_ret_type
                 }
           }
           : Type.Class.Method.t) )
      in
      let declared =
        Ast.Node.create
          ~loc:range
          ~data:
            { visibility
            ; name
            ; args = checked_args
            ; ret_type = checked_ret_type
            ; body
            ; overrides
            }
      in
      Ok (declared, signature)
    ;;
  end

  type t =
    { id : Type.Class_id.t
    ; super_type : Type.Class_id.t option
    ; fields : Field.t list
    ; constructor : Constructor.t Ast.Node.t option
    ; evolver : Constructor.t Ast.Node.t option
    ; methods : Method.t Ast.Node.t list
    ; range : Range.t
    ; scope : Scope.t
    }

  let check_class_signature
        t
        Precheck_decl.Class.
          { id; super_type; fields; constructors; methods; range; scope }
    =
    let t = with_module_scope t ~scope in
    let%bind super_type_resolved =
      Option.map super_type ~f:(fun path -> lookup_class t ~range ~path)
      |> transpose_result_option
    in
    let%bind checked_fields, field_sigs =
      List.map_result fields ~f:(Field.check t) |> Result.map ~f:List.unzip
    in
    let%bind field_sig_map =
      match Ast.Ident.Map.of_alist field_sigs with
      | `Ok s -> Ok s
      | `Duplicate_key name ->
        create_type_error_result
          ~range
          ~msg:[ Text "Field declared multiple times "; Ident name ]
    in
    let ensure_unique ~evolves_filter ~constructor_type =
      match
        List.filter constructors ~f:(fun constructor ->
          constructor.data.evolves |> evolves_filter)
      with
      | [ constructor ] -> Ok (Some constructor)
      | [] -> Ok None
      | _ :: _ :: _ ->
        create_type_error_result
          ~range
          ~msg:[ Text ("Class has multiple " ^ constructor_type ^ ".") ]
    in
    let%bind constructor =
      ensure_unique ~evolves_filter:Option.is_none ~constructor_type:"constructors"
    in
    let%bind evolver =
      ensure_unique ~evolves_filter:Option.is_some ~constructor_type:"evolvers"
    in
    let check_opt_constructor t = function
      | Some constructor ->
        let%bind checked, sig_ = Constructor.check t constructor in
        return (Some checked, Some sig_)
      | None -> return (None, None)
    in
    let%bind checked_constructor, constructor_sig = check_opt_constructor t constructor in
    let%bind checked_evolver, evolver_sig = check_opt_constructor t evolver in
    let%bind checked_methods, method_sigs =
      List.map_result methods ~f:(Method.check t) |> Result.map ~f:List.unzip
    in
    let%bind method_sig_map =
      match Ast.Ident.Map.of_alist method_sigs with
      | `Ok s -> Ok s
      | `Duplicate_key name ->
        create_type_error_result
          ~range
          ~msg:[ Text "Field declared multiple times "; Ident name ]
    in
    let signature =
      Type.Class.
        { id
        ; fields = field_sig_map
        ; constructor = constructor_sig
        ; evolver = evolver_sig
        ; methods = method_sig_map
        ; super = super_type_resolved
        }
    in
    let declaration =
      { id
      ; super_type = super_type_resolved
      ; fields = checked_fields
      ; constructor = checked_constructor
      ; evolver = checked_evolver
      ; methods = checked_methods
      ; range
      ; scope
      }
    in
    Ok (declaration, signature)
  ;;

  let declare_class t c =
    let%bind declaration, signature = check_class_signature t c in
    Hashtbl.add_exn t.class_table ~key:declaration.id ~data:signature;
    Ok declaration
  ;;
end

let check_constant t Precheck_decl.Constant.{ id; annot; value; range; scope } =
  let t = with_module_scope t ~scope in
  (* TODO: We are checking constants before adding their types into the 
     environment, so we will run in to issues if constants refer to eachother in
     an order other than in the order defined. If the don't do this there will
     be an exception when we look up the global id. *)
  let%bind checked_value = infer_with_opt_annot t ~term:value ~annot in
  Ok Tast.Decl.Const.{ id; expr = checked_value; range }
;;

let declare_constant t constant =
  let%map checked_constant = check_constant t constant in
  Hashtbl.add_exn
    t.global_types
    ~key:checked_constant.id
    ~data:(Tast.Expr.ty checked_constant.expr);
  checked_constant
;;

let declare_extern_function
      t
      Precheck_decl.Extern_function.{ id; arg_tys; ret_type; range; external_name; scope }
  =
  let t = with_module_scope t ~scope in
  let%bind checked_arg_types =
    List.map_result arg_tys ~f:(fun arg_ty -> check_type t arg_ty)
  in
  let%bind checked_ret_type = check_type t ret_type in
  let fun_type = Type.{ args = checked_arg_types; ret = checked_ret_type } in
  Hashtbl.add_exn t.global_types ~key:id ~data:(Type.Fun fun_type);
  Ok
    Tast.Decl.Extern_function.
      { id
      ; arg_tys = checked_arg_types
      ; ret_type = checked_ret_type
      ; range
      ; external_name
      }
;;

let declare_args t ~range args =
  let%map new_t, declared_args =
    List.fold_result args ~init:(t, []) ~f:(fun (t, args) (ident, ty) ->
      let%map new_t, declared_arg = with_local t ~range ~ident ~ty in
      new_t, (declared_arg, ty) :: args)
  in
  new_t, List.rev declared_args
;;

let check_function_body t ~args ~ret_type ~body ~range ?this_id ?constructor_state () =
  let function_check =
    Function_check.create ~return_type:ret_type ?this_id ?constructor_state ()
  in
  let t = { t with function_check = Some function_check } in
  let%bind t = with_new_scope t ~range in
  let%bind t, checked_args = declare_args t ~range args in
  let%bind checked_body = check_expr t ~ty:ret_type ~term:body in
  let%bind checked_body_with_vtable =
    match constructor_state with
    | Some _ -> check_vtable_update t ~range ~expr:checked_body
    | None -> Ok checked_body
  in
  Ok (checked_body_with_vtable, checked_args)
;;

let check_function t Declared_function.{ id; args; ret_type; body; range; scope } =
  let t = with_module_scope t ~scope in
  let%bind checked_body, checked_args =
    check_function_body t ~args ~ret_type ~body ~range ()
  in
  Ok Tast.Decl.Function.{ id; args = checked_args; ret_type; body = checked_body; range }
;;

let check_field
      t
      ~(class_signature : Type.Class.t)
      ~class_path
      Tast.Decl.Class.Field.{ name; ty; overrides; visibility = _; mut = _; range }
  =
  (* If a field is marked as override we have to check that there is a 
     corresponding field to override and that it is not marked as [mut] *)
  let%bind () =
    true_or_check (not overrides) ~f:(fun () ->
      let%bind super_class =
        class_signature.super
        |> Comp_error.Or_error.of_option
             ~stage:Type_checker
             ~range
             ~msg:
               [ Text "Field marked as override but "
               ; Path class_path
               ; Text " has no super class"
               ]
      in
      let%bind super_field =
        Class.find_field t ~name ~class_id:super_class ~visibility:Private ()
        |> Comp_error.Or_error.of_option
             ~stage:Type_checker
             ~range
             ~msg:
               [ Text "Override field "
               ; Ident name
               ; Text " not found in any super class"
               ]
      in
      let%bind () = Type.check_subtype t ~range ~sub:ty ~super:super_field.ty in
      Result.ok_if_true
        (not super_field.mut)
        ~error:(create_type_error ~range ~msg:[ Text "Cannot override mutable field." ]))
  in
  return ()
;;

let check_constructor t ~(class_signature : Type.Class.t) Ast.Node.{ range; data } =
  let { args; evolves; body } : Declared_class.Constructor.t = data in
  let%bind needs_super =
    match class_signature.super, evolves with
    | Some super_id, Some evolves_id when Type.Class_id.equal super_id evolves_id ->
      Ok false
    | Some _, None -> Ok true
    | None, None -> Ok false
    | _, Some evolves_id ->
      create_type_error_result
        ~range
        ~msg:
          [ Text "Evolves class annotation "
          ; Type.code_str t (Object evolves_id)
          ; Text " does not match super class."
          ]
  in
  let constructor_state = Constructor_state.create ~class_signature ~needs_super in
  let%bind checked_body, checked_args =
    check_function_body
      t
      ~args
      ~ret_type:Type.Unit
      ~body
      ~range
      ~this_id:class_signature.id
      ~constructor_state
      ()
  in
  let%bind () =
    Result.ok_if_true
      (Class.constructor_is_complete (Some constructor_state))
      ~error:
        (create_type_error
           ~range
           ~msg:
             [ Text "Constructor does not fully initalise all fields/super classes: "
             ; Text
                 (Sexp.to_string_hum [%sexp (!constructor_state : Constructor_state.t)])
             ])
  in
  return Tast.Decl.Class.Constructor.{ args = checked_args; body = checked_body; range }
;;

let check_method t ~(class_signature : Type.Class.t) ~class_path Ast.Node.{ range; data } =
  let Declared_class.Method.{ visibility; name; args; ret_type; body; overrides } =
    data
  in
  let%bind () =
    true_or_check (not overrides) ~f:(fun () ->
      (* Copied from the field version but with distinct error messages *)
      let%bind super_class =
        class_signature.super
        |> Comp_error.Or_error.of_option
             ~stage:Type_checker
             ~range
             ~msg:
               [ Text "Method marked as override but "
               ; Path class_path
               ; Text " has no super class"
               ]
      in
      let%bind super_method =
        Class.find_method t ~name ~class_id:super_class ~visibility:Private ()
        |> Comp_error.Or_error.of_option
             ~stage:Type_checker
             ~range
             ~msg:
               [ Text "Override method "
               ; Ident name
               ; Text " not found in any super class"
               ]
      in
      let super_function = super_method.function_ in
      let%bind () =
        Type.check_subtype
          t
          ~range
          ~sub:(Fun (Declared_function.fun_type_of ~args ~ret_type))
          ~super:(Fun super_function)
      in
      Ok ())
  in
  let%bind checked_body, checked_args =
    check_function_body t ~range ~args ~body ~ret_type ~this_id:class_signature.id ()
  in
  return
    Tast.Decl.Class.Method.
      { visibility
      ; name
      ; args = checked_args
      ; ret_type
      ; body = checked_body
      ; overrides
      ; range
      }
;;

let check_class
      t
      Declared_class.
        { id; super_type; fields; constructor; methods; evolver; range; scope }
  =
  let t = with_module_scope t ~scope in
  let class_signature = resolve_class t ~class_id:id in
  let class_path = Class_env.find_name t.class_env ~id in
  (* Check the inheritance graph is acyclic. 
  
     TODO: Would be cool to do something where you can only look up 
     fields/methods if you carry around a witness that the inheritance of the 
     class is acyclic. *)
  let%bind () =
    Result.ok_if_true
      (Class.is_inheritance_acyclic t class_signature)
      ~error:(create_type_error ~range ~msg:[ Text "Class has recursive super class." ])
  in
  let%bind () = List.iter_result fields ~f:(check_field t ~class_signature ~class_path) in
  let%bind checked_class_constructor =
    constructor
    |> Option.map ~f:(check_constructor t ~class_signature)
    |> transpose_result_option
  in
  let%bind checked_class_evolver =
    evolver
    |> Option.map ~f:(check_constructor t ~class_signature)
    |> transpose_result_option
  in
  let%bind checked_class_methods =
    List.map_result methods ~f:(check_method t ~class_signature ~class_path)
  in
  return
    Tast.Decl.Class.
      { id
      ; super_type
      ; fields
      ; constructor = checked_class_constructor
      ; methods = checked_class_methods
      ; evolver = checked_class_evolver
      ; range
      }
;;

let sort_classes classes =
  let seen =
    Hash_set.create
      (module struct
        type t = Type.Class_id.t option [@@deriving hash, compare, sexp_of]
      end)
  in
  Hash_set.add seen None;
  let rec loop remaining sorted =
    match remaining with
    | [] -> sorted
    | _ ->
      let new_sorted, new_remaining =
        List.partition_tf remaining ~f:(fun (class_ : Tast.Decl.Class.t) ->
          match Hash_set.mem seen class_.super_type with
          | true ->
            Hash_set.add seen (Some class_.id);
            true
          | false -> false)
      in
      loop new_remaining (sorted @ new_sorted)
  in
  loop classes []
;;

let check_decls t ~decls =
  let%bind classes, constants, functions, extern_functions =
    Precheck_decl.register_decls t ~decls
  in
  let%bind declared_functions =
    List.map_result functions ~f:(Declared_function.declare_function t)
  in
  let%bind declared_classes =
    List.map_result classes ~f:(Declared_class.declare_class t)
  in
  let%bind declared_extern_functions =
    List.map_result extern_functions ~f:(declare_extern_function t)
  in
  let%bind checked_constants = List.map_result constants ~f:(declare_constant t) in
  let%bind checked_classes =
    List.map_result declared_classes ~f:(check_class t) |> Result.map ~f:sort_classes
  in
  let%bind checked_functions = List.map_result declared_functions ~f:(check_function t) in
  Ok
    Tast.Decls.
      { constants = checked_constants
      ; classes = checked_classes
      ; functions = checked_functions
      ; extern_functions = declared_extern_functions
      }
;;

module For_testing = struct
  module Function_check = struct
    type t = Function_check.t

    let create = Function_check.create ?constructor_state:None
  end

  let with_function_check t fc = { t with function_check = Some fc }

  let to_string t =
    let globals =
      Hashtbl.to_alist t.global_types
      |> List.map ~f:(fun (id, ty) ->
        let path = Global_env.find_name t.global_env ~id in
        [%string "%{path#Ast.Path}: %{Type.to_string t ty}"])
      |> String.concat_lines
    in
    let classes =
      Hashtbl.to_alist t.class_table
      |> List.map ~f:(fun (id, signature) ->
        let path = Class_env.find_name t.class_env ~id in
        [%string
          "%{path#Ast.Path}: %{Type.Class.sexp_of_t signature |> Sexp.to_string_hum}"])
      |> String.concat_lines
    in
    "Globals:\n" ^ globals ^ "\nClasses:\n" ^ classes
  ;;

  module Type = struct
    let to_string = Type.to_string
  end
end
