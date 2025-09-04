open! Core

module Numeric = struct
  type t =
    | Int
    | Char
    | Float
  [@@deriving sexp_of, equal, bin_io]

  let is_integral = function
    | Int | Char -> true
    | Float -> false
  ;;

  let bitwidth = function
    | Int -> 64
    | Char -> 8
    | Float -> 64
  ;;
end

(** TODO: Consider moving this. *)
module Class_id = Unique_id.Int63 ()

module Interface_id = Unique_id.Int63 ()

module Ownership = struct
  type t =
    | Owned
    | Shared
  [@@deriving sexp_of, equal]

  let is_owned = function
    | Owned -> true
    | Shared -> false
  ;;

  let is_shared = function
    | Shared -> true
    | Owned -> false
  ;;
end

module Object_kind = struct
  type t =
    | Class of Class_id.t
    | Interface of Interface_id.t
  [@@deriving sexp_of, equal]
end

type fun_signature =
  { args : t list
  ; ret : t
  }

and t =
  | Bottom
  | Unit
  | Bool
  | Numeric of Numeric.t
  | Array of
      { mut : bool
      ; elt : t
      }
  | Fun of fun_signature (** An object is an instance of a particular class. *)
  | Object of (Ownership.t * Object_kind.t)
  | Top_object
  | Option of t
[@@deriving sexp_of, equal]

let is_reference = function
  | Option _ | Bottom | Unit | Bool | Numeric _ -> false
  | Array _ | Fun _ | Object _ | Top_object -> true
;;

let erase_ownership = function
  | Object (Owned, obj) -> Object (Shared, obj)
  | Option (Object (Owned, obj)) -> Option (Object (Shared, obj))
  | _ as ty -> ty
;;

let promote_ownership = function
  | Object (Shared, obj) -> Object (Owned, obj)
  | Option (Object (Shared, obj)) -> Option (Object (Owned, obj))
  | _ ->
    failwith "cannot promote ownership of non shared object type. this is a compiler bug"
;;

module Class = struct
  type ty = t [@@deriving sexp_of]

  module Field = struct
    type t =
      { ty : ty
      ; mut : bool
      ; overrides : bool
      ; evolves : bool
      ; visibility : Ast.Decl.Visibility.t
      }
    [@@deriving sexp_of, fields ~getters]

    let overrides t = t.overrides
    let evolves t = t.evolves
  end

  module Constructor = struct
    type t = { args : ty list } [@@deriving sexp_of]
  end

  module Method = struct
    type t =
      { visibility : Ast.Decl.Visibility.t
      ; function_ : fun_signature
      ; overrides : bool
      ; receiver_evolves : bool
      }
    [@@deriving sexp_of, fields ~getters]

    let overrides t = t.overrides
  end

  type t =
    { id : Class_id.t
    ; fields : Field.t Ast.Ident.Map.t
    ; constructor : Constructor.t option [@sexp.option]
    ; evolver : Constructor.t option [@sexp.option]
    ; methods : Method.t Ast.Ident.Map.t
    ; super : Class_id.t option
    ; implements : Interface_id.t list
    }
  [@@deriving sexp_of]
end

module Interface = struct
  module Method_signature = struct
    type t =
      { function_signature : fun_signature
      ; receiver_evolves : bool
      }
    [@@deriving sexp_of]
  end

  type t =
    { id : Interface_id.t
    ; implements : Interface_id.t list
    ; method_signatures : Method_signature.t Ast.Ident.Map.t
    }
  [@@deriving sexp_of]
end

let rec to_string ~resolve_object_kind_name = function
  | Bottom -> "noreturn"
  | Numeric Int -> "int"
  | Numeric Char -> "char"
  | Numeric Float -> "float"
  | Unit -> "unit"
  | Bool -> "bool"
  | Array { mut = true; elt } ->
    [%string "[]mut %{to_string ~resolve_object_kind_name elt}"]
  | Array { mut = false; elt } -> [%string "[]%{to_string ~resolve_object_kind_name elt}"]
  | Object (Shared, obj_kind) -> resolve_object_kind_name obj_kind
  | Object (Owned, obj_kind) -> "!" ^ resolve_object_kind_name obj_kind
  | Top_object -> "object"
  | Fun { args; ret } ->
    "("
    ^ (List.map args ~f:(to_string ~resolve_object_kind_name) |> String.concat ~sep:", ")
    ^ ") -> "
    ^ to_string ~resolve_object_kind_name ret
  | Option ty -> "?" ^ to_string ~resolve_object_kind_name ty
;;
