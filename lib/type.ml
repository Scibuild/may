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
  | Object of Class_id.t
  | Top_object
  | Option of t
[@@deriving sexp_of, equal]

let is_reference = function
  | Bottom -> false
  | Unit -> false
  | Bool -> false
  | Numeric _ -> false
  | Array _ -> false
  | Fun _ -> true
  | Object _ -> true
  | Top_object -> true
  | Option _ -> false
;;

let class_id_exn = function
  | Object class_id -> class_id
  | ty -> raise_s [%message "attempted to get class id of non object type" (ty : t)]
;;

module Class = struct
  type ty = t [@@deriving sexp_of]

  module Field = struct
    type t =
      { ty : ty
      ; mut : bool
      ; overrides : bool
      ; visibility : Ast.Decl.Visibility.t
      }
    [@@deriving sexp_of, fields ~getters]

    let overrides t = t.overrides
  end

  module Constructor = struct
    type t = { args : ty list } [@@deriving sexp_of]
  end

  module Method = struct
    type t =
      { visibility : Ast.Decl.Visibility.t
      ; function_ : fun_signature
      ; overrides : bool
      }
    [@@deriving sexp_of, fields ~getters]

    let overrides t = t.overrides
  end

  type t =
    { id : Class_id.t
    ; fields : Field.t Ast.Ident.Map.t
    ; constructor : Constructor.t
    ; evolver : Constructor.t option [@sexp.option]
    ; methods : Method.t Ast.Ident.Map.t
    ; super : Class_id.t option
    }
  [@@deriving sexp_of]
end

let rec to_string ~resolve_class_name = function
  | Bottom -> "noreturn"
  | Numeric Int -> "int"
  | Numeric Char -> "char"
  | Numeric Float -> "float"
  | Unit -> "unit"
  | Bool -> "bool"
  | Array { mut = true; elt } -> [%string "[]mut %{to_string ~resolve_class_name elt}"]
  | Array { mut = false; elt } -> [%string "[]%{to_string ~resolve_class_name elt}"]
  | Object class_id -> resolve_class_name class_id
  | Top_object -> "object"
  | Fun { args; ret } ->
    "("
    ^ (List.map args ~f:(to_string ~resolve_class_name) |> String.concat ~sep:", ")
    ^ ") -> "
    ^ to_string ~resolve_class_name ret
  | Option ty -> "?" ^ to_string ~resolve_class_name ty
;;
