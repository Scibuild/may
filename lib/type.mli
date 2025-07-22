open! Core

module Numeric : sig
  type t =
    | Int
    | Char
    | Float
  [@@deriving sexp_of, equal, bin_io]

  val is_integral : t -> bool
  val bitwidth : t -> int
end

module Class_id : Unique_id.Id

type fun_signature =
  { args : t list
  ; ret : t
  }

and t =
  | Bottom
  (** Subtype of all types. Refers to expressions that do not finish computing. *)
  | Unit (** A type with a single value. Can be optimised away. *)
  | Bool
  | Numeric of Numeric.t
  | Array of
      { mut : bool
      ; elt : t
      }
  | Fun of fun_signature
  | Object of Class_id.t (** An object is an instance of a particular class. *)
  | Owned_object of Class_id.t
  | Top_object
  | Option of t
[@@deriving sexp_of, equal]

val is_reference : t -> bool
val class_id_exn : t -> Class_id.t
val erase_ownership : t -> t

module Class : sig
  type ty := t

  module Field : sig
    type t =
      { ty : ty
      ; mut : bool
      ; overrides : bool
      ; visibility : Ast.Decl.Visibility.t
      }
    [@@deriving sexp_of, fields ~getters]
  end

  module Constructor : sig
    type t = { args : ty list } [@@deriving sexp_of]
  end

  module Method : sig
    type t =
      { visibility : Ast.Decl.Visibility.t
      ; function_ : fun_signature
      ; overrides : bool
      }
    [@@deriving sexp_of, fields ~getters]

    val overrides : t -> bool
  end

  type t =
    { id : Class_id.t
    ; fields : Field.t Ast.Ident.Map.t
    ; constructor : Constructor.t option
    ; evolver : Constructor.t option
    ; methods : Method.t Ast.Ident.Map.t
    ; super : Class_id.t option
    }
  [@@deriving sexp_of]
end

val to_string : resolve_class_name:(Class_id.t -> string) -> t -> string
