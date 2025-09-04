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
module Interface_id : Unique_id.Id

module Ownership : sig
  type t =
    | Owned
    | Shared
  [@@deriving sexp_of, equal]

  val is_owned : t -> bool
  val is_shared : t -> bool
end

module Object_kind : sig
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
  (** Subtype of all types. Refers to expressions that do not finish computing. *)
  | Unit (** A type with a single value. Can be optimised away. *)
  | Bool
  | Numeric of Numeric.t
  | Array of
      { mut : bool
      ; elt : t
      }
  | Fun of fun_signature
  | Object of (Ownership.t * Object_kind.t)
  (** An object is an instance of a particular class/interface, with an ownership permission. *)
  | Top_object
  | Option of t
[@@deriving sexp_of, equal]

val is_reference : t -> bool
val erase_ownership : t -> t
val promote_ownership : t -> t

module Class : sig
  type ty := t

  module Field : sig
    type t =
      { ty : ty
      ; mut : bool
      ; overrides : bool
      ; evolves : bool
      ; visibility : Ast.Decl.Visibility.t
      }
    [@@deriving sexp_of, fields ~getters]

    val evolves : t -> bool
  end

  module Constructor : sig
    type t = { args : ty list } [@@deriving sexp_of]
  end

  module Method : sig
    type t =
      { visibility : Ast.Decl.Visibility.t
      ; function_ : fun_signature
      ; overrides : bool
      ; receiver_evolves : bool
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
    ; implements : Interface_id.t list
    }
  [@@deriving sexp_of]
end

module Interface : sig
  module Method_signature : sig
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

val to_string : resolve_object_kind_name:(Object_kind.t -> string) -> t -> string
