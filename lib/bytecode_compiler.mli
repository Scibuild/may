open! Core

module Function : sig
  type t [@@deriving sexp_of]
end

module Runtime_value : sig
  type slice =
    { underlying : t Array.t
    ; start : int
    ; len : int
    }
  [@@deriving sexp_of]

  and object_ =
    { mutable method_table : Type.Class_id.t option
    ; fields : t Array.t
    }

  and t =
    | Int of int
    | Function of Function.t
    | Object of object_
    | Float of float
    | Slice of slice
    | Undefined (** Represents unused memory *)
  [@@deriving sexp_of]
end

module Compilation_unit : sig
  type t [@@deriving sexp_of]

  val to_string : t -> string

  val compile_program
    :  check:Check.t
    -> decls:Tast.Decls.t
    -> t * Resolved_ident.Global.Id.t option
end

module VM : sig
  type t [@@deriving sexp_of]

  val of_compilation_unit : Compilation_unit.t -> Resolved_ident.Global.Id.t -> t
  val execute_vm : t -> Runtime_value.t
end
