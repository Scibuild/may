open! Core

module Local : sig
  type t [@@deriving sexp_of]

  include Hashable.S_plain with type t := t
  include Comparable.S_plain with type t := t

  val create : name:Ast.Ident.t -> id:int -> t
  val name : t -> Ast.Ident.t
  val id : t -> int
  val to_string : t -> string
  val to_mangled_string : t -> string
end

module Global : sig
  module Id : Unique_id.Id

  type t [@@deriving sexp_of, equal]

  val to_mangled_string : t -> string
  val to_string : t -> string
  val id : t -> Id.t
end
