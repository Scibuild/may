open! Core

module Compilation_unit : sig
  type t

  val to_bytes : t -> bytes
  val compile_program : check:Check.t -> decls:Tast.Decls.t -> t * string option
end
