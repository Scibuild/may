open! Core

module type S = sig
  module Id : Unique_id.Id

  module Row : sig
    type t

    val id : t -> Id.t
    val fqn : t -> Ast.Path.t
  end

  (** An [Env] is a general purpose environment for storing and retrieving data
    from a nested hierarchy structure that flattens that data. *)

  type t

  val create : unit -> t

  val insert
    :  t
    -> scope:Ast.Ident.t list
    -> name:Ast.Ident.t
    -> [ `Ok of Id.t | `Already_defined ]

  val find_name : t -> id:Id.t -> Ast.Path.t
  val find_id : t -> scope:Ast.Ident.t list -> qualified_name:Ast.Path.t -> Id.t option
end
