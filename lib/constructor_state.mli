open! Core

type t = private
  { uninitialised_fields :
      [ `Uninit | `Uninit_needs_owned | `Semi_init | `Init ] Ast.Ident.Map.t
  ; needs_super : bool
  ; vtable_initialised : bool
  ; evolving_fields : Ast.Ident.Set.t
    (** [this.a] has type !Blah within in this evolver *)
  }
[@@deriving equal, sexp_of]

val create
  :  class_signature:Type.Class.t
  -> super_class_signature:Type.Class.t option
  -> needs_super:bool
  -> t ref

val can_initialise_field : t ref -> field:Ast.Ident.t -> [ `Yes | `No | `With_ownership ]
val read_field_ownership : t ref -> field:Ast.Ident.t -> Type.Ownership.t
val finish_initialising_field : t ref -> field:Ast.Ident.t -> unit
val is_complete : t -> bool
val initialise_vtable : t ref -> unit
val initialise_super : t ref -> unit
