open! Core

module Id : sig
  module Global = Resolved_ident.Global.Id
  module Class = Type.Class_id
  module Interface = Type.Interface_id

  type t = private
    | Global of Resolved_ident.Global.Id.t
    | Class of Type.Class_id.t
    | Interface of Interface.t
end

module Row : sig
  type t

  val id : t -> Id.t
  val fqn : t -> Ast.Path.t
end

(** An [Env] is a general purpose environment for storing and retrieving data
  from a nested hierarchy structure that flattens that data. *)

type t

val create : unit -> t

(** Note that it is a compiler error for the scope to contain modules that have not been 
    inserted. *)
val insert_global
  :  t
  -> scope:Ast.Ident.t list
  -> name:Ast.Ident.t
  -> [ `Ok of Id.Global.t | `Already_defined ]

(** Note that it is a compiler error for the scope to contain modules that have not been 
    inserted. *)
val insert_class
  :  t
  -> scope:Ast.Ident.t list
  -> name:Ast.Ident.t
  -> [ `Ok of Id.Class.t | `Already_defined ]

(** Note that it is a compiler error for the scope to contain modules that have not been 
    inserted. *)
val insert_interface
  :  t
  -> scope:Ast.Ident.t list
  -> name:Ast.Ident.t
  -> [ `Ok of Id.Interface.t | `Already_defined ]

(** Note that it is a compiler error for the scope to contain modules that have not been 
    inserted. *)
val insert_module
  :  t
  -> scope:Ast.Ident.t list
  -> name:Ast.Ident.t
  -> [ `Ok | `Duplicate ]

(** Note that it is a compiler error for the scope to contain modules that have not been 
    inserted. *)
val insert_module_alias
  :  t
  -> scope:Ast.Ident.t list
  -> name:Ast.Ident.t
  -> module_path:Ast.Ident.t list
  -> [ `Ok | `Duplicate ]

val find_class_name : t -> id:Id.Class.t -> Ast.Path.t
val find_global_name : t -> id:Id.Global.t -> Ast.Path.t
val find_interface_name : t -> id:Id.Interface.t -> Ast.Path.t
val find_id : t -> scope:Ast.Ident.t list -> qualified_name:Ast.Path.t -> Id.t option
