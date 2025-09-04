open! Core

type t

val empty : mode:Mode.t -> t
val check_expr : t -> ty:Type.t -> term:Ast.Expr.t -> (Tast.Expr.t, Comp_error.t) Result.t
val infer_expr : t -> term:Ast.Expr.t -> (Tast.Expr.t, Comp_error.t) Result.t

val check_decls
  :  t
  -> decls:Ast.Decl.t list
  -> load_file:
       (range:Range.t
        -> current_file:Ast.Ident.t
        -> filepath:string
        -> (Ast.Ident.t * Ast.Decl.t list, Comp_error.t) Result.t)
  -> starting_file:Ast.Ident.t
  -> (Tast.Decls.t, Comp_error.t) Result.t

val env : t -> Env.t
val class_table : t -> Type.Class.t Type.Class_id.Table.t
val interface_table : t -> Type.Interface.t Type.Interface_id.Table.t

module For_testing : sig
  module Function_check : sig
    type t

    val create
      :  return_type:Type.t
      -> ?this_id:Type.Class_id.t * Type.Ownership.t
      -> unit
      -> t
  end

  val with_function_check : t -> Function_check.t -> t

  module Type : sig
    val to_string : t -> Type.t -> string
  end

  val to_string : t -> string
end
