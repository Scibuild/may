open! Core

type t

module Global_env : Env.S with type Id.t := Resolved_ident.Global.Id.t
module Class_env : Env.S with type Id.t := Type.Class_id.t

val empty : unit -> t
val check_expr : t -> ty:Type.t -> term:Ast.Expr.t -> (Tast.Expr.t, Comp_error.t) Result.t
val infer_expr : t -> term:Ast.Expr.t -> (Tast.Expr.t, Comp_error.t) Result.t
val check_decls : t -> decls:Ast.Decl.t list -> (Tast.Decls.t, Comp_error.t) Result.t
val global_env : t -> Global_env.t
val class_env : t -> Class_env.t
val class_table : t -> Type.Class.t Type.Class_id.Table.t

module For_testing : sig
  module Function_check : sig
    type t

    val create : return_type:Type.t -> ?this_id:Type.Class_id.t -> unit -> t
  end

  val with_function_check : t -> Function_check.t -> t

  module Type : sig
    val to_string : t -> Type.t -> string
  end

  val to_string : t -> string
end
