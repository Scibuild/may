type token = Menhir_parser.token

val token_to_string : token -> string
val parse_string : string -> (Ast.Decl.t list, Comp_error.t) result

module For_testing : sig
  val parse_expr_string : string -> (Ast.Expr.t, Comp_error.t) result
end
