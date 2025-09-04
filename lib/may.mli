open! Core
module Ast = Ast
module Parser = Parser
module Check = Check
module Bytecode_compiler = Bytecode_compiler
module Comp_error = Comp_error
module Mode = Mode
module Resolved_ident = Resolved_ident
module Range = Range
module Qbe_backend = Qbe_backend
module Tast = Tast
module Type = Type

module For_testing : sig
  val parse_string : string -> (Ast.Decl.t list, Comp_error.t) Result.t
  val expr_parse_string : string -> (Ast.Expr.t, Comp_error.t) Result.t
  val lex_string : string -> Parser.token list
end
