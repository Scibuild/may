open! Core
module Ast = Ast
module Parser = Parser
module Check = Check
module Bytecode_compiler = Bytecode_compiler
module Comp_error = Comp_error
module Tast = Tast
module Resolved_ident = Resolved_ident
module Qbe_backend = Qbe_backend
module Type = Type

module For_testing = struct
  let parse_string = Parser.parse_string
  let expr_parse_string = Parser.For_testing.parse_expr_string

  let lex_string s =
    let lexbuf = Lexing.from_string s in
    let rec each_token acc =
      match Lexer.next_token lexbuf with
      | Menhir_parser.EOF -> []
      | anything_else -> each_token (anything_else :: acc)
    in
    List.rev (each_token [])
  ;;
end
