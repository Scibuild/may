open! Core

let token_to_string (tk : Menhir_parser.token) =
  match tk with
  | Menhir_parser.CLASS -> "CLASS"
  | CONST -> "CONST"
  | WHILE -> "WHILE"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | SEMI -> "SEMI"
  | PLUS -> "PLUS"
  | O_SQUARE -> "O_SQUARE"
  | O_PAREN -> "O_PAREN"
  | O_CURLY -> "O_CURLY"
  | NEQ -> "NEQ"
  | MINUS -> "MINUS"
  | LT -> "LT"
  | LET -> "LET"
  | LE -> "LE"
  | IF -> "IF"
  | GT -> "GT"
  | GE -> "GE"
  | FUN -> "FUN"
  | EQQ -> "EQQ"
  | EQ -> "EQ"
  | EOF -> "EOF"
  | ELSE -> "ELSE"
  | DOT -> "DOT"
  | C_SQUARE -> "C_SQUARE"
  | C_PAREN -> "C_PAREN"
  | C_CURLY -> "C_CURLY"
  | COMMA -> "COMMA"
  (* | COLON_EQ -> "COLON_EQ" *)
  | COLON -> "COLON"
  | STRING s -> [%string "STRING(%{s})"]
  | INT i -> [%string "INT(%{i})"]
  | IDENT v -> [%string "IDENT(%{v#Ast.Ident})"]
  | CAP_IDENT v -> [%string "CAP_IDENT(%{v#Ast.Ident})"]
  | CHAR_INT v -> [%string "CHAR_INT(%{v})"]
  | CHAR_LIT v -> [%string "CHAR_LIT(%{v#Char})"]
  | PIPE_PIPE -> "PIPE_PIPE"
  | BANG -> "BANG"
  | AMP_AMP -> "AMP_AMP"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | MODULE -> "MODULE"
  | MUT -> "MUT"
  | RETURN -> "RETURN"
  | OVERRIDES -> "OVERRIDES"
  | PUBLIC -> "PUBLIC"
  | PRIVATE -> "PRIVATE"
  | CONSTRUCTOR -> "CONSTRUCTOR"
  | EVOLVES -> "EVOLVES"
  | THIS -> "THIS"
  | SUPER -> "SUPER"
  | NEW -> "NEW"
  | IFO -> "IFO"
  | NULL -> "NULL"
  | ORELSE -> "ORELSE"
  | DOT_INTER -> "DOT_INTER"
  | INTER -> "INTER"
  | EXTERN -> "EXTERN"
  | DOT_DOT -> "DOT_DOT"
  | XCHG -> "XCHG"
  | IMPORT -> "IMPORT"
  | FROM -> "FROM"
  | INTERFACE -> "INTERFACE"
  | IMPLEMENTS -> "IMPLEMENTS"
;;

type token = Menhir_parser.token

(* module Error = struct
  type t =
    | Lex_error of
        { msg : string
        ; loc : Range.t
        }
    | Parse_error of
        { token : token
        ; loc : Range.t
        }

  let to_formatted_string error =
    match error with
    | Lex_error { msg; loc } ->
      [%string "Lexer error: %{msg} at position %{loc#Range}"]
    | Parse_error { token; loc } ->
      [%string
        "Parser error: at token %{token_to_string token} at position %{loc#Range}"]
  ;;
end *)

let next_token curr_token lexbuf =
  let token = Lexer.next_token lexbuf in
  curr_token := Some token;
  token
;;

let curr_range lexbuf : Range.t = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf

let parse lexbuf parser =
  let curr_token : token option ref = ref None in
  try Ok (parser (next_token curr_token) lexbuf) with
  | Lexer.LexError msg ->
    Error (Comp_error.create ~stage:Lexer ~msg:[ Text msg ] ~range:(curr_range lexbuf))
  | Menhir_parser.Error ->
    (match !curr_token with
     | Some token ->
       Error
         (Comp_error.create
            ~stage:Parser
            ~msg:[ Text "at token "; Code_str (token_to_string token) ]
            ~range:(curr_range lexbuf))
     | None ->
       Error
         (Comp_error.create
            ~stage:Lexer
            ~msg:[ Text "Parse error encounter before lexing token" ]
            ~range:(curr_range lexbuf)))
;;

let parse_string_of_parser parser s = parse (Lexing.from_string s) parser
let parse_string s = parse_string_of_parser Menhir_parser.decl_prog s

module For_testing = struct
  let parse_expr_string = parse_string_of_parser Menhir_parser.expr_prog
end
