{
open Menhir_parser
open! Core

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let integer = ['0'-'9']+
let ident =   [ 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let cap_ident =   ['A'-'Z' ] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let hex_digit = ['0' - '9' 'a'-'f' 'A'-'F'] 

rule next_token = parse
  | eof { EOF }
  | whitespace+ { next_token lexbuf }
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { comment 0 lexbuf; next_token lexbuf }

  | integer as i { INT i }
  | integer as i "c" { CHAR_INT i }
  | "'" { char_lit lexbuf }

  | "class" { CLASS }
  | "const" { CONST }
  | "else" { ELSE }
  | "false" { FALSE }
  | "fun" { FUN }
  | "if" { IF }
  | "let" { LET }
  | "module" { MODULE }
  | "mut" { MUT }
  | "return" { RETURN }
  | "true" { TRUE }
  | "while" { WHILE }
  | "overrides" { OVERRIDES }
  | "public" { PUBLIC }
  | "private" { PRIVATE }
  | "constructor" { CONSTRUCTOR }
  | "evolves" { EVOLVES }
  | "this" { THIS }
  | "super" { SUPER }
  | "new" { NEW }
  | "if?" { IFO }
  | "orelse" { ORELSE }
  | "null" { NULL }
  | "extern" { EXTERN }
  | "from" { FROM }
  | "import" { IMPORT }
  | "interface" { INTERFACE }
  | "implements" { IMPLEMENTS }

  | ',' { COMMA }
  | '.' { DOT }
  | ';' { SEMI }
  | ':' { COLON }
  | '+' { PLUS }
  | '*' { STAR }
  | '-' { MINUS }
  | '/' { SLASH }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LE }
  | ">=" { GE }
  | "!=" { NEQ }
  | "==" { EQQ }
  | "=" { EQ }
  (* | ":=" { COLON_EQ } *)
  | "!" { BANG }
  | "&&" { AMP_AMP }
  | "||" { PIPE_PIPE }
  | ".?" { DOT_INTER }
  | "?" { INTER }
  | ".." { DOT_DOT }
  | ">=<" { XCHG }

  | '{' { O_CURLY }
  | '}' { C_CURLY }
  | '(' { O_PAREN }
  | ')' { C_PAREN }
  | '[' { O_SQUARE }
  | ']' { C_SQUARE }

  | '"' {string [] lexbuf}

  | ident as atom { IDENT (Ast.Ident.of_string atom) }
  | cap_ident as atom { CAP_IDENT (Ast.Ident.of_string atom) }

  | _ as c { illegal c }

and single_line_comment = parse
  | newline { Lexing.new_line lexbuf; next_token lexbuf}
  | _ { single_line_comment lexbuf}

and comment nesting = parse
  | "/*"
    { comment (nesting+1) lexbuf }
  | "*/"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { comment nesting lexbuf }

and char_lit = parse
  | [' '-'~'] as c { finish_char c lexbuf}
  | _ as c { failwith ("[lexer] unexpected character in character literal '" ^ Char.to_string c ^ "'")}

and finish_char c = parse
  | ''' { CHAR_LIT c }
  | _ { failwith "[lexer] unterminated character literal"}

and string so_far = parse
  | '"' { STRING (String.of_char_list (List.rev so_far)) }
  | "\\n" { string ('\n' :: so_far) lexbuf}
  | "\\\\" { string ('\\' :: so_far) lexbuf}
  | "\\\"" { string ('"' :: so_far) lexbuf}
  | "\\t" { string ('\t' :: so_far) lexbuf}
  | "\\0" { string ('\x00' :: so_far) lexbuf}
  | "\\" (_ as c) { failwith ("[lexer] invalid escape character '" ^ Char.to_string c ^"'")}
  | "\\x" ( hex_digit hex_digit as b) { string (Char.of_int_exn (Int.of_string ("0x" ^ b)) :: so_far) lexbuf }
  | _ as c { string (c :: so_far) lexbuf}