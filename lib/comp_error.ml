open! Core

module Stage = struct
  type t =
    | Lexer
    | Parser
    | Type_checker [@rename "Type"]
  [@@deriving string ~capitalize:"Sentence case"]
end

module Msg_part = struct
  type t =
    | Text of string
    | Code of Ast.Expr.t
    | Ident of Ast.Ident.t
    | Path of Ast.Path.t
    | Code_str of string
end

type t =
  { stage : Stage.t
  ; msg : Msg_part.t list
  ; range : Range.t
  }

let create ~stage ~range ~msg = { stage; msg; range }

let to_string { stage; msg; range } =
  let msg_joined =
    msg
    |> List.map ~f:(function
      | Msg_part.Text t -> t
      | Code expr -> Ast.Expr.to_string expr
      | Ident i -> Ast.Ident.to_string i
      | Path p -> Ast.Path.to_string p
      | Code_str s -> s)
    |> String.concat ~sep:""
  in
  [%string "%{stage#Stage} error at %{range#Range}: %{msg_joined}"]
;;

module Or_error = struct
  let of_option v ~stage ~range ~msg =
    Result.of_option v ~error:(create ~stage ~range ~msg)
  ;;
end
