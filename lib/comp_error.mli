open! Core

module Stage : sig
  type t =
    | Lexer
    | Parser
    | Type_checker
end

module Msg_part : sig
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

val create : stage:Stage.t -> range:Range.t -> msg:Msg_part.t list -> t
val to_string : t -> string

module Or_error : sig
  val of_option
    :  'a option
    -> stage:Stage.t
    -> range:Range.t
    -> msg:Msg_part.t list
    -> ('a, t) Result.t
end
