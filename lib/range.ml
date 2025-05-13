open! Core

module Position = struct
  type t = Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp_of]

  let to_string t =
    let line_num = t.pos_lnum in
    let col_num = 1 + t.pos_cnum - t.pos_bol in
    [%string "%{line_num#Int}:%{col_num#Int}"]
  ;;
end

type t = Position.t * Position.t [@@deriving sexp_of]

let to_string (s, e) = [%string "%{s#Position}-%{e#Position}"]
