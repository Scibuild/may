module Position : sig
  type t = Lexing.position [@@deriving sexp_of]

  val to_string : t -> string
end

type t = Position.t * Position.t [@@deriving sexp_of]

val to_string : t -> string
