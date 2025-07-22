open! Core

module Node : sig
  type 'a t =
    { data : 'a
    ; range : Range.t
    }
  [@@deriving sexp_of, fields]

  val create : data:'a -> loc:Range.t -> 'a t
end

module Ident : sig
  include String_id.S

  val is_module : t -> bool
end

module Path : sig
  type t = private Ident.t Nonempty_list.t [@@deriving sexp_of]

  include Hashable.S_plain with type t := t

  val of_list : Ident.t Nonempty_list.t -> t
  val to_nonempty_list : t -> Ident.t Nonempty_list.t
  val of_name_in_scope : name:Ident.t -> scope:Ident.t list -> t
  val qualify : path:t -> scope:Ident.t list -> t
  val of_string : string -> t
  val to_string : ?sep:string -> t -> string
end

module Type : sig
  type type_kind =
    | Ident of Ident.t
    (** [Ident] is used for lower case type names, like [bool], [int], etc.*)
    | Path of Path.t
    (** [Path] is used for references to user defined types, like [Module.Class] *)
    | Owned of Path.t
    (** [Owned] is used for the unique reference to the evolvable version of a class
        [!Module.Class] *)
    | Array of
        { mut : bool
        ; elt : t
        }
    | Option of t
  [@@deriving sexp_of]

  and t = type_kind Node.t [@@deriving sexp_of]

  val create : ty:type_kind -> loc:Range.t -> t
  val to_string : t -> string
end

module Expr : sig
  module Bin_op : sig
    type t =
      | Add
      | Sub
      | Mul
      | Div
      | Lt
      | Gt
      | Ge
      | Le
      | Eq
      | Ne
      | And
      | Or
    [@@deriving sexp_of]

    val to_symbol : t -> string
  end

  module Un_op : sig
    type t =
      | Neg
      | Not
    [@@deriving sexp_of]

    val to_symbol : t -> string
  end

  type expr_kind =
    | Lit_int of string
    | Lit_string of string
    | Lit_bool of bool
    | Lit_char of char
    | Lit_array of t list
    | Ident of Path.t
    | This
    | Super
    | Bin_op of
        { lhs : t
        ; op : Bin_op.t
        ; rhs : t
        }
    | Un_op of
        { op : Un_op.t
        ; rhs : t
        }
    | If of
        { cond : t
        ; if_then : t
        ; if_else : t option
        }
    | Unit
    | While of
        { cond : t
        ; block : t
        }
    | Block of t list
    | Field_subscript of
        { expr : t
        ; field : Ident.t
        }
    | Array_subscript of
        { expr : t
        ; index : t
        }
    | Array_subrange of
        { expr : t
        ; from : t
        ; to_ : t
        }
    | Let of
        { ident : Ident.t
        ; annot : Type.t option
        ; expr : t
        }
    | Assign of
        { lhs : t
        ; rhs : t
        }
    | Function_call of
        { expr : t
        ; arguments : t list
        }
    | Method_call of
        { expr : t
        ; method_ : Ident.t
        ; arguments : t list
        }
    | New of
        { class_ : Path.t
        ; arguments : t list
        }
    | New_array of
        { size : t
        ; ty : Type.t
        ; init : t
        }
    | Evolves of
        { expr : t
        ; class_ : Path.t
        ; arguments : t list
        }
    | Return of t
    | Null
    | If_option of
        { expr : t
        ; var : Ident.t
        ; annot : Type.t option
        ; if_value : t
        ; if_null : t option
        }
    | Or_else of
        { lhs : t
        ; or_else : t
        }
    | De_null of { lhs : t }
    | Exchange of
        { expr1 : t
        ; expr2 : t
        }
  [@@deriving sexp_of]

  and t = expr_kind Node.t [@@deriving sexp_of]

  val create : expr:expr_kind -> loc:Range.t -> t
  val to_string : t -> string
end

module Decl : sig
  module Visibility : sig
    type t =
      | Private
      | Public
    [@@deriving sexp_of, equal, compare, hash]

    val is_visible : t -> to_:t -> bool
  end

  module Function : sig
    module Args : sig
      type t = (Ident.t * Type.t) list [@@deriving sexp_of]
    end

    type t =
      { name : Ident.t
      ; args : Args.t
      ; ret_type : Type.t
      ; body : Expr.t
      }
    [@@deriving sexp_of]

    val create : name:Ident.t -> args:Args.t -> ret_type:Type.t -> body:Expr.t -> t
  end

  module Class : sig
    module Field : sig
      type t =
        { name : Ident.t
        ; ty : Type.t
        ; visibility : Visibility.t
        ; overrides : bool
        ; mut : bool
        }
      [@@deriving sexp_of]

      val to_string : depth:int -> t -> string
    end

    module Constructor : sig
      type t =
        { args : Function.Args.t
        ; evolves : Path.t option
          (* TODO: fix that you can't reference classes in other modules *)
        ; body : Expr.t
        }
      [@@deriving sexp_of]

      val to_string : depth:int -> t -> string
    end

    module Method : sig
      type t =
        { visibility : Visibility.t
        ; function_ : Function.t
        ; overrides : bool
        }
      [@@deriving sexp_of]

      val to_string : depth:int -> t -> string
    end

    type t =
      { name : Ident.t
      ; super_type : Path.t option
      ; fields : Field.t Node.t list
      ; constructors : Constructor.t Node.t list
      ; methods : Method.t Node.t list
      }
    [@@deriving sexp_of]

    val to_string : depth:int -> t -> string
  end

  type decl_kind =
    | Function of Function.t
    | Class of Class.t
    | Constant of
        { ident : Ident.t
        ; annot : Type.t option
        ; value : Expr.t
        }
    | Module of
        { name : Ident.t
        ; decls : t list
        }
    | Extern_function of
        { name : Ident.t
        ; arg_tys : Type.t list
        ; ret_type : Type.t
        ; external_name : string
        }
  [@@deriving sexp_of]

  and t = decl_kind Node.t

  val create : decl:decl_kind -> loc:Range.t -> t
  val to_string : ?depth:int -> t -> string
end
