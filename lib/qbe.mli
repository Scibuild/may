open! Core
module Block_name : String_id.S

module Const : sig
  type t =
    [ `Int of int
    | `Sp_float of float
    | `Dp_float of float
    | `Symbol of string
    ]
end

module Dyn_const : sig
  type t =
    [ `Thread_local of string
    | Const.t
    ]
end

module Value : sig
  type t =
    [ `Local of string
    | Dyn_const.t
    ]

  val to_string : t -> string
end

module Base_ty : sig
  type t =
    [ `W
    | `L
    | `S
    | `D
    ]
end

module Ext_ty : sig
  type t =
    [ Base_ty.t
    | `B
    | `H
    ]
end

module Abi_ty : sig
  type t =
    [ Base_ty.t
    | `SB
    | `UB
    | `SH
    | `UH
    | `Name of string
    ]
end

module Type : sig
  val to_string : [< Abi_ty.t | Ext_ty.t ] -> string
end

module Instr : sig
  type t

  val create
    :  ?comment:string
    -> ?ty:Base_ty.t
    -> ?v:string
    -> op:string
    -> ?args:Value.t list
    -> unit
    -> t

  val to_string : t -> string

  module Jump : sig
    type t =
      | Ret
      | Ret_val of Value.t
      | Jmp of Block_name.t
      | Jnz of
          { v : Value.t
          ; ifz : Block_name.t
          ; ifnz : Block_name.t
          }
      | Hlt

    val jnz : v:Value.t -> ifz:Block_name.t -> ifnz:Block_name.t -> t
    val to_string : t -> string
  end
end

module Block : sig
  type t

  val create : name:Block_name.t -> t
  val add_jump : t -> jump:Instr.Jump.t -> unit

  (** Note that adding an instruction after adding a jump is a no-op and will silently 
      fail. This is to make codegen easier but can be disabled in source for debugging 
      purposes. *)
  val add_instr : t -> Instr.t -> unit

  val name : t -> Block_name.t

  (** Note that adding an instruction after adding a jump is a no-op and will silently 
      fail. This is to make codegen easier but can be disabled in source for debugging 
      purposes. *)
  val addi
    :  t
    -> ?comment:string
    -> ?ty:Base_ty.t
    -> ?v:string
    -> op:string
    -> ?args:Value.t list
    -> unit
    -> unit

  val add_to_buffer : Bigbuffer.t -> t -> unit
  val to_string : t -> string
end

module Linkage : sig
  type t =
    | Export
    | Thread
    | Section of string

  val to_string : t -> string
end

module Function : sig
  type t

  val create
    :  ?return_type:Abi_ty.t
    -> ?parameters:(string * Abi_ty.t) list
    -> name:string
    -> ?linkage:Linkage.t list
    -> unit
    -> t

  val param_values : t -> Value.t list
  val add_block : t -> name:string -> Block.t
  val to_string : t -> string
  val add_to_buffer : t -> Bigbuffer.t -> unit
end

module Data : sig
  module Item : sig
    module Elt : sig
      type t =
        [ Const.t
        | `Symbol_plus of string * int
        | `String of string
        ]
    end

    type t =
      | Pad of int
      | I of Ext_ty.t * Elt.t
  end

  type t

  val name : t -> string
  val create : name:string -> ?linkage:Linkage.t list -> Item.t list -> t
  val add_to_buffer : t -> Bigbuffer.t -> unit
end

module Decl : sig
  type t =
    | Data of Data.t
    | Function of Function.t

  val add_to_buffer : t -> Bigbuffer.t -> unit
end
