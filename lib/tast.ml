open! Core

module Expr = struct
  type expr_kind =
    | Lit_int of string
    | Lit_string of string
    | Lit_bool of bool
    | Lit_char of char
    | Lit_array of t list
    | Local of Resolved_ident.Local.t
    | Global of Resolved_ident.Global.Id.t
    (* TODO: turn this into a local that's implicitly passed around. *)
    | This
    | Super
    | Bin_op of
        { lhs : t
        ; op : Ast.Expr.Bin_op.t
        ; rhs : t
        }
    | Un_op of
        { op : Ast.Expr.Un_op.t
        ; rhs : t
        }
    | If of
        { cond : t
        ; if_then : t
        ; if_else : t
        }
    | Unit
    | While of
        { cond : t
        ; block : t
        }
    | Block of t list
    | Field_subscript of
        { expr : t
        ; field : Ast.Ident.t
          (* TODO: This should probably also be resolved to an offset. *)
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
        { local : Resolved_ident.Local.t
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
        ; method_ : Ast.Ident.t
          (* TODO: This should probably also be resolved to an offset. *)
        ; arguments : t list
        }
    | New of
        { class_id : Type.Class_id.t
        ; arguments : t list
        }
    | New_array of
        { size : t
        ; init : t
        }
    | Evolves of
        { expr : t
        ; class_id : Type.Class_id.t
        ; arguments : t list
        }
    | Super_call of
        { super_id : Type.Class_id.t
        ; arguments : t list
        }
    | Super_method_call of
        { super_id : Type.Class_id.t
        ; method_ : Ast.Ident.t
        ; arguments : t list
        }
    | Return of t
    | Update_this_vtable_after of
        { expr : t
        ; new_table : Type.Class_id.t
        }
    | Null
    | If_option of
        { expr : t
        ; var : Resolved_ident.Local.t
        ; if_value : t
        ; if_null : t
        }
    | Or_else of
        { lhs : t
        ; or_else : t
        }
    | De_null of { lhs : t }
    | Exchange of
        { lhs : t
        ; rhs : t
        }

  and t =
    { kind : expr_kind
    ; ty : Type.t
    ; range : Range.t
    }
  [@@deriving sexp_of, fields ~getters]

  let create ~expr ~ty ~range = { kind = expr; ty; range }
end

module Decl = struct
  module Const = struct
    type t =
      { id : Resolved_ident.Global.Id.t
      ; expr : Expr.t
      ; range : Range.t
      }
  end

  module Function = struct
    module Args = struct
      type t = (Resolved_ident.Local.t * Type.t) list
    end

    type t =
      { id : Resolved_ident.Global.Id.t
      ; args : Args.t
      ; ret_type : Type.t
      ; body : Expr.t
      ; range : Range.t
      }
  end

  module Extern_function = struct
    type t =
      { id : Resolved_ident.Global.Id.t
      ; arg_tys : Type.t list
      ; ret_type : Type.t
      ; external_name : string
      ; range : Range.t
      }
  end

  module Class = struct
    module Field = struct
      type t =
        { name : Ast.Ident.t
        ; ty : Type.t
        ; visibility : Ast.Decl.Visibility.t
        ; overrides : bool
        ; mut : bool
        ; range : Range.t
        }
    end

    module Constructor = struct
      type t =
        { args : Function.Args.t
        ; body : Expr.t
        ; range : Range.t
        }
    end

    module Method = struct
      type t =
        { visibility : Ast.Decl.Visibility.t
        ; name : Ast.Ident.t
        ; args : Function.Args.t
        ; ret_type : Type.t
        ; body : Expr.t
        ; overrides : bool
        ; range : Range.t
        }
    end

    type t =
      { id : Type.Class_id.t
      ; super_type : Type.Class_id.t option
      ; fields : Field.t list
      ; constructor : Constructor.t option
      ; evolver : Constructor.t option
      ; methods : Method.t list
      ; range : Range.t
      }
  end
end

module Decls = struct
  type t =
    { constants : Decl.Const.t list
    ; classes : Decl.Class.t list
    ; functions : Decl.Function.t list
    ; extern_functions : Decl.Extern_function.t list
    }
end
