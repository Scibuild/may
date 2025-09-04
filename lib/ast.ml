open! Core
open Composition_infix

module Node = struct
  type 'a t =
    { data : 'a
    ; range : Range.t
    }
  [@@deriving fields]

  let sexp_of_t f t = f t.data
  let create ~data ~loc = { data; range = loc }
end

module Ident = struct
  include
    String_id.Make
      (struct
        let module_name = "Ident"
      end)
      ()

  let is_module t = Char.is_uppercase (String.nget (to_string t) 0)
end

module Path = struct
  module T = struct
    type t = Ident.t Nonempty_list.t [@@deriving sexp_of, hash, compare, equal]
  end

  include T
  include Hashable.Make_plain (T)

  let of_list x = x

  let to_string ?(sep = ".") p =
    p |> Nonempty_list.to_list |> List.map ~f:Ident.to_string |> String.concat ~sep
  ;;

  let of_string s =
    s |> String.split ~on:'.' |> List.map ~f:Ident.of_string |> Nonempty_list.of_list_exn
  ;;

  let to_nonempty_list x = x
  let of_name_in_scope ~name ~scope = Nonempty_list.of_list_exn (scope @ (name :: []))
  let qualify ~path ~scope = Nonempty_list.of_list_exn (scope @ Nonempty_list.to_list path)
end

module Type = struct
  type type_kind =
    | Ident of Ident.t
    | Path of Path.t
    | Owned of Path.t
    | Array of
        { mut : bool
        ; elt : t
        }
    | Option of t
  [@@deriving sexp_of]

  and t = type_kind Node.t [@@deriving sexp_of]

  let create ~ty ~(loc : Range.t) = Node.create ~data:ty ~loc

  let rec to_string t =
    match Node.data t with
    | Ident i -> Ident.to_string i
    | Path p -> Path.to_string p
    | Owned p -> [%string "!%{p#Path}"]
    | Array { mut; elt } ->
      (match mut with
       | true -> [%string "([]mut %{to_string elt})"]
       | false -> [%string "([]%{to_string elt})"])
    | Option t -> [%string "?%{to_string t}"]
  ;;
end

module Expr = struct
  module Bin_op = struct
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

    let to_symbol = function
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Lt -> "<"
      | Gt -> ">"
      | Ge -> ">="
      | Le -> "<="
      | Eq -> "=="
      | Ne -> "!="
      | And -> "&&"
      | Or -> "||"
    ;;
  end

  module Un_op = struct
    type t =
      | Neg
      | Not
    [@@deriving sexp_of]

    let to_symbol = function
      | Neg -> "-"
      | Not -> "!"
    ;;
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

  and t = expr_kind Node.t [@@deriving sexp_of]

  let create ~expr ~loc = Node.create ~data:expr ~loc

  let rec to_string t =
    let comma_sep = List.map ~f:to_string >> String.concat ~sep:", " in
    let str_of_annot annot =
      annot
      |> Option.map ~f:Type.to_string
      |> Option.map ~f:(String.append ": ")
      |> Option.value ~default:""
    in
    match Node.data t with
    | Lit_int x -> x
    | Lit_string x -> [%string "\"%{x}\""]
    | Lit_bool b -> Bool.to_string b
    | Lit_char c -> [%string "'%{c#Char}'"]
    | Lit_array elts -> [%string "[%{comma_sep elts}]"]
    | Ident x -> Path.to_string x
    | This -> "this"
    | Super -> "super"
    | Bin_op { lhs; op; rhs } ->
      "(" ^ to_string lhs ^ " " ^ Bin_op.to_symbol op ^ " " ^ to_string rhs ^ ")"
    | Un_op { op; rhs } -> "(" ^ Un_op.to_symbol op ^ to_string rhs ^ ")"
    | Unit -> "()"
    | If { cond; if_then; if_else } ->
      (match if_else with
       | None -> [%string "(if (%{to_string cond}) %{to_string if_then})"]
       | Some if_else ->
         [%string
           "(if (%{to_string cond}) %{to_string if_then} else %{to_string if_else})"])
    | While { cond; block } -> [%string "(while (%{to_string cond}) %{to_string block})"]
    | Block exprs ->
      let exprs_joined = exprs |> List.map ~f:to_string |> String.concat ~sep:"; " in
      [%string "{ %{exprs_joined} }"]
    | Field_subscript { expr; field } -> [%string "(%{to_string expr}.%{field#Ident})"]
    | Array_subscript { expr; index } ->
      [%string "(%{to_string expr}[%{to_string index}])"]
    | Array_subrange { expr; from; to_ } ->
      [%string "(%{to_string expr}[%{to_string from} .. %{to_string to_}])"]
    | Let { ident; expr; annot } ->
      [%string "(let %{ident#Ident}%{str_of_annot annot} = %{to_string expr})"]
    | Assign { lhs; rhs } -> [%string "(%{to_string lhs} = %{to_string rhs})"]
    | Function_call { expr; arguments } ->
      [%string "(%{to_string expr}(%{comma_sep arguments}))"]
    | Return expr -> [%string "(return %{to_string expr})"]
    | Method_call { expr; method_; arguments } ->
      [%string "(%{to_string expr}:%{method_#Ident}(%{comma_sep arguments}))"]
    | New { class_; arguments } ->
      [%string "(new %{class_#Path}(%{comma_sep arguments}))"]
    | New_array { size; ty; init } ->
      [%string "(new [%{to_string size}]%{ty#Type}(%{to_string init}))"]
    | Evolves { expr; class_; arguments } ->
      [%string "(%{to_string expr} evolves %{class_#Path}(%{comma_sep arguments}))"]
    | De_null { lhs } -> to_string lhs ^ ".?"
    | If_option { expr; var; annot; if_value; if_null = None } ->
      [%string
        "(if? %{var#Ident}%{str_of_annot annot} = (%{to_string expr}) %{to_string \
         if_value})"]
    | If_option { expr; var; annot; if_value; if_null = Some if_null } ->
      [%string
        "(if? %{var#Ident}%{str_of_annot annot} = (%{to_string expr}) %{to_string \
         if_value} else %{to_string if_null})"]
    | Or_else { lhs; or_else } -> [%string "%{to_string lhs} orelse %{to_string or_else}"]
    | Null -> "null"
    | Exchange { expr1; expr2 } -> [%string "%{to_string expr1} >=< %{to_string expr2}"]
  ;;
end

module Decl = struct
  module Visibility = struct
    (* TODO: consider Protected*)
    type t =
      | Private
      | Public
    [@@deriving sexp_of, equal, compare, hash]

    let to_string ?(with_trailing_space = true) ?default t =
      match default, t, with_trailing_space with
      | (None | Some Private), Public, true -> "public "
      | (None | Some Private), Public, false -> "public"
      | (None | Some Public), Private, true -> "private "
      | (None | Some Public), Private, false -> "private"
      | Some Public, Public, (true | false) | Some Private, Private, (true | false) -> ""
    ;;

    let is_visible vis ~to_ =
      match vis, to_ with
      | Public, _ | Private, Private -> true
      | Private, Public -> false
    ;;
  end

  module Function = struct
    module Args = struct
      type t = (Ident.t * Type.t) list [@@deriving sexp_of]

      let to_string args =
        args
        |> List.map ~f:(fun (ident, ty) -> [%string "%{ident#Ident} : %{ty#Type}"])
        |> String.concat ~sep:", "
      ;;
    end

    type t =
      { name : Ident.t
      ; args : Args.t
      ; ret_type : Type.t
      ; body : Expr.t
      }
    [@@deriving sexp_of, fields ~iterators:create]

    let create = Fields.create

    let to_string { name; args; ret_type; body } =
      [%string "fun %{name#Ident}(%{args#Args}) : %{ret_type#Type} %{body#Expr}"]
    ;;
  end

  let mk_on_line ~depth s =
    let indentation = String.make depth ' ' in
    "\n" ^ indentation ^ s
  ;;

  module Class = struct
    module Field = struct
      type t =
        { name : Ident.t
        ; ty : Type.t
        ; visibility : Visibility.t
        ; overrides : bool
        ; evolves : bool
        ; mut : bool
        }
      [@@deriving sexp_of]

      let to_string ~depth { visibility; mut; overrides; evolves; name; ty } =
        let visibility_str = Visibility.to_string ~default:Private visibility in
        let mut_str = if mut then "mut " else "" in
        let overrides_str = if overrides then "overrides " else "" in
        let evolves_str = if evolves then "evolves " else "" in
        mk_on_line
          ~depth
          [%string
            "%{visibility_str}%{overrides_str}%{evolves_str}%{mut_str}%{name#Ident} : \
             %{ty#Type};"]
      ;;
    end

    module Constructor = struct
      type t =
        { args : Function.Args.t
        ; evolves : Path.t option
        ; body : Expr.t
        }
      [@@deriving sexp_of]

      let to_string ~depth { args; evolves; body } =
        let evolves_str =
          match evolves with
          | Some ident -> [%string " evolves %{ident#Path}"]
          | None -> ""
        in
        mk_on_line
          ~depth
          [%string "constructor(%{args#Function.Args})%{evolves_str} %{body#Expr}"]
      ;;
    end

    module Method = struct
      type t =
        { visibility : Visibility.t
        ; function_ : Function.t
        ; overrides : bool
        ; receiver_evolves : bool
        }
      [@@deriving sexp_of]

      let to_string ~depth { visibility; function_; overrides; receiver_evolves } =
        let overrides_str = if overrides then "overrides " else "" in
        let receiver_evolves_str = if receiver_evolves then "evolves " else "" in
        mk_on_line
          ~depth
          (Visibility.to_string ~default:Public visibility
           ^ overrides_str
           ^ receiver_evolves_str
           ^ Function.to_string function_)
      ;;
    end

    type t =
      { name : Ident.t
      ; super_type : Path.t option
      ; implements : Path.t list
      ; fields : Field.t Node.t list
      ; constructors : Constructor.t Node.t list
      ; methods : Method.t Node.t list
      }
    [@@deriving sexp_of]

    let implements_to_string implements =
      match implements with
      | [] -> ""
      | _ ->
        let s = implements |> List.map ~f:Path.to_string |> String.concat ~sep:", " in
        [%string "implements %{s} "]
    ;;

    let to_string ~depth { name; super_type; implements; fields; constructors; methods } =
      let super_type_string =
        match super_type with
        | Some path -> [%string " < %{path#Path}"]
        | None -> ""
      in
      let fields_string =
        fields
        |> List.map ~f:(Node.data >> Field.to_string ~depth:(depth + 2))
        |> String.concat
      in
      let constructors_string =
        constructors
        |> List.map ~f:(Node.data >> Constructor.to_string ~depth:(depth + 2))
        |> String.concat
      in
      let methods_string =
        methods
        |> List.map ~f:(Node.data >> Method.to_string ~depth:(depth + 2))
        |> String.concat
      in
      mk_on_line
        ~depth
        [%string
          "class %{name#Ident}%{super_type_string} %{implements_to_string implements}{ \
           %{fields_string}%{constructors_string}%{methods_string}"]
      ^ mk_on_line ~depth "}\n"
    ;;
  end

  module Function_signature = struct
    type t =
      { name : Ident.t
      ; arg_tys : Type.t list
      ; ret_type : Type.t
      }
    [@@deriving sexp_of]

    let to_string { name; arg_tys; ret_type } =
      let arg_tys_string =
        arg_tys |> List.map ~f:Type.to_string |> String.concat ~sep:", "
      in
      [%string "fun %{name#Ident}(%{arg_tys_string}) : %{ret_type#Type}"]
    ;;
  end

  module Interface = struct
    module Method_signature = struct
      type t =
        { function_signature : Function_signature.t
        ; receiver_evolves : bool
        }
      [@@deriving sexp_of]

      let to_string ~depth { function_signature; receiver_evolves } =
        let receiver_evolves_str = if receiver_evolves then "evolves " else "" in
        mk_on_line
          ~depth
          (receiver_evolves_str ^ Function_signature.to_string function_signature)
      ;;
    end

    type t =
      { name : Ident.t
      ; implements : Path.t list
      ; method_signatures : Method_signature.t Node.t list
      }
    [@@deriving sexp_of]

    let to_string ~depth { name; implements; method_signatures } =
      let method_signatures_string =
        method_signatures
        |> List.map ~f:(Node.data >> Method_signature.to_string ~depth:(depth + 2))
        |> String.concat
      in
      mk_on_line
        ~depth
        [%string
          "interface %{name#Ident} %{Class.implements_to_string \
           implements}{%{method_signatures_string}"]
      ^ mk_on_line ~depth "}\n"
    ;;
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
        { function_signature : Function_signature.t
        ; external_name : string
        }
    | Import of
        { name : Ident.t
        ; file : string
        ; visibility : Visibility.t
        }
    | Interface of Interface.t
  [@@deriving sexp_of]

  and t = decl_kind Node.t

  let create ~decl ~loc = Node.create ~data:decl ~loc

  (** The idea behind printing decls is that a decl should print to some text
      that can stand on it's own, i.e. doesn't expect a caller to insert extra
      indentation or new lines. Then when we indent further we just increase
      the depth parameter and we get out lines that are correctly indented for us. *)
  let rec to_string ?(depth = 0) t =
    let indent = String.make depth ' ' in
    match Node.data t with
    | Constant { ident; value; annot } ->
      (match annot with
       | Some annot ->
         [%string "%{indent}const %{ident#Ident} : %{annot#Type} =  %{value#Expr};"]
       | None -> [%string "%{indent}const %{ident#Ident} =  %{value#Expr};"])
    | Function f -> indent ^ Function.to_string f
    | Class c -> Class.to_string ~depth c
    | Extern_function { function_signature; external_name } ->
      [%string
        "%{indent}extern %{function_signature#Function_signature} = \"%{external_name}\";"]
    | Module { name; decls } ->
      let decl_strings =
        List.map decls ~f:(fun decl -> to_string decl ~depth:(depth + 2))
      in
      [%string
        "%{indent}module %{name#Ident} {\n%{String.concat decl_strings}%{indent}}\n"]
    | Import { name; file; visibility } ->
      [%string
        "%{indent} %{Visibility.to_string ~default:Private visibility}import \
         %{name#Ident} from \"%{file}\""]
    | Interface i -> Interface.to_string ~depth i
  ;;
end
