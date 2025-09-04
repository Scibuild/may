open! Core
open! Composition_infix

(** MEMORY LAYOUT FOR OBJECTS: 

    Arrays: pointer to a slice record:
    x -> | pointer : 64 | -> | the actual data |
         | offset  : 32 |
         | length  : 32 |   Length is relative to the offset

    The slice record is immutable. To change it, it must be cloned. Therefore it *can*
    be aliased, but this is safe by immutability.
  
    Objects: 
    x -> | vtable : 64 | -> | interfaces | -> | interface 1 id  |
         |-------------|    | method 1   |    | interface 1 ptr | -> | method 1 |
         | the fields  |    | ...        |    | ...             |    | method 2 |
         | ...         |    | ...        |                           | ...      |

    This layout is *not* stable! It will likely be changed later. Mark code locations
    that rely on layout with (* LAYOUT *)

*)

module Layout = struct
  let array_pointer_offset = 0
  let array_offset_offset = 8
  let array_length_offset = 12
  let array_slice_size = 16
  let start_of_fields_offset = 8
  let start_of_methods_offset = 8
  let method_width = 8
  let interface_table_entry_size = 16
  let interface_table_entry_id_offset = 0
  let interface_table_entry_ptr_offset = 8
  let class_vtable_offset = 0
  let interface_table_offset_in_vtable = 0
end

module Interface_layout : sig
  type t

  val of_interfaces : Type.Interface.t Env.Id.Interface.Table.t -> t

  val get_method_offset_exn
    :  t
    -> interface_id:Env.Id.Interface.t
    -> method_name:Ast.Ident.t
    -> Env.Id.Interface.t * int

  val get_method_tables_exn
    :  t
    -> interface_id:Env.Id.Interface.t
    -> Ast.Ident.t list Env.Id.Interface.Map.t
end = struct
  module Row = struct
    type t =
      { by_method_name : (Env.Id.Interface.t * int) Ast.Ident.Map.t
      ; method_tables : Ast.Ident.t list Env.Id.Interface.Map.t
      }
    [@@deriving fields]
  end

  type t = Row.t Env.Id.Interface.Table.t

  let of_interfaces (interfaces : Type.Interface.t Type.Interface_id.Table.t) =
    let t = Env.Id.Interface.Table.create () in
    let rec aux id : Row.t =
      Hashtbl.find_or_add t id ~default:(fun () ->
        let interface_sig = Hashtbl.find_exn interfaces id in
        let super_rows =
          List.map interface_sig.implements ~f:(fun super_id -> aux super_id)
        in
        let only_my_method_table = Map.keys interface_sig.method_signatures in
        let my_by_name =
          only_my_method_table
          |> List.mapi ~f:(fun i method_name -> method_name, (id, i))
          |> Ast.Ident.Map.of_alist_exn
        in
        let my_method_tables =
          Env.Id.Interface.Map.of_alist_exn [ id, only_my_method_table ]
        in
        let list_merge ~init ~selector =
          List.fold super_rows ~init ~f:(fun acc super_row ->
            Map.merge_skewed
              (selector super_row)
              acc
              ~combine:(fun ~key:_ _ existing_row -> existing_row))
        in
        Row.
          { by_method_name = list_merge ~init:my_by_name ~selector:Row.by_method_name
          ; method_tables = list_merge ~init:my_method_tables ~selector:Row.method_tables
          })
    in
    Hashtbl.iter_keys interfaces ~f:(fun id -> ignore (aux id));
    t
  ;;

  let get_method_offset_exn (t : t) ~interface_id ~method_name =
    Map.find_exn (Hashtbl.find_exn t interface_id |> Row.by_method_name) method_name
  ;;

  let get_method_tables_exn (t : t) ~interface_id =
    Hashtbl.find_exn t interface_id |> Row.method_tables
  ;;
end

let escaped_string_of_path path =
  path
  |> Ast.Path.to_string ~sep:"_"
  |> String.map ~f:(fun c -> if Char.is_alphanum c then c else '_')
;;

let sym_of_global env global =
  let path = Env.find_global_name env ~id:global in
  "g_" ^ escaped_string_of_path path ^ "_" ^ Resolved_ident.Global.Id.to_string global
;;

let blah_of_class env ~class_id =
  let path = Env.find_class_name env ~id:class_id in
  escaped_string_of_path path
;;

let constructor_of_class env ~class_id = "constructor_" ^ blah_of_class env ~class_id
let evolver_of_class env ~class_id = "evolver_" ^ blah_of_class env ~class_id
let vtable_of_class env ~class_id = "vtable_" ^ blah_of_class env ~class_id

let interface_table_of_class env ~class_id =
  "interface_table_" ^ blah_of_class env ~class_id
;;

let implementation_table_of_class env ~class_id ~interface_id =
  let interface_path = Env.find_interface_name env ~id:interface_id in
  "implementation_table_"
  ^ blah_of_class env ~class_id
  ^ "_"
  ^ escaped_string_of_path interface_path
;;

let function_of_class_method env ~class_id ~method_ =
  "method_" ^ blah_of_class env ~class_id ^ "_" ^ Ast.Ident.to_string method_
;;

module Conversions = struct
  let ty_to_basic_qbe ty =
    match (ty : Type.t) with
    | Bottom | Unit (* -> failwith "unreachable" *) | Bool -> `W
    | Numeric Int -> `L
    | Numeric Float -> `D
    | Numeric Char -> `W
    | Array _ | Fun _ | Top_object | Option _ | Object _ -> `L
  ;;

  let ty_to_abi_qbe ty =
    match (ty : Type.t) with
    (* We can use this to erase Unit types from function returns for example. This might 
       actually be super unsound. 
       
       TODO: think about it. 
       
       Ok thought about it, going to make it have a type*)
    | Bool | Numeric Char -> `UB
    | Bottom | Unit -> `W
    | Numeric Float -> `D
    | Numeric Int | Array _ | Fun _ | Top_object | Option _ | Object _ -> `L
  ;;

  let size_of_ty ty =
    match ty_to_abi_qbe ty with
    | `UB -> 1
    | `W -> 4
    | `D -> 8
    | `L -> 8
  ;;
end

module Statics_compiler = struct
  type t =
    { mutable temp_count : int
    ; globals : Qbe.Data.t Vec.t
    ; folded_globals : Qbe.Const.t Resolved_ident.Global.Id.Table.t
    }

  let create () =
    { temp_count = 0
    ; globals = Vec.create ()
    ; folded_globals = Resolved_ident.Global.Id.Table.create ()
    }
  ;;

  let fresh_name t =
    t.temp_count <- t.temp_count + 1;
    ".static." ^ Int.to_string t.temp_count
  ;;

  let add_array t ~name ~raw ~length =
    let data =
      Qbe.Data.create ~name [ I (`L, `Symbol raw); I (`W, `Int 0); I (`W, `Int length) ]
    in
    Vec.push_back t.globals data
  ;;

  let rec compile_const (t : t) ~name ~(expr : Tast.Expr.t) : Qbe.Const.t =
    let name = Option.value_or_thunk name ~default:(fun () -> fresh_name t) in
    match expr.kind with
    | Null | Unit -> `Int 0
    | Lit_int v -> `Int (Int.of_string v)
    | Lit_bool b -> `Int (Bool.to_int b)
    | Lit_char c -> `Int (Char.to_int c)
    | Lit_string s ->
      let raw_string_name = fresh_name t in
      let raw_string_data =
        Qbe.Data.create ~name:raw_string_name [ I (`B, `String (String.escaped s)) ]
      in
      add_array t ~name ~raw:raw_string_name ~length:(String.length s);
      Vec.push_back t.globals raw_string_data;
      `Symbol name
    | Lit_array elts ->
      let raw_list_name = fresh_name t in
      let raw_list_data =
        elts
        |> List.map ~f:(fun elt ->
          Qbe.Data.Item.I
            ( Conversions.ty_to_basic_qbe elt.ty
            , (compile_const t ~name:None ~expr:elt :> Qbe.Data.Item.Elt.t) ))
        |> Qbe.Data.create ~name
      in
      add_array t ~name ~raw:raw_list_name ~length:(List.length elts);
      Vec.push_back t.globals raw_list_data;
      `Symbol name
    | _ -> failwith "unimplemented"
  ;;

  let save_global (t : t) ~global_id ~const =
    Hashtbl.set t.folded_globals ~key:global_id ~data:const
  ;;

  let resolve_global (t : t) ~env ~global_id =
    global_id
    |> Hashtbl.find t.folded_globals
    |> Option.value_or_thunk ~default:(fun () -> `Symbol (sym_of_global env global_id))
  ;;
end

module Function_compiler = struct
  type t =
    { mutable temp_count : int
    ; class_layout : Class_layout.t
    ; interface_layout : Interface_layout.t
    ; qbe_function : Qbe.Function.t
    ; mutable current_block : Qbe.Block.t
    ; env : Env.t
    ; statics_compiler : Statics_compiler.t
    }

  let qbe_function t = t.qbe_function

  let load_of_ty ty =
    "load"
    ^
    match (ty : Type.t) with
    | Bool | Numeric Char -> "ub"
    | Bottom | Unit -> "uw"
    | Numeric Float -> "d"
    | Numeric Int | Array _ | Fun _ | Top_object | Option _ | Object _ -> "l"
  ;;

  let store_of_ty ty =
    let ty_str = Qbe.Type.to_string (Conversions.ty_to_abi_qbe ty) in
    "store" ^ String.suffix ty_str 1
  ;;

  let%expect_test "load and stores" =
    let tys =
      Type.[ Bool; Numeric Char; Bottom; Unit; Numeric Float; Numeric Int; Top_object ]
    in
    List.map tys ~f:(fun ty ->
      [%sexp
        { ty : Type.t; load : string = load_of_ty ty; store : string = store_of_ty ty }])
    |> Expectable.print;
    [%expect
      {|
      ┌─────────────────┬────────┬────────┐
      │ ty              │ load   │ store  │
      ├─────────────────┼────────┼────────┤
      │ Bool            │ loadub │ storeb │
      │ (Numeric Char)  │ loadub │ storeb │
      │ Bottom          │ loaduw │ storew │
      │ Unit            │ loaduw │ storew │
      │ (Numeric Float) │ loadd  │ stored │
      │ (Numeric Int)   │ loadl  │ storel │
      │ Top_object      │ loadl  │ storel │
      └─────────────────┴────────┴────────┘
      |}]
  ;;

  let reg_of_local local = Resolved_ident.Local.to_mangled_string local

  let create
        ~env
        ~class_layout
        ~interface_layout
        ~name
        ?ret_type
        ~args
        ~linkage
        ~statics_compiler
        ?(with_this = false)
        ()
    =
    let parameters =
      (if with_this then [ "this", `L ] else [])
      @ List.map args ~f:(fun (local, ty) ->
        (* We can't really silently erase the unit type in parameters
        particularly easily, I'm not yet convinced it's totally safe to do so.
        So for now I guess we pass them as just `W, which means when we handle
        passing them later we'll have to do the same. Probably won't be too
        hard, but do need to think. 
        
        Well we aren't doing that anymore. *)
        reg_of_local local, Conversions.ty_to_abi_qbe ty)
    in
    let qbe_function =
      Qbe.Function.create
        ~name
        ?return_type:(Option.map ret_type ~f:Conversions.ty_to_abi_qbe)
        ~parameters
        ~linkage
        ()
    in
    let current_block = Qbe.Function.add_block qbe_function ~name:"start" in
    { temp_count = -1
    ; qbe_function
    ; current_block
    ; env
    ; class_layout
    ; statics_compiler
    ; interface_layout
    }
  ;;

  let fresh_temp t =
    t.temp_count <- t.temp_count + 1;
    "t_" ^ Int.to_string t.temp_count
  ;;

  let this = `Local "this"

  let add_block t ~name =
    t.temp_count <- t.temp_count + 1;
    Qbe.Function.add_block t.qbe_function ~name:[%string "b_%{name}_%{t.temp_count#Int}"]
  ;;

  let enter_block t block = t.current_block <- block

  let addi t ?comment ?ty ?v ~op ?args () =
    Qbe.Block.addi t.current_block ?comment ?ty ?v ~op ?args ()
  ;;

  let addi_temp t ?comment ~ty ~op ?args () =
    let temp = fresh_temp t in
    Qbe.Block.addi t.current_block ?comment ~ty ~v:temp ~op ?args ();
    `Local temp
  ;;

  let add_jmp t ~block =
    Qbe.Block.add_jump t.current_block ~jump:(Jmp (Qbe.Block.name block))
  ;;

  let add_hlt t = Qbe.Block.add_jump t.current_block ~jump:Hlt
  let add_ret_val t value = Qbe.Block.add_jump t.current_block ~jump:(Ret_val value)
  let add_ret t = Qbe.Block.add_jump t.current_block ~jump:Ret

  let add_jnz t ~v ~ifz ~ifnz =
    Qbe.Block.add_jump
      t.current_block
      ~jump:(Qbe.Instr.Jump.jnz ~v ~ifnz:(Qbe.Block.name ifnz) ~ifz:(Qbe.Block.name ifz))
  ;;

  let add_copy t ~dst ~src ~ty = addi t ~ty ~v:dst ~op:"copy" ~args:[ src ] ()

  let call_str ~fn ~args =
    let formatted_args =
      List.map args ~f:(fun (abi_ty, value) ->
        [%string "%{abi_ty#Qbe.Type} %{value#Qbe.Value}"])
      |> String.concat ~sep:", "
    in
    [%string "call %{fn#Qbe.Value}(%{formatted_args})"]
  ;;

  let add_call_void t ~fn ~args = addi t ~op:(call_str ~fn ~args) ~args:[] ()

  let add_call t ~dst ~fn ~args ~ty =
    addi t ~op:([%string "%%{dst} =%{ty#Qbe.Type} "] ^ call_str ~fn ~args) ~args:[] ()
  ;;

  let add_call_temp t ~fn ~args ~ty =
    let dst = fresh_temp t in
    add_call t ~dst ~fn ~args ~ty;
    `Local dst
  ;;

  let add_allocate ?temp t ~size =
    let temp = Option.value_or_thunk temp ~default:(fun () -> fresh_temp t) in
    add_call t ~dst:temp ~ty:`L ~fn:(`Symbol "malloc") ~args:[ `L, size ];
    `Local temp
  ;;

  let add_load_offset t ~ty ~load ~ptr ~off =
    let ptr_off = addi_temp t ~ty:`L ~op:"add" ~args:[ ptr; off ] () in
    addi_temp t ~ty ~op:load ~args:[ ptr_off ] ()
  ;;

  let add_store_offset t ~store ~ptr ~off ~v =
    let ptr_off = addi_temp t ~ty:`L ~op:"add" ~args:[ ptr; off ] () in
    addi t ~op:store ~args:[ v; ptr_off ] ()
  ;;

  let add_panic t ~name ~(range : Range.t) =
    let start_pos, end_pos = range in
    add_call_void
      t
      ~fn:(`Symbol name)
      ~args:
        [ `W, `Int start_pos.pos_lnum
        ; `W, `Int (start_pos.pos_cnum - start_pos.pos_bol)
        ; `W, `Int end_pos.pos_lnum
        ; `W, `Int (end_pos.pos_cnum - end_pos.pos_bol)
        ];
    add_hlt t
  ;;

  let assert_comparison t ~lhs ~op ~rhs ~panic_name ~range =
    let is_good = addi_temp t ~ty:`W ~op ~args:[ lhs; rhs ] () in
    let good_block = add_block t ~name:"assert_ok" in
    let bad_block = add_block t ~name:"assert_fail" in
    add_jnz t ~v:is_good ~ifnz:good_block ~ifz:bad_block;
    enter_block t bad_block;
    add_panic t ~name:panic_name ~range;
    enter_block t good_block
  ;;

  let check_index_in_array_bounds t ~slice ~index ~range =
    (* LAYOUT *)
    let len =
      add_load_offset
        t
        ~ty:`L
        ~load:"loaduw"
        ~ptr:slice
        ~off:(`Int Layout.array_length_offset)
    in
    let index_is_positive = addi_temp t ~ty:`W ~op:"csgel" ~args:[ index; `Int 0 ] () in
    let index_is_in_range = addi_temp t ~ty:`W ~op:"csltl" ~args:[ index; len ] () in
    let index_is_good =
      addi_temp t ~ty:`W ~op:"mul" ~args:[ index_is_positive; index_is_in_range ] ()
    in
    let bad_index_block = add_block t ~name:"bad_index" in
    let good_index_block = add_block t ~name:"good_index" in
    add_jnz t ~v:index_is_good ~ifnz:good_index_block ~ifz:bad_index_block;
    enter_block t bad_index_block;
    add_panic t ~name:"panic_index_out_of_bounds" ~range;
    enter_block t good_index_block
  ;;

  let compile_array_indexing t ~slice ~index ~element_size ~(range : Range.t) =
    (* LAYOUT *)
    check_index_in_array_bounds t ~slice ~index ~range;
    let array_ptr =
      add_load_offset
        t
        ~ty:`L
        ~load:"loadl"
        ~ptr:slice
        ~off:(`Int Layout.array_pointer_offset)
    in
    let offset =
      add_load_offset
        t
        ~ty:`L
        ~load:"loaduw"
        ~ptr:slice
        ~off:(`Int Layout.array_offset_offset)
    in
    let underlying_index = addi_temp t ~ty:`L ~op:"add" ~args:[ index; offset ] () in
    let underlying_offset =
      addi_temp t ~ty:`L ~op:"mul" ~args:[ underlying_index; `Int element_size ] ()
    in
    addi_temp t ~ty:`L ~op:"add" ~args:[ array_ptr; underlying_offset ] ()
  ;;

  let compile_allocate_new_array t ~ptr ~offset ~length =
    (* LAYOUT *)
    let slice = add_allocate t ~size:(`Int Layout.array_slice_size) in
    add_store_offset
      t
      ~store:"storel"
      ~ptr:slice
      ~off:(`Int Layout.array_pointer_offset)
      ~v:ptr;
    add_store_offset
      t
      ~store:"storew"
      ~ptr:slice
      ~off:(`Int Layout.array_offset_offset)
      ~v:offset;
    add_store_offset
      t
      ~store:"storew"
      ~ptr:slice
      ~off:(`Int Layout.array_length_offset)
      ~v:length;
    slice
  ;;

  let add_ext_to_l t ~ty ~src =
    let suffix =
      match (ty : Type.t) with
      | Bool | Numeric Char -> Some "ub"
      | Bottom | Unit -> Some "uw"
      | Numeric Float | Numeric Int | Array _ | Fun _ | Top_object | Option _ | Object _
        -> None
    in
    match suffix with
    | None -> src
    | Some s ->
      addi_temp t ~ty:(Conversions.ty_to_basic_qbe ty) ~op:("ext" ^ s) ~args:[ src ] ()
  ;;

  let rec compile (t : t) ~(expr : Tast.Expr.t) : Qbe.Value.t =
    match expr.kind with
    | Tast.Expr.Null | Tast.Expr.Unit -> `Int 0
    | Tast.Expr.Lit_int v -> `Int (Int.of_string v)
    | Tast.Expr.Lit_string _ ->
      (Statics_compiler.compile_const t.statics_compiler ~name:None ~expr :> Qbe.Value.t)
    | Tast.Expr.Lit_bool b -> `Int (Bool.to_int b)
    | Tast.Expr.Lit_char c -> `Int (Char.to_int c)
    | Tast.Expr.Local local -> `Local (reg_of_local local)
    | Tast.Expr.Global global_id ->
      (Statics_compiler.resolve_global t.statics_compiler ~env:t.env ~global_id
        :> Qbe.Value.t)
    | Tast.Expr.This -> this
    | Tast.Expr.Super -> failwith "unimplemented"
    | Tast.Expr.Bin_op { lhs; rhs; op = (And | Or) as op } ->
      let str =
        match op with
        | And -> "and"
        | Or -> "or"
        | _ -> failwith "unreachable"
      in
      let result = fresh_temp t in
      let lhs_result = compile t ~expr:lhs in
      add_copy t ~ty:`W ~dst:result ~src:lhs_result;
      let check_rhs_block = add_block t ~name:(str ^ "_check_rhs") in
      let result_block = add_block t ~name:(str ^ "_result") in
      let ifnz, ifz =
        match op with
        | And -> check_rhs_block, result_block
        | Or -> result_block, check_rhs_block
        | _ -> failwith "unreachable"
      in
      add_jnz t ~v:lhs_result ~ifnz ~ifz;
      enter_block t check_rhs_block;
      let rhs_result = compile t ~expr:rhs in
      add_copy t ~ty:`W ~dst:result ~src:rhs_result;
      add_jmp t ~block:result_block;
      enter_block t result_block;
      `Local result
    | Tast.Expr.Bin_op { lhs; rhs; op } ->
      let lhs_value = compile t ~expr:lhs in
      let rhs_value = compile t ~expr:rhs in
      let op_ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty lhs) in
      let generate_arith op =
        addi_temp t ~ty:op_ty ~op ~args:[ lhs_value; rhs_value ] ()
      in
      let generate_compare ~sign op =
        let cmp_instr ~ty ~op =
          let prefix =
            match sign with
            | false -> "c"
            | true ->
              (match ty with
               | `W | `L -> "cs"
               | `S | `D -> "c")
          in
          prefix ^ op ^ Qbe.Type.to_string ty
        in
        addi_temp t ~ty:`W ~op:(cmp_instr ~ty:op_ty ~op) ~args:[ lhs_value; rhs_value ] ()
      in
      (match op with
       | Add -> generate_arith "add"
       | Sub -> generate_arith "sub"
       | Div -> generate_arith "div"
       | Mul -> generate_arith "mul"
       | Gt -> generate_compare ~sign:true "gt"
       | Ge -> generate_compare ~sign:true "ge"
       | Lt -> generate_compare ~sign:true "lt"
       | Le -> generate_compare ~sign:true "le"
       | Eq -> generate_compare ~sign:false "eq"
       | Ne -> generate_compare ~sign:false "ne"
       | And | Or -> failwith "unreachable")
    | Tast.Expr.Un_op { rhs; op = Neg } ->
      let rhs_value = compile t ~expr:rhs in
      let op_ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty rhs) in
      addi_temp t ~ty:op_ty ~op:"neg" ~args:[ rhs_value ] ()
    | Tast.Expr.Un_op { rhs; op = Not } ->
      let rhs_value = compile t ~expr:rhs in
      addi_temp t ~ty:`W ~op:"sub" ~args:[ `Int 1; rhs_value ] ()
    | Tast.Expr.If { cond; if_then; if_else } ->
      let cond_value = compile t ~expr:cond in
      let if_then_block = add_block t ~name:"if_then" in
      let if_else_block = add_block t ~name:"if_else" in
      add_jnz t ~v:cond_value ~ifnz:if_then_block ~ifz:if_else_block;
      let result = fresh_temp t in
      let result_ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty expr) in
      enter_block t if_then_block;
      let if_then_value = compile t ~expr:if_then in
      add_copy t ~ty:result_ty ~dst:result ~src:if_then_value;
      let result_block = add_block t ~name:"if_result" in
      add_jmp t ~block:result_block;
      enter_block t if_else_block;
      let if_else_value = compile t ~expr:if_else in
      add_copy t ~ty:result_ty ~dst:result ~src:if_else_value;
      add_jmp t ~block:result_block;
      enter_block t result_block;
      `Local result
    | Tast.Expr.While { cond; block } ->
      let cond_block = add_block t ~name:"while_condition" in
      add_jmp t ~block:cond_block;
      enter_block t cond_block;
      let cond_value = compile t ~expr:cond in
      let while_block = add_block t ~name:"while_block" in
      let after_while_block = add_block t ~name:"after_while_block" in
      add_jnz t ~v:cond_value ~ifnz:while_block ~ifz:after_while_block;
      enter_block t while_block;
      let _ = compile t ~expr:block in
      add_jmp t ~block:cond_block;
      enter_block t after_while_block;
      `Int 0
    | Tast.Expr.Block exprs ->
      List.fold exprs ~init:(`Int 0) ~f:(fun _ expr -> compile t ~expr)
    | Tast.Expr.Let { local; expr } ->
      let expr_value = compile t ~expr in
      let ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty expr) in
      add_copy t ~ty ~dst:(reg_of_local local) ~src:expr_value;
      `Int 0
    | Tast.Expr.Assign { lhs; rhs } ->
      let rvalue = compile t ~expr:rhs in
      compile_assignment_to t ~lhs ~rvalue;
      `Int 0
    | Tast.Expr.Return expr ->
      (match Tast.Expr.ty expr with
       | Type.Unit -> add_ret t
       | _ ->
         let value = compile t ~expr in
         add_ret_val t value);
      `Int 0
    | Tast.Expr.Function_call { expr = f; arguments } ->
      let f_value = compile t ~expr:f in
      let argument_values = compile_function_arguments t arguments in
      let ty = expr |> Tast.Expr.ty |> Conversions.ty_to_abi_qbe in
      add_call_temp t ~fn:f_value ~ty ~args:argument_values
    | Tast.Expr.Field_subscript { expr = sub_expr; field; class_id } ->
      (* LAYOUT *)
      let obj_value = compile t ~expr:sub_expr in
      let offset = Class_layout.field_offset t.class_layout ~class_id ~field in
      let ty = Tast.Expr.ty expr in
      add_load_offset
        t
        ~ty:(Conversions.ty_to_basic_qbe ty)
        ~load:(load_of_ty ty)
        ~ptr:obj_value
        ~off:(`Int (Layout.start_of_fields_offset + offset))
    | Tast.Expr.Array_subscript { expr = sub_expr; index } ->
      (* LAYOUT *)
      let array_ptr_ref = compile t ~expr:sub_expr in
      let index_value =
        add_ext_to_l t ~ty:(Tast.Expr.ty index) ~src:(compile t ~expr:index)
      in
      let ty = Tast.Expr.ty expr in
      let elem_ptr =
        compile_array_indexing
          t
          ~slice:array_ptr_ref
          ~index:index_value
          ~element_size:(Conversions.size_of_ty ty)
          ~range:(Tast.Expr.range index)
      in
      addi_temp
        t
        ~ty:(Conversions.ty_to_basic_qbe ty)
        ~op:(load_of_ty ty)
        ~args:[ elem_ptr ]
        ()
    | Tast.Expr.Array_subrange { expr = sub_expr; from; to_ } ->
      (* LAYOUT *)
      let array_ptr_ref = compile t ~expr:sub_expr in
      let from_value = compile t ~expr:from in
      let to_value = compile t ~expr:to_ in
      check_index_in_array_bounds
        t
        ~slice:array_ptr_ref
        ~index:from_value
        ~range:(Tast.Expr.range from);
      check_index_in_array_bounds
        t
        ~slice:array_ptr_ref
        ~index:to_value
        ~range:(Tast.Expr.range to_);
      assert_comparison
        t
        ~lhs:from_value
        ~op:"cslel"
        ~rhs:to_value
        ~range:(Tast.Expr.range to_)
        ~panic_name:"panic_subrange_invalid";
      let array_ptr =
        add_load_offset
          t
          ~ty:`L
          ~load:"loadl"
          ~ptr:array_ptr_ref
          ~off:(`Int Layout.array_pointer_offset)
      in
      let offset =
        add_load_offset
          t
          ~ty:`W
          ~load:"loadw"
          ~ptr:array_ptr_ref
          ~off:(`Int Layout.array_offset_offset)
      in
      let new_offset = addi_temp t ~ty:`W ~op:"add" ~args:[ from_value; offset ] () in
      let new_length_minus_one =
        addi_temp t ~ty:`W ~op:"sub" ~args:[ to_value; from_value ] ()
      in
      let new_length =
        addi_temp t ~ty:`W ~op:"add" ~args:[ new_length_minus_one; `Int 1 ] ()
      in
      compile_allocate_new_array t ~ptr:array_ptr ~offset:new_offset ~length:new_length
    | Tast.Expr.Lit_array elements ->
      let len = List.length elements in
      let elem_ty =
        match Tast.Expr.ty expr with
        | Array { elt; mut = _ } -> elt
        | _ -> failwith "unreachable"
      in
      let size = Conversions.size_of_ty elem_ty in
      (* LAYOUT *)
      let slice = add_allocate t ~size:(`Int Layout.array_slice_size) in
      let array = add_allocate t ~size:(`Int (size * len)) in
      add_store_offset
        t
        ~store:"storel"
        ~ptr:slice
        ~off:(`Int Layout.array_pointer_offset)
        ~v:array;
      add_store_offset
        t
        ~store:"storew"
        ~ptr:slice
        ~off:(`Int Layout.array_offset_offset)
        ~v:(`Int 0);
      add_store_offset
        t
        ~store:"storew"
        ~ptr:slice
        ~off:(`Int Layout.array_length_offset)
        ~v:(`Int len);
      List.iteri elements ~f:(fun i elt ->
        let value = compile t ~expr:elt in
        add_store_offset
          t
          ~store:(store_of_ty elem_ty)
          ~ptr:array
          ~off:(`Int (i * size))
          ~v:value);
      slice
    | Tast.Expr.Method_call { expr; method_; arguments; obj_kind } ->
      (* LAYOUT *)
      let obj_value = compile t ~expr in
      let argument_values = compile_function_arguments t arguments in
      let ty = expr |> Tast.Expr.ty |> Conversions.ty_to_abi_qbe in
      let vtable = addi_temp t ~ty:`L ~op:"loadl" ~args:[ obj_value ] () in
      let method_value =
        match obj_kind with
        | Class class_id ->
          let vtable_offset =
            Layout.start_of_methods_offset
            + (Layout.method_width
               * Class_layout.method_offset t.class_layout ~class_id ~method_)
          in
          add_load_offset t ~ty:`L ~load:"loadl" ~ptr:vtable ~off:(`Int vtable_offset)
        | Interface interface_id ->
          let precise_interface_id, method_index =
            Interface_layout.get_method_offset_exn
              t.interface_layout
              ~interface_id
              ~method_name:method_
          in
          let interface_table =
            add_load_offset
              t
              ~ty:`L
              ~load:"loadl"
              ~ptr:vtable
              ~off:(`Int Layout.interface_table_offset_in_vtable)
          in
          let interface_table_reg = fresh_temp t in
          addi t ~ty:`L ~v:interface_table_reg ~op:"copy" ~args:[ interface_table ] ();
          let interface_table = `Local interface_table_reg in
          let loop_block = add_block t ~name:"interface_load" in
          add_jmp t ~block:loop_block;
          enter_block t loop_block;
          let id_in_table =
            add_load_offset
              t
              ~ty:`L
              ~load:"loadl"
              ~ptr:interface_table
              ~off:(`Int Layout.interface_table_entry_id_offset)
          in
          let is_interface_id =
            addi_temp
              t
              ~ty:`W
              ~op:"ceql"
              ~args:
                [ id_in_table; `Int (Type.Interface_id.to_int_exn precise_interface_id) ]
              ()
          in
          let after_loop_block = add_block t ~name:"interface_found" in
          let increment_block = add_block t ~name:"interface_load_incr" in
          add_jnz t ~v:is_interface_id ~ifz:increment_block ~ifnz:after_loop_block;
          enter_block t increment_block;
          addi
            t
            ~ty:`L
            ~v:interface_table_reg
            ~op:"add"
            ~args:[ interface_table; `Int Layout.interface_table_entry_size ]
            ();
          add_jmp t ~block:loop_block;
          enter_block t after_loop_block;
          let implementation_table =
            add_load_offset
              t
              ~ty:`L
              ~load:"loadl"
              ~ptr:interface_table
              ~off:(`Int Layout.interface_table_entry_ptr_offset)
          in
          add_load_offset
            t
            ~ty:`L
            ~load:"loadl"
            ~ptr:implementation_table
            ~off:(`Int (Layout.method_width * method_index))
      in
      add_call_temp t ~fn:method_value ~ty ~args:([ `L, obj_value ] @ argument_values)
    | Tast.Expr.New { class_id; arguments } ->
      let argument_values = compile_function_arguments t arguments in
      let size = Class_layout.max_fields_size t.class_layout ~class_id in
      (* LAYOUT *)
      let object_value =
        add_allocate t ~size:(`Int (size + Layout.start_of_fields_offset))
      in
      (* Null initialise the vtable for safety vibes *)
      add_store_offset
        t
        ~store:"storel"
        ~ptr:object_value
        ~off:(`Int Layout.class_vtable_offset)
        ~v:(`Int 0);
      let constructor_value = `Symbol (constructor_of_class t.env ~class_id) in
      add_call_void t ~fn:constructor_value ~args:([ `L, object_value ] @ argument_values);
      object_value
    | Tast.Expr.Update_this_vtable_after { expr; new_table } ->
      (* LAYOUT *)
      let expr_value = compile t ~expr in
      let vtable_value = `Symbol (vtable_of_class t.env ~class_id:new_table) in
      add_store_offset
        t
        ~store:"storel"
        ~ptr:this
        ~off:(`Int Layout.class_vtable_offset)
        ~v:vtable_value;
      expr_value
    | Tast.Expr.Evolves { expr; class_id; arguments } ->
      let expr_value = compile t ~expr in
      let argument_values = compile_function_arguments t arguments in
      let evolver = `Symbol (evolver_of_class t.env ~class_id) in
      add_call_temp t ~ty:`L ~fn:evolver ~args:([ `L, expr_value ] @ argument_values)
    | Tast.Expr.Super_call { super_id; arguments } ->
      let argument_values = compile_function_arguments t arguments in
      let super_constructor = `Symbol (constructor_of_class t.env ~class_id:super_id) in
      let ty = expr |> Tast.Expr.ty |> Conversions.ty_to_abi_qbe in
      add_call_temp t ~ty ~fn:super_constructor ~args:([ `L, this ] @ argument_values)
    | Tast.Expr.Super_method_call { super_id; arguments; method_ } ->
      let argument_values = compile_function_arguments t arguments in
      let super_method =
        `Symbol (function_of_class_method t.env ~class_id:super_id ~method_)
      in
      add_call_void t ~fn:super_method ~args:([ `L, this ] @ argument_values);
      `Int 0
    | Tast.Expr.De_null { lhs } ->
      let value = compile t ~expr:lhs in
      (* SAFETY_CHECK *)
      let good_block = add_block t ~name:"is_non_null" in
      let bad_block = add_block t ~name:"is_null" in
      add_jnz t ~v:value ~ifz:bad_block ~ifnz:good_block;
      enter_block t bad_block;
      add_panic t ~name:"panic_unexpected_option_is_null" ~range:(Tast.Expr.range expr);
      enter_block t good_block;
      value
    | Tast.Expr.If_option { expr = cond; var; if_value; if_null } ->
      (* compile the condition *)
      let value = compile t ~expr:cond in
      let if_value_block = add_block t ~name:"if_value" in
      let if_null_block = add_block t ~name:"if_null" in
      let result_block = add_block t ~name:"after_ifq" in
      (* check if the condition is null *)
      add_jnz t ~v:value ~ifz:if_null_block ~ifnz:if_value_block;
      (* if not null we bind it to the local *)
      enter_block t if_value_block;
      let ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty cond) in
      add_copy t ~ty ~dst:(reg_of_local var) ~src:value;
      let if_value_result = compile t ~expr:if_value in
      (* copy the value into a register and jump to the main control flow *)
      let result = fresh_temp t in
      let result_ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty expr) in
      add_copy t ~ty:result_ty ~dst:result ~src:if_value_result;
      add_jmp t ~block:result_block;
      (* same for if null but without the variable binding *)
      enter_block t if_null_block;
      let if_null_result = compile t ~expr:if_null in
      add_copy t ~ty:result_ty ~dst:result ~src:if_null_result;
      add_jmp t ~block:result_block;
      enter_block t result_block;
      `Local result
    | Tast.Expr.Or_else { lhs; or_else } ->
      let value = compile t ~expr:lhs in
      let value_reg = fresh_temp t in
      let ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty lhs) in
      add_copy t ~dst:value_reg ~ty ~src:value;
      let or_ok_block = add_block t ~name:"or_ok" in
      let or_else_block = add_block t ~name:"or_else" in
      add_jnz t ~v:value ~ifnz:or_ok_block ~ifz:or_else_block;
      enter_block t or_else_block;
      let or_else_value = compile t ~expr:or_else in
      add_copy t ~dst:value_reg ~ty ~src:or_else_value;
      add_jmp t ~block:or_ok_block;
      enter_block t or_ok_block;
      `Local value_reg
    | Tast.Expr.New_array { size; init } ->
      (* LAYOUT *)
      let size_value =
        add_ext_to_l t ~ty:(Tast.Expr.ty size) ~src:(compile t ~expr:size)
      in
      let size_is_negative =
        addi_temp t ~ty:`W ~op:"csltl" ~args:[ size_value; `Int 0 ] ()
      in
      let negative_size_block = add_block t ~name:"negative_array_size" in
      let non_negative_size_block = add_block t ~name:"non_negative_array_size" in
      add_jnz t ~v:size_is_negative ~ifnz:negative_size_block ~ifz:non_negative_size_block;
      enter_block t negative_size_block;
      add_panic
        t
        ~name:"panic_cannot_allocate_negative_array_size"
        ~range:(Tast.Expr.range size);
      add_hlt t;
      enter_block t non_negative_size_block;
      let zero_size_block = add_block t ~name:"zero_array_size" in
      let positive_size_block = add_block t ~name:"positive_array_size" in
      add_jnz t ~v:size_value ~ifnz:positive_size_block ~ifz:zero_size_block;
      let init_exit_block = add_block t ~name:"init_loop_exit" in
      let return_slice = fresh_temp t in
      enter_block t zero_size_block;
      let sliceZero =
        compile_allocate_new_array t ~ptr:(`Int 0) ~offset:(`Int 0) ~length:(`Int 0)
      in
      add_copy t ~ty:`L ~dst:return_slice ~src:sliceZero;
      add_jmp t ~block:init_exit_block;
      enter_block t positive_size_block;
      let size_value_reg = fresh_temp t in
      add_copy t ~dst:size_value_reg ~src:size_value ~ty:`L;
      let elt_ty =
        match Tast.Expr.ty expr with
        | Array { mut = _; elt = ty } -> ty
        | _ -> failwith "unreachable"
      in
      let elt_size = Conversions.size_of_ty elt_ty in
      let array_buf_size =
        addi_temp t ~ty:`L ~op:"mul" ~args:[ size_value; `Int elt_size ] ()
      in
      let array_buf_reg = fresh_temp t in
      let array_buf = add_allocate ~temp:array_buf_reg t ~size:array_buf_size in
      let slice =
        compile_allocate_new_array t ~ptr:array_buf ~offset:(`Int 0) ~length:size_value
      in
      let loop_start_block = add_block t ~name:"array_init_loop_begin" in
      let loop_exit_block = add_block t ~name:"array_init_loop_exit" in
      let init_value = compile t ~expr:init in
      add_jmp t ~block:loop_start_block;
      enter_block t loop_start_block;
      add_store_offset
        t
        ~store:(store_of_ty elt_ty)
        ~ptr:array_buf
        ~off:(`Int 0)
        ~v:init_value;
      addi
        t
        ~ty:`L
        ~v:array_buf_reg
        ~op:"add"
        ~args:[ `Local array_buf_reg; `Int elt_size ]
        ();
      addi t ~ty:`L ~v:size_value_reg ~op:"sub" ~args:[ `Local size_value_reg; `Int 1 ] ();
      add_jnz t ~v:(`Local size_value_reg) ~ifz:loop_exit_block ~ifnz:loop_start_block;
      enter_block t loop_exit_block;
      add_copy t ~ty:`L ~dst:return_slice ~src:slice;
      add_jmp t ~block:init_exit_block;
      enter_block t init_exit_block;
      `Local return_slice
    | Tast.Expr.Exchange { lhs; rhs } ->
      let load_lhs, store_lhs = compile_load_store_pair t ~lhs in
      let load_rhs, store_rhs = compile_load_store_pair t ~lhs:rhs in
      let lhs_value = load_lhs () in
      let rhs_value = load_rhs () in
      store_rhs lhs_value;
      store_lhs rhs_value;
      `Int 0
    | Tast.Expr.Array_length { expr } ->
      let value = compile t ~expr in
      add_load_offset
        t
        ~ty:`L
        ~load:"loadw"
        ~ptr:value
        ~off:(`Int Layout.array_length_offset)

  and compile_assignment_to t ~lhs ~rvalue =
    let _, store = compile_load_store_pair t ~lhs in
    store rvalue

  and compile_load_store_pair t ~(lhs : Tast.Expr.t) =
    match Tast.Expr.kind lhs with
    | Local local ->
      let ty = Conversions.ty_to_basic_qbe (Tast.Expr.ty lhs) in
      ( (fun () -> addi_temp t ~ty ~op:"copy" ~args:[ `Local (reg_of_local local) ] ())
      , fun rvalue -> add_copy t ~ty ~dst:(reg_of_local local) ~src:rvalue )
    | Tast.Expr.Array_subscript { expr = sub_expr; index } ->
      let array_ptr_ref = compile t ~expr:sub_expr in
      let index_value = compile t ~expr:index in
      let ty = Tast.Expr.ty lhs in
      let elem_ptr =
        compile_array_indexing
          t
          ~slice:array_ptr_ref
          ~index:index_value
          ~element_size:(Conversions.size_of_ty ty)
          ~range:(Tast.Expr.range index)
      in
      ( (fun () ->
          add_load_offset
            t
            ~load:(load_of_ty ty)
            ~ty:(Conversions.ty_to_basic_qbe ty)
            ~ptr:elem_ptr
            ~off:(`Int 0))
      , fun rvalue ->
          add_store_offset t ~store:(store_of_ty ty) ~ptr:elem_ptr ~off:(`Int 0) ~v:rvalue
      )
    | Tast.Expr.Field_subscript { expr = sub_expr; field; class_id } ->
      let obj_value = compile t ~expr:sub_expr in
      let offset = Class_layout.field_offset t.class_layout ~class_id ~field in
      let ty = Tast.Expr.ty lhs in
      ( (fun () ->
          add_load_offset
            t
            ~ty:(Conversions.ty_to_basic_qbe ty)
            ~load:(load_of_ty ty)
            ~ptr:obj_value
            ~off:(`Int (8 (* vtable *) + offset)))
      , fun rvalue ->
          add_store_offset
            t
            ~store:(store_of_ty ty)
            ~ptr:obj_value
            ~off:(`Int (8 (* vtable *) + offset))
            ~v:rvalue )
    | Tast.Expr.Lit_int _
    | Tast.Expr.Lit_string _
    | Tast.Expr.Lit_bool _
    | Tast.Expr.Lit_char _
    | Tast.Expr.Lit_array _
    | Tast.Expr.Global _
    | Tast.Expr.This
    | Tast.Expr.Super
    | Tast.Expr.Bin_op _
    | Tast.Expr.Un_op _
    | Tast.Expr.If _
    | Tast.Expr.Unit
    | Tast.Expr.While _
    | Tast.Expr.Block _
    | Tast.Expr.Let _
    | Tast.Expr.Assign _
    | Tast.Expr.Function_call _
    | Tast.Expr.Method_call _
    | Tast.Expr.New _
    | Tast.Expr.Evolves _
    | Tast.Expr.Super_call _
    | Tast.Expr.Super_method_call _
    | Tast.Expr.Return _
    | Tast.Expr.Update_this_vtable_after _
    | Tast.Expr.Null
    | Tast.Expr.If_option _
    | Tast.Expr.Or_else _
    | Tast.Expr.De_null _
    | Tast.Expr.New_array _
    | Tast.Expr.Exchange _
    | Tast.Expr.Array_length _
    | Tast.Expr.Array_subrange _ -> failwith "unreachable"

  and compile_function_arguments t arguments =
    List.map arguments ~f:(fun expr ->
      let abi_ty = expr |> Tast.Expr.ty |> Conversions.ty_to_abi_qbe in
      let value = compile t ~expr in
      abi_ty, value)
  ;;
end

module Compilation_unit = struct
  type t =
    { globals : Qbe.Decl.t Vec.t
    ; env : (Env.t[@sexp.opaque])
    ; methods : string array Type.Class_id.Table.t
    ; class_layout : (Class_layout.t[@sexp.opaque])
    ; interface_layout : (Interface_layout.t[@sexp.opaque])
    ; statics_compiler : (Statics_compiler.t[@sexp.opaque])
      (* ; interfaces : Type.Interface.t Type.Interface_id.Table.t *)
    }

  let to_bytes t =
    let b = Bigbuffer.create 4096 in
    Vec.iter t.statics_compiler.globals ~f:(Fn.flip Qbe.Data.add_to_buffer b);
    Vec.iter t.globals ~f:(Fn.flip Qbe.Decl.add_to_buffer b);
    (* Hashtbl.iter_keys t.constructors ~f:(fun class_id ->
      Hashtbl.find t.constructors class_id
      |> Option.iter ~f:(Fn.flip Qbe.Function.add_to_buffer b);
      Hashtbl.find t.evolvers class_id
      |> Option.iter ~f:(Fn.flip Qbe.Function.add_to_buffer b);
      Hashtbl.find t.methods class_id
      |> Option.iter ~f:(fun (methods, method_table, _) ->
        List.iter methods ~f:(Fn.flip Qbe.Function.add_to_buffer b);
        Qbe.Data.add_to_buffer method_table b));
    *)
    Bigbuffer.contents_bytes b
  ;;

  let compile_constant t Tast.Decl.Const.{ id; expr; range = _ } =
    let name = sym_of_global t.env id in
    let const =
      Statics_compiler.compile_const t.statics_compiler ~name:(Some name) ~expr
    in
    Statics_compiler.save_global t.statics_compiler ~global_id:id ~const
  ;;

  let add_function_compiler t ~function_compiler =
    Vec.push_back
      t.globals
      (Qbe.Decl.Function (Function_compiler.qbe_function function_compiler))
  ;;

  let compile_function t Tast.Decl.Function.{ id; args; body; ret_type; range = _ } =
    let name = sym_of_global t.env id in
    let function_compiler =
      Function_compiler.create
        ~statics_compiler:t.statics_compiler
        ~env:t.env
        ~class_layout:t.class_layout
        ~interface_layout:t.interface_layout
        ~name
        ~ret_type
        ~args
        ~linkage:[]
        ()
    in
    let return_value = Function_compiler.compile function_compiler ~expr:body in
    Function_compiler.add_ret_val function_compiler return_value;
    add_function_compiler t ~function_compiler
  ;;

  let compile_extern_function
        t
        Tast.Decl.Extern_function.{ id; arg_tys; ret_type; external_name; range = _ }
    =
    let name = sym_of_global t.env id in
    (* TODO: this args business is messy *)
    let args =
      List.mapi arg_tys ~f:(fun id arg_ty ->
        Resolved_ident.Local.create ~name:(Ast.Ident.of_string "extern") ~id, arg_ty)
    in
    let extern_args =
      List.map args ~f:(fun (resolved_local, ty) ->
        ( Conversions.ty_to_abi_qbe ty
        , `Local (Function_compiler.reg_of_local resolved_local) ))
    in
    let function_compiler =
      Function_compiler.create
        ~statics_compiler:t.statics_compiler
        ~env:t.env
        ~class_layout:t.class_layout
        ~interface_layout:t.interface_layout
        ~name
        ~ret_type
        ~args
        ~linkage:[]
        ()
    in
    let return_value =
      Function_compiler.add_call_temp
        function_compiler
        ~fn:(`Symbol external_name)
        ~args:extern_args
        ~ty:(Conversions.ty_to_abi_qbe ret_type)
    in
    Function_compiler.add_ret_val function_compiler return_value;
    add_function_compiler t ~function_compiler
  ;;

  let compile_constructor
        t
        ~class_id
        Tast.Decl.Class.Constructor.{ args; body; range = _ }
    =
    let name = constructor_of_class t.env ~class_id in
    let function_compiler =
      Function_compiler.create
        ~statics_compiler:t.statics_compiler
        ~env:t.env
        ~class_layout:t.class_layout
        ~interface_layout:t.interface_layout
        ~name
        ~args
        ~linkage:[]
        ~with_this:true
        ()
    in
    let _ = Function_compiler.compile function_compiler ~expr:body in
    Function_compiler.add_ret function_compiler;
    add_function_compiler t ~function_compiler
  ;;

  let compile_evolver
        t
        ~class_id
        ~super_id
        Tast.Decl.Class.Constructor.{ args; body; range = _ }
    =
    let name = evolver_of_class t.env ~class_id in
    let function_compiler =
      Function_compiler.create
        ~statics_compiler:t.statics_compiler
        ~env:t.env
        ~class_layout:t.class_layout
        ~interface_layout:t.interface_layout
        ~name
        ~args
        ~linkage:[]
        ~ret_type:(Option (Object (Owned, Class class_id)))
        ~with_this:true
        ()
    in
    let this_vtable =
      Function_compiler.add_load_offset
        function_compiler
        ~load:"loadl"
        ~ty:`L
        ~ptr:Function_compiler.this
        ~off:(`Int 0)
    in
    let super_vtable = `Symbol (vtable_of_class t.env ~class_id:super_id) in
    let is_equal =
      Function_compiler.addi_temp
        function_compiler
        ~ty:`W
        ~op:"ceql"
        ~args:[ this_vtable; super_vtable ]
        ()
    in
    let is_super_block = Function_compiler.add_block function_compiler ~name:"is_super" in
    let not_super_block =
      Function_compiler.add_block function_compiler ~name:"not_super"
    in
    Function_compiler.add_jnz
      function_compiler
      ~v:is_equal
      ~ifnz:is_super_block
      ~ifz:not_super_block;
    Function_compiler.enter_block function_compiler not_super_block;
    Function_compiler.add_ret_val function_compiler (`Int 0);
    Function_compiler.enter_block function_compiler is_super_block;
    let _ = Function_compiler.compile function_compiler ~expr:body in
    Function_compiler.add_ret_val function_compiler Function_compiler.this;
    add_function_compiler t ~function_compiler
  ;;

  let compile_method
        t
        ~class_id
        Tast.Decl.Class.Method.
          { visibility = _; name; args; ret_type; body; overrides = _; range = _ }
    =
    let name = function_of_class_method t.env ~class_id ~method_:name in
    let function_compiler =
      Function_compiler.create
        ~statics_compiler:t.statics_compiler
        ~env:t.env
        ~class_layout:t.class_layout
        ~interface_layout:t.interface_layout
        ~name
        ~args
        ~ret_type
        ~linkage:[]
        ~with_this:true
        ()
    in
    let value = Function_compiler.compile function_compiler ~expr:body in
    Function_compiler.add_ret_val function_compiler value;
    add_function_compiler t ~function_compiler
  ;;

  let compile_class
        t
        Tast.Decl.Class.
          { id
          ; super_type
          ; implements
          ; methods
          ; fields = _
          ; constructor
          ; evolver
          ; range = _
          }
    =
    Option.iter constructor ~f:(compile_constructor t ~class_id:id);
    Option.iter evolver ~f:(fun evolver ->
      compile_evolver t ~class_id:id ~super_id:(Option.value_exn super_type) evolver);
    List.iter methods ~f:(fun m -> compile_method t ~class_id:id m);
    let opt_array =
      Array.create ~len:(Class_layout.methods_size t.class_layout ~class_id:id) None
    in
    Option.iter super_type ~f:(fun super_id ->
      let super_vtable =
        Hashtbl.find_exn t.methods super_id |> Array.map ~f:Option.some
      in
      Array.blito ~src:super_vtable ~dst:opt_array ());
    List.iter methods ~f:(fun m ->
      Array.set
        opt_array
        (Class_layout.method_offset t.class_layout ~class_id:id ~method_:m.name)
        (Some (function_of_class_method t.env ~class_id:id ~method_:m.name)));
    let vtable_entries = Array.map opt_array ~f:(fun s -> Option.value_exn s) in
    let interface_method_tables =
      List.fold implements ~init:Env.Id.Interface.Map.empty ~f:(fun acc interface_id ->
        Map.merge_skewed
          (Interface_layout.get_method_tables_exn t.interface_layout ~interface_id)
          acc
          ~combine:(fun ~key:_ _ existing_row -> existing_row))
    in
    let interface_table_entries =
      Map.mapi interface_method_tables ~f:(fun ~key:interface_id ~data:method_list ->
        let method_table_data_items =
          List.map method_list ~f:(fun method_name ->
            let method_index =
              Class_layout.method_offset t.class_layout ~class_id:id ~method_:method_name
            in
            let method_symbol = Array.get vtable_entries method_index in
            Qbe.Data.Item.I (`L, `Symbol method_symbol))
        in
        Qbe.Data.create
          ~name:(implementation_table_of_class t.env ~class_id:id ~interface_id)
          method_table_data_items)
      |> Map.to_alist
    in
    let interface_table =
      let contents =
        interface_table_entries
        |> List.concat_map ~f:(fun (interface_id, data) ->
          Qbe.Data.Item.
            [ I (`L, `Int (Env.Id.Interface.to_int_exn interface_id))
            ; I (`L, `Symbol (Qbe.Data.name data))
            ])
      in
      Qbe.Data.create
        ~name:(interface_table_of_class t.env ~class_id:id)
        (contents @ Qbe.Data.Item.[ Pad 16 ])
    in
    Hashtbl.add_exn t.methods ~key:id ~data:vtable_entries;
    let vtable_data =
      let contents =
        vtable_entries
        |> Array.map ~f:(fun s -> Qbe.Data.Item.I (`L, `Symbol s))
        |> Array.to_list
      in
      let interface_table_entry =
        match List.is_empty implements with
        | true -> Qbe.Data.Item.I (`L, `Int 0)
        | false -> Qbe.Data.Item.I (`L, `Symbol (Qbe.Data.name interface_table))
      in
      Qbe.Data.create
        ~name:(vtable_of_class t.env ~class_id:id)
        (interface_table_entry :: contents)
    in
    Vec.push_back t.globals (Qbe.Decl.Data vtable_data);
    match List.is_empty implements with
    | true -> ()
    | false ->
      List.iter interface_table_entries ~f:(fun (_, data) ->
        Vec.push_back t.globals (Qbe.Decl.Data data));
      Vec.push_back t.globals (Qbe.Decl.Data interface_table)
  ;;

  let compile_program ~check ~(decls : Tast.Decls.t) =
    let Tast.Decls.{ constants; functions; classes; extern_functions; interfaces = _ } =
      decls
    in
    let env = Check.env check in
    let class_layout =
      Class_layout.of_signatures ~sizeof:Conversions.size_of_ty (Check.class_table check)
    in
    let interface_layout = Interface_layout.of_interfaces (Check.interface_table check) in
    let t =
      { globals = Vec.create ()
      ; env
      ; class_layout
      ; interface_layout
      ; methods = Type.Class_id.Table.create ()
      ; statics_compiler = Statics_compiler.create ()
      }
    in
    List.iter constants ~f:(compile_constant t);
    List.iter extern_functions ~f:(compile_extern_function t);
    List.iter functions ~f:(compile_function t);
    List.iter classes ~f:(compile_class t);
    let main_id =
      match Env.find_id env ~scope:[] ~qualified_name:(Ast.Path.of_string "main") with
      | Some (Global global_id) -> Some (sym_of_global env global_id)
      | None | Some _ -> None
    in
    t, main_id
  ;;
end
