open! Core

module Instruction = struct
  type t =
    | Return
    | Call (* arg1, arg2 ... argN function -- return_value *)
    | Add of Type.Numeric.t
    | Sub of Type.Numeric.t
    | Mul of Type.Numeric.t
    | Div of Type.Numeric.t
    | Cmp_gt of Type.Numeric.t
    | Cmp_ge of Type.Numeric.t
    | Not
    | Neg of Type.Numeric.t
    | Cmp_eq
    | Const_0 (* False *)
    | Const_1 (* True *)
    | Const_int of int
    | Const_float of float
    | Get_local of int (* Index onto the local stack relative to the current frame *)
    | Set_local of int
    | Get_global of Resolved_ident.Global.Id.t
    | Set_global of Resolved_ident.Global.Id.t
    | Set_array (* value array index -- *)
    | Get_array (* array index -- value *)
    | New_array (* size default -- array*)
    | Array_of_stack (* elt0 ... eltN-1 N -- array *)
    | Branch of int (* Index to the instruction for the particular function. *)
    | Branch_if_true of int
    | Branch_if_false of int
    | Branch_if_vtable_equals of int * Type.Class_id.t
    | Error_undefined
      (* Used to fill in spots where we don't know what the instruction that's going to go there is. *)
    | Pop (* Pops a value off the value stack *)
    | Get_external of int
    | Get_field of int (* object -- value *)
    | Set_field of int (* value object -- *)
    | Lookup_method of int
    | Lookup_method_static of int * Type.Class_id.t
    | Update_this_vtable of Type.Class_id.t
    | Get_this
    | Set_this
    | Get_constructor of Type.Class_id.t
    | Get_evolver of Type.Class_id.t
    | Create_object of int (* Number of fields to allocate *)
    | Assert_nonzero
    | Dup
  [@@deriving bin_io, sexp_of]

  let to_string t = t |> sexp_of_t |> Sexp.to_string_hum
end

module Function = struct
  type t =
    { name : string
    ; instructions : Instruction.t Vec.t
    }
  [@@deriving sexp_of]

  let create_empty name = { name; instructions = Vec.create () }

  let to_string { name; instructions } =
    name
    ^ ":\n"
    ^ (Vec.to_alist instructions
       |> List.map ~f:(fun (i, instr) ->
         String.pad_left (Int.to_string i) ~len:4
         ^ ": "
         ^ Instruction.to_string instr
         ^ "\n")
       |> String.concat)
  ;;
end

module Runtime_value = struct
  type slice =
    { underlying : t Array.t
    ; start : int
    ; len : int
    }
  [@@deriving sexp_of]

  and object_ =
    { mutable method_table : Type.Class_id.t option
    ; fields : t Array.t
    }

  and t =
    | Int of int
    | Function of Function.t
    | Object of object_
    | Float of float
    | Slice of slice
    | Undefined
  [@@deriving sexp_of]

  let to_string = function
    | Function f -> Function.to_string f
    | v -> v |> sexp_of_t |> Sexp.to_string
  ;;

  let equal a b =
    match a, b with
    | Int a, Int b -> a = b
    | Float a, Float b -> Float.equal a b
    | Function a, Function b -> phys_equal a b
    | ( Slice { underlying; start; len }
      , Slice { underlying = underlying2; start = start2; len = len2 } ) ->
      phys_equal underlying underlying2 && start = start2 && len = len2
    | Object a, Object b -> phys_equal a b
    | (Int _ | Float _ | Function _ | Undefined | Slice _ | Object _), _ -> false
  ;;

  let runtime_int_of_bool = function
    | true -> Int 1
    | false -> Int 0
  ;;
end

module Function_compiler = struct
  type t =
    { output : Function.t
    ; class_layout : Class_layout.t
    }

  let create ~class_layout ~name = { output = Function.create_empty name; class_layout }
  let emit_instr t instr = Vec.push_back t.output.instructions instr

  let emit_to_patch t =
    Vec.push_back t.output.instructions Instruction.Error_undefined;
    Vec.length t.output.instructions - 1
  ;;

  let patch t ~pos ~instr = Vec.set t.output.instructions pos instr
  let get_loc t = Vec.length t.output.instructions

  let patch_branch_to_here t ~at kind =
    let loc = get_loc t in
    let instr =
      match kind with
      | `If_true -> Instruction.Branch_if_true loc
      | `If_vtable_equals id -> Instruction.Branch_if_vtable_equals (loc, id)
      | `If_false -> Instruction.Branch_if_false loc
      | `Unconditional -> Instruction.Branch loc
    in
    patch t ~pos:at ~instr
  ;;

  let compile_arguments t args =
    (* Arguments are pushed in order, so we have to assign them to the arguments
       in reverse order *)
    args
    |> List.rev
    |> List.iter ~f:(fun (arg_id, _) ->
      emit_instr t (Set_local (Resolved_ident.Local.id arg_id)))
  ;;

  let push_unit t = emit_instr t Instruction.Const_0

  let emit_pop t =
    match Vec.peek_back t.output.instructions with
    | Some (Const_0 | Const_1 | Const_int _ | Const_float _) ->
      Vec.pop_back_unit_exn t.output.instructions
    | None | Some _ -> emit_instr t Pop
  ;;

  let rec compile_expr t ~(expr : Tast.Expr.t) =
    match Tast.Expr.kind expr with
    | Lit_int v ->
      let i_val = Int.of_string v in
      (match i_val with
       | 0 -> emit_instr t Instruction.Const_0
       | 1 -> emit_instr t Instruction.Const_1
       | _ -> emit_instr t (Instruction.Const_int i_val))
    | Lit_char v -> emit_instr t (Instruction.Const_int (Char.to_int v))
    | Null -> emit_instr t Instruction.Const_0
    | Unit -> push_unit t
    | Lit_string _ ->
      (* TODO: implement *)
      failwith "unimplemented"
    | Lit_bool true -> emit_instr t Instruction.Const_1
    | Lit_bool false -> emit_instr t Instruction.Const_0
    | Bin_op { lhs; rhs; op } ->
      let lhs_ty = Tast.Expr.ty lhs in
      let compile_std_op ~op ~lhs_ty =
        compile_expr t ~expr:lhs;
        compile_expr t ~expr:rhs;
        match lhs_ty with
        | Type.Numeric num_ty -> emit_instr t (op num_ty)
        | _ -> failwith "unreachable"
      in
      let compile_flipped_op ~op ~lhs_ty =
        compile_expr t ~expr:rhs;
        compile_expr t ~expr:lhs;
        match lhs_ty with
        | Type.Numeric num_ty -> emit_instr t (op num_ty)
        | _ -> failwith "unreachable"
      in
      (match op with
       | Add -> compile_std_op ~op:(fun s -> Add s) ~lhs_ty
       | Sub -> compile_std_op ~op:(fun s -> Sub s) ~lhs_ty
       | Div -> compile_std_op ~op:(fun s -> Div s) ~lhs_ty
       | Mul -> compile_std_op ~op:(fun s -> Mul s) ~lhs_ty
       | Gt -> compile_std_op ~op:(fun s -> Cmp_gt s) ~lhs_ty
       | Ge -> compile_std_op ~op:(fun s -> Cmp_ge s) ~lhs_ty
       | Lt -> compile_flipped_op ~op:(fun s -> Cmp_gt s) ~lhs_ty
       | Le -> compile_flipped_op ~op:(fun s -> Cmp_ge s) ~lhs_ty
       | Eq ->
         compile_expr t ~expr:lhs;
         compile_expr t ~expr:rhs;
         emit_instr t Instruction.Cmp_eq
       | Ne ->
         compile_expr t ~expr:lhs;
         compile_expr t ~expr:rhs;
         emit_instr t Instruction.Cmp_eq;
         emit_instr t Instruction.Not
       | And | Or ->
         (* TODO: implement *)
         failwith "unimplemented")
    | Local local -> emit_instr t (Instruction.Get_local (Resolved_ident.Local.id local))
    | Global id -> emit_instr t (Instruction.Get_global id)
    | Un_op { rhs; op } ->
      compile_expr t ~expr:rhs;
      (match op with
       | Neg ->
         let instr =
           match Tast.Expr.ty rhs with
           | Numeric numty -> Instruction.Neg numty
           | _ -> failwith "unreachable"
         in
         emit_instr t instr
       | Not -> emit_instr t Instruction.Not)
    | If { cond; if_then; if_else } ->
      compile_expr t ~expr:cond;
      let branch_if_false = emit_to_patch t in
      compile_expr t ~expr:if_then;
      let branch_after_true = emit_to_patch t in
      patch_branch_to_here t ~at:branch_if_false `If_false;
      compile_expr t ~expr:if_else;
      patch_branch_to_here t ~at:branch_after_true `Unconditional
    | Let { local; expr } ->
      compile_expr t ~expr;
      emit_instr t (Instruction.Set_local (Resolved_ident.Local.id local));
      push_unit t
    | Block exprs ->
      let exprs_len = List.length exprs in
      (match exprs with
       | [] -> push_unit t
       | _ ->
         List.iteri exprs ~f:(fun i expr ->
           compile_expr t ~expr;
           if i < exprs_len - 1 then emit_pop t))
    | Return expr ->
      compile_expr t ~expr;
      emit_instr t Instruction.Return
    | While { cond; block } ->
      let branch_to_condition_loc = get_loc t in
      compile_expr t ~expr:cond;
      let branch_if_false = emit_to_patch t in
      compile_expr t ~expr:block;
      emit_pop t;
      emit_instr t (Instruction.Branch branch_to_condition_loc);
      patch_branch_to_here t ~at:branch_if_false `If_false;
      push_unit t
    | Function_call { expr; arguments } ->
      List.iter arguments ~f:(fun arg -> compile_expr t ~expr:arg);
      compile_expr t ~expr;
      emit_instr t Instruction.Call
    | Assign { lhs; rhs } ->
      compile_expr t ~expr:rhs;
      compile_assignment_to t ~lvalue:lhs;
      push_unit t
    | Array_subscript { expr; index } ->
      compile_expr t ~expr;
      compile_expr t ~expr:index;
      emit_instr t Instruction.Get_array
    | Lit_array elts ->
      List.iter elts ~f:(fun elt -> compile_expr t ~expr:elt);
      emit_instr t (Instruction.Const_int (List.length elts));
      emit_instr t Instruction.Array_of_stack
    | Field_subscript { expr = sub_expr; field } ->
      compile_expr t ~expr:sub_expr;
      let ty = Tast.Expr.ty sub_expr in
      let class_id =
        match ty with
        | Type.Object class_id -> class_id
        | _ -> failwith "unreachable"
      in
      let offset = Class_layout.field_offset t.class_layout ~class_id ~field in
      emit_instr t (Instruction.Get_field offset)
    | Super | This -> emit_instr t Instruction.Get_this
    | Method_call { expr; method_; arguments } ->
      List.iter arguments ~f:(fun arg -> compile_expr t ~expr:arg);
      compile_expr t ~expr;
      let ty = Tast.Expr.ty expr in
      let class_id =
        match ty with
        | Type.Object class_id -> class_id
        | _ -> failwith "unreachable"
      in
      let offset = Class_layout.method_offset t.class_layout ~class_id ~method_ in
      emit_instr t (Instruction.Lookup_method offset);
      emit_instr t Instruction.Call
    | Super_method_call { super_id; method_; arguments } ->
      List.iter arguments ~f:(fun arg -> compile_expr t ~expr:arg);
      let offset =
        Class_layout.method_offset t.class_layout ~class_id:super_id ~method_
      in
      emit_instr t Instruction.Get_this;
      emit_instr t (Instruction.Lookup_method_static (offset, super_id));
      emit_instr t Instruction.Call
    | New { class_id; arguments } ->
      List.iter arguments ~f:(fun expr -> compile_expr t ~expr);
      let size = Class_layout.max_fields_size t.class_layout ~class_id in
      emit_instr t (Instruction.Create_object size);
      emit_instr t (Instruction.Get_constructor class_id);
      emit_instr t Instruction.Call
    | Evolves { expr; class_id; arguments } ->
      List.iter arguments ~f:(fun expr -> compile_expr t ~expr);
      compile_expr t ~expr;
      emit_instr t (Instruction.Get_evolver class_id);
      emit_instr t Instruction.Call
    | Super_call { super_id; arguments } ->
      List.iter arguments ~f:(fun expr -> compile_expr t ~expr);
      emit_instr t Instruction.Get_this;
      emit_instr t (Instruction.Get_constructor super_id);
      emit_instr t Instruction.Call
    | Update_this_vtable_after { expr; new_table } ->
      compile_expr t ~expr;
      emit_instr t (Instruction.Update_this_vtable new_table)
    | De_null { lhs } ->
      compile_expr t ~expr:lhs;
      emit_instr t Instruction.Dup;
      emit_instr t Instruction.Assert_nonzero
    | Or_else { lhs; or_else } ->
      compile_expr t ~expr:lhs;
      emit_instr t Instruction.Dup;
      let branch_if_non_null = emit_to_patch t in
      (* If it is null *)
      emit_instr t Instruction.Pop;
      compile_expr t ~expr:or_else;
      patch_branch_to_here t ~at:branch_if_non_null `If_true
    | If_option { expr; var; if_value; if_null } ->
      compile_expr t ~expr;
      emit_instr t Instruction.Dup;
      let branch_if_false = emit_to_patch t in
      emit_instr t (Instruction.Set_local (Resolved_ident.Local.id var));
      compile_expr t ~expr:if_value;
      let branch_after_true = emit_to_patch t in
      patch_branch_to_here t ~at:branch_if_false `If_false;
      emit_instr t Instruction.Pop;
      compile_expr t ~expr:if_null;
      patch_branch_to_here t ~at:branch_after_true `Unconditional
    | Exchange _ | Array_subrange _ | New_array _ -> failwith "unimplemented"

  and compile_assignment_to t ~(lvalue : Tast.Expr.t) =
    match Tast.Expr.kind lvalue with
    | Local local -> emit_instr t (Instruction.Set_local (Resolved_ident.Local.id local))
    | Global id -> emit_instr t (Instruction.Set_global id)
    | Array_subscript { expr; index } ->
      compile_expr t ~expr;
      compile_expr t ~expr:index;
      emit_instr t Instruction.Set_array
    | Field_subscript { expr; field } ->
      compile_expr t ~expr;
      let ty = Tast.Expr.ty expr in
      let class_id =
        match ty with
        | Type.Object class_id -> class_id
        | _ -> raise_s [%message "unreachable: cannot subscript type" (ty : Type.t)]
      in
      let offset = Class_layout.field_offset t.class_layout ~class_id ~field in
      emit_instr t (Instruction.Set_field offset)
    | Lit_int _
    | Lit_char _
    | Lit_string _
    | Lit_bool _
    | This
    | Super
    | Bin_op _
    | Un_op _
    | If _
    | Unit
    | While _
    | Block _
    | Let _
    | Assign _
    | Function_call _
    | Return _
    | Method_call _
    | Super_method_call _
    | New _
    | Evolves _
    | Update_this_vtable_after _
    | Super_call _
    | De_null _
    | Null
    | Or_else _
    | If_option _
    | Array_subrange _
    | Exchange _
    | New_array _
    | Lit_array _ -> failwith "unreachable"
  ;;
end

module Compilation_unit = struct
  type t =
    { globals : Runtime_value.t Resolved_ident.Global.Id.Table.t
    ; global_env : (Check.Global_env.t[@sexp.opaque])
    ; class_env : (Check.Class_env.t[@sexp.opaque])
    ; constructors : Function.t Type.Class_id.Table.t
    ; evolvers : Function.t Type.Class_id.Table.t
    ; methods : Function.t Array.t Type.Class_id.Table.t
    ; class_layout : (Class_layout.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let to_string t =
    let globals = Hashtbl.data t.globals in
    let constructors =
      Hashtbl.data t.constructors |> List.map ~f:(fun s -> Runtime_value.Function s)
    in
    let evolvers =
      Hashtbl.data t.evolvers |> List.map ~f:(fun s -> Runtime_value.Function s)
    in
    let methods =
      Hashtbl.data t.methods
      |> Array.concat
      |> Array.to_list
      |> List.map ~f:(fun s -> Runtime_value.Function s)
    in
    globals @ constructors @ evolvers @ methods
    |> List.map ~f:Runtime_value.to_string
    |> String.concat ~sep:"\n"
  ;;

  (* TODO: Make this more generic. *)
  let rec evaluate_expr t ~expr =
    match Tast.Expr.kind expr with
    | Tast.Expr.Lit_int x -> Runtime_value.Int (Int.of_string x)
    | Lit_char x -> Runtime_value.Int (Char.to_int x)
    | Global id -> Hashtbl.find_exn t.globals id
    | Null | Unit -> Runtime_value.Int 0
    | Lit_bool true -> Runtime_value.Int 1
    | Lit_bool false -> Runtime_value.Int 0
    | Bin_op { lhs; rhs; op } ->
      let lhs_value = evaluate_expr t ~expr:lhs in
      let rhs_value = evaluate_expr t ~expr:rhs in
      let evaluate_num_num_num ~int_fn ~float_fn ~lhs_value ~rhs_value =
        (* TODO: This doesn't work with chars who are represented as ints but
           have different semantics wrt to adding at runtime. *)
        match lhs_value, rhs_value with
        | Runtime_value.Int a, Runtime_value.Int b -> Runtime_value.Int (int_fn a b)
        | Runtime_value.Float a, Runtime_value.Float b ->
          Runtime_value.Float (float_fn a b)
        | _ -> failwith "unreachable"
      in
      let evaluate_num_num_bool ~int_fn ~float_fn ~lhs_value ~rhs_value =
        match lhs_value, rhs_value with
        | Runtime_value.Int a, Runtime_value.Int b ->
          Runtime_value.runtime_int_of_bool (int_fn a b)
        | Runtime_value.Float a, Runtime_value.Float b ->
          Runtime_value.runtime_int_of_bool (float_fn a b)
        | _ -> failwith "unreachable"
      in
      (match op with
       | Add ->
         evaluate_num_num_num ~int_fn:( + ) ~float_fn:Float.( + ) ~lhs_value ~rhs_value
       | Sub ->
         evaluate_num_num_num ~int_fn:( - ) ~float_fn:Float.( - ) ~lhs_value ~rhs_value
       | Mul ->
         evaluate_num_num_num ~int_fn:( * ) ~float_fn:Float.( * ) ~lhs_value ~rhs_value
       | Div ->
         evaluate_num_num_num ~int_fn:( / ) ~float_fn:Float.( / ) ~lhs_value ~rhs_value
       | Lt ->
         evaluate_num_num_bool
           ~int_fn:Int.( < )
           ~float_fn:Float.( < )
           ~lhs_value
           ~rhs_value
       | Gt ->
         evaluate_num_num_bool
           ~int_fn:Int.( > )
           ~float_fn:Float.( > )
           ~lhs_value
           ~rhs_value
       | Ge ->
         evaluate_num_num_bool
           ~int_fn:Int.( >= )
           ~float_fn:Float.( >= )
           ~lhs_value
           ~rhs_value
       | Le ->
         evaluate_num_num_bool
           ~int_fn:Int.( <= )
           ~float_fn:Float.( <= )
           ~lhs_value
           ~rhs_value
       | Eq -> Runtime_value.runtime_int_of_bool (Runtime_value.equal lhs_value rhs_value)
       | Ne ->
         Runtime_value.runtime_int_of_bool (not (Runtime_value.equal lhs_value rhs_value))
       | And ->
         (match lhs_value, rhs_value with
          | Runtime_value.Int 1, _ -> rhs_value
          | _ -> lhs_value)
       | Or ->
         (match lhs_value, rhs_value with
          | Runtime_value.Int 0, _ -> rhs_value
          | _ -> lhs_value))
    | Un_op { rhs; op } ->
      let rhs_value = evaluate_expr t ~expr:rhs in
      (match op, rhs_value with
       | Neg, Runtime_value.Int x -> Runtime_value.Int (-x)
       | Neg, Runtime_value.Float x -> Runtime_value.Float (Float.neg x)
       | Not, Runtime_value.Int x -> Runtime_value.Int (1 - x)
       | _ -> failwith "unreachable")
    | Local _
    | Lit_string _
    | This
    | Super
    | If _
    | While _
    | Block _
    | Field_subscript _
    | Array_subscript _
    | Array_subrange _
    | Let _
    | Assign _
    | Function_call _
    | Lit_array _
    | New_array _
    | Method_call _
    | Return _
    | Update_this_vtable_after _
    | Super_call _
    | Super_method_call _
    | De_null _
    | Or_else _
    | If_option _
    | Evolves _
    | Exchange _
    | New _ ->
      (* TODO-someday: implement. Probably not worth implementing here anyway
         and to do something more robust that can be shared across backends. *)
      failwith "unimplemented"
  ;;

  let compile_constant t Tast.Decl.Const.{ id; expr; range = _ } =
    let value = evaluate_expr t ~expr in
    Hashtbl.set t.globals ~key:id ~data:value
  ;;

  let compile_function t Tast.Decl.Function.{ id; args; ret_type = _; body; range = _ } =
    let name = Check.Global_env.find_name t.global_env ~id |> Ast.Path.to_string in
    let function_compiler = Function_compiler.create ~class_layout:t.class_layout ~name in
    Function_compiler.compile_arguments function_compiler args;
    Function_compiler.compile_expr function_compiler ~expr:body;
    Function_compiler.emit_instr function_compiler Instruction.Return;
    let output = function_compiler.output in
    Hashtbl.set t.globals ~key:id ~data:(Runtime_value.Function output)
  ;;

  let compile_constructor
        t
        ~class_id
        ~class_path
        Tast.Decl.Class.Constructor.{ args; body; range = _ }
    =
    let name = Ast.Path.to_string class_path ^ ".constructor" in
    let function_compiler = Function_compiler.create ~class_layout:t.class_layout ~name in
    Function_compiler.emit_instr function_compiler Instruction.Set_this;
    Function_compiler.compile_arguments function_compiler args;
    Function_compiler.compile_expr function_compiler ~expr:body;
    Function_compiler.emit_pop function_compiler;
    (* Despite not doing so in the type system, a constructor should implicitly 
       return [this] *)
    Function_compiler.emit_instr function_compiler Instruction.Get_this;
    Function_compiler.emit_instr function_compiler Instruction.Return;
    let output = function_compiler.output in
    Hashtbl.set t.constructors ~key:class_id ~data:output
  ;;

  let compile_evolver
        t
        ~super_id
        ~class_id
        ~class_path
        Tast.Decl.Class.Constructor.{ args; body; range = _ }
    =
    let name = Ast.Path.to_string class_path ^ ".evolver" in
    let function_compiler = Function_compiler.create ~class_layout:t.class_layout ~name in
    (* We have to check the method table in an evolver. *)
    Function_compiler.emit_instr function_compiler Instruction.Dup;
    let branch_if_right_id = Function_compiler.emit_to_patch function_compiler in
    Function_compiler.emit_instr function_compiler Instruction.Pop;
    Function_compiler.emit_instr function_compiler Instruction.Const_0;
    Function_compiler.emit_instr function_compiler Return;
    Function_compiler.patch_branch_to_here
      function_compiler
      ~at:branch_if_right_id
      (`If_vtable_equals super_id);
    Function_compiler.emit_instr function_compiler Instruction.Set_this;
    Function_compiler.compile_arguments function_compiler args;
    Function_compiler.compile_expr function_compiler ~expr:body;
    Function_compiler.emit_pop function_compiler;
    Function_compiler.emit_instr function_compiler Instruction.Get_this;
    Function_compiler.emit_instr function_compiler Instruction.Return;
    let output = function_compiler.output in
    Hashtbl.set t.evolvers ~key:class_id ~data:output
  ;;

  let compile_method
        t
        ~class_id
        ~class_path
        Tast.Decl.Class.Method.
          { visibility = _; name; args; ret_type = _; body; overrides = _; range = _ }
    =
    let fn_name = Ast.Path.to_string class_path ^ "." ^ Ast.Ident.to_string name in
    let function_compiler =
      Function_compiler.create ~class_layout:t.class_layout ~name:fn_name
    in
    Function_compiler.emit_instr function_compiler Instruction.Set_this;
    Function_compiler.compile_arguments function_compiler args;
    Function_compiler.compile_expr function_compiler ~expr:body;
    Function_compiler.emit_instr function_compiler Instruction.Return;
    let output = function_compiler.output in
    let offset = Class_layout.method_offset t.class_layout ~class_id ~method_:name in
    output, offset
  ;;

  let compile_class
        t
        Tast.Decl.Class.
          { id; super_type; methods; fields = _; constructor; evolver; range = _ }
    =
    let class_path = Check.Class_env.find_name t.class_env ~id in
    Option.iter constructor ~f:(compile_constructor t ~class_id:id ~class_path);
    Option.iter evolver ~f:(fun evolver ->
      compile_evolver
        t
        ~super_id:(Option.value_exn super_type)
        ~class_id:id
        ~class_path
        evolver);
    let compiled_methods =
      List.map methods ~f:(fun method_ ->
        compile_method t ~class_id:id ~class_path method_)
    in
    let opt_array =
      Array.create ~len:(Class_layout.methods_size t.class_layout ~class_id:id) None
    in
    (match super_type with
     | None -> ()
     | Some super_id ->
       let super_vtable =
         Hashtbl.find_exn t.methods super_id |> Array.map ~f:Option.some
       in
       Array.blito ~src:super_vtable ~dst:opt_array ());
    List.iter compiled_methods ~f:(fun (f, pos) -> Array.set opt_array pos (Some f));
    let vtable = Array.map opt_array ~f:(fun s -> Option.value_exn s) in
    Hashtbl.add_exn t.methods ~key:id ~data:vtable
  ;;

  (* Returns the entry point of the program "main.main" *)
  let compile_program ~check ~(decls : Tast.Decls.t) =
    let Tast.Decls.{ constants; functions; classes; extern_functions } = decls in
    if not (List.is_empty extern_functions)
    then raise_s [%message "external functions not supported for bytecode compiler"];
    let global_env = Check.global_env check in
    let class_env = Check.class_env check in
    let class_layout =
      Class_layout.of_signatures ~sizeof:(Fn.const 1) (Check.class_table check)
    in
    let t =
      { globals = Resolved_ident.Global.Id.Table.create ()
      ; global_env
      ; class_env
      ; class_layout
      ; methods = Type.Class_id.Table.create ()
      ; constructors = Type.Class_id.Table.create ()
      ; evolvers = Type.Class_id.Table.create ()
      }
    in
    List.iter constants ~f:(compile_constant t);
    List.iter functions ~f:(compile_function t);
    List.iter classes ~f:(compile_class t);
    let main_id =
      Check.Global_env.find_id
        global_env
        ~scope:[]
        ~qualified_name:(Ast.Path.of_string "main")
    in
    t, main_id
  ;;
end

module VM = struct
  module Stack_frame = struct
    type t =
      { f : Function.t
      ; mutable ip : int
      ; mutable this : Runtime_value.t
      ; locals : Runtime_value.t Vec.t
      }
    [@@deriving sexp_of]

    let create f = { f; ip = 0; this = Runtime_value.Undefined; locals = Vec.create () }
  end

  type t =
    { globals : Runtime_value.t Resolved_ident.Global.Id.Table.t
    ; working_stack : Runtime_value.t Vec.t
    ; constructors : Function.t Type.Class_id.Table.t
    ; evolvers : Function.t Type.Class_id.Table.t
    ; methods : Function.t Array.t Type.Class_id.Table.t
    ; mutable function_stack : Stack_frame.t Nonempty_list.t
    }
  [@@deriving sexp_of]

  let of_compilation_unit (compilation_unit : Compilation_unit.t) entry_point =
    match Hashtbl.find compilation_unit.globals entry_point with
    | None -> raise_s [%message "Entry point does not exist"]
    | Some
        ( Runtime_value.Float _
        | Runtime_value.Int _
        | Runtime_value.Undefined
        | Runtime_value.Slice _
        | Runtime_value.Object _ ) -> raise_s [%message "Entry point is not a function."]
    | Some (Runtime_value.Function f) ->
      { globals = compilation_unit.globals
      ; methods = compilation_unit.methods
      ; constructors = compilation_unit.constructors
      ; evolvers = compilation_unit.evolvers
      ; working_stack = Vec.create ()
      ; function_stack = [ Stack_frame.create f ]
      }
  ;;

  let pop t = Vec.pop_back_exn t.working_stack

  let pop_int t =
    match Vec.pop_back_exn t.working_stack with
    | Runtime_value.Int x -> x
    | _ -> failwith "unreachable"
  ;;

  let pop_float t =
    match Vec.pop_back_exn t.working_stack with
    | Runtime_value.Float x -> x
    | _ -> raise_s [%message "unreachable"]
  ;;

  let pop_function t =
    match Vec.pop_back_exn t.working_stack with
    | Runtime_value.Function x -> x
    | _ ->
      raise_s
        [%message
          "unreachable"
            (t.working_stack : Runtime_value.t Vec.t)
            (t.function_stack : Stack_frame.t Nonempty_list.t)]
  ;;

  let pop_slice t =
    match Vec.pop_back_exn t.working_stack with
    | Runtime_value.Slice x -> x
    | _ -> raise_s [%message "unreachable"]
  ;;

  let pop_object t =
    match Vec.pop_back_exn t.working_stack with
    | Runtime_value.Object x -> x
    | _ -> raise_s [%message "unreachable"]
  ;;

  let push t i = Vec.push_back t.working_stack i
  let push_int t i = Vec.push_back t.working_stack (Runtime_value.Int i)
  let push_float t i = Vec.push_back t.working_stack (Runtime_value.Float i)

  let execute_vm t =
    let final_value = ref None in
    while !final_value |> Option.is_none do
      let stack_frame = Nonempty_list.hd t.function_stack in
      let instr = Vec.get stack_frame.f.instructions stack_frame.ip in
      stack_frame.ip <- stack_frame.ip + 1;
      let[@inline] perform_numeric ~int_fn ~float_fn ty =
        match Type.Numeric.is_integral ty with
        | true ->
          let b = pop_int t in
          let a = pop_int t in
          (match Type.Numeric.bitwidth ty with
           | 64 -> push_int t (int_fn a b % 256)
           | width -> push_int t (int_fn a b % (1 lsl width)))
        | false ->
          let b = pop_float t in
          let a = pop_float t in
          push_float t (float_fn a b)
      in
      let[@inline] perform_numeric_check ~int_fn ~float_fn ty =
        match Type.Numeric.is_integral ty with
        | true ->
          let b = pop_int t in
          let a = pop_int t in
          push t (Runtime_value.runtime_int_of_bool (int_fn a b))
        | false ->
          let b = pop_float t in
          let a = pop_float t in
          push t (Runtime_value.runtime_int_of_bool (float_fn a b))
      in
      match instr with
      | Add ty -> perform_numeric ~int_fn:( + ) ~float_fn:Float.( + ) ty
      | Sub ty -> perform_numeric ~int_fn:( - ) ~float_fn:Float.( - ) ty
      | Mul ty -> perform_numeric ~int_fn:( * ) ~float_fn:Float.( * ) ty
      | Div ty -> perform_numeric ~int_fn:( / ) ~float_fn:Float.( / ) ty
      | Cmp_gt ty -> perform_numeric_check ~int_fn:( > ) ~float_fn:Float.( > ) ty
      | Cmp_ge ty -> perform_numeric_check ~int_fn:( >= ) ~float_fn:Float.( >= ) ty
      | Neg ty ->
        (match Type.Numeric.is_integral ty with
         | true -> push_int t (-pop_int t)
         | false -> push_float t Float.(-pop_float t))
      | Not -> push_int t (1 - pop_int t)
      | Cmp_eq ->
        let b = pop t in
        let a = pop t in
        push t (Runtime_value.runtime_int_of_bool (Runtime_value.equal a b))
      | Const_0 -> push_int t 0
      | Const_1 -> push_int t 1
      | Const_int x -> push_int t x
      | Const_float x -> push_float t x
      | Pop -> ignore (pop t : Runtime_value.t)
      | Get_local i ->
        let f = Nonempty_list.hd t.function_stack in
        i |> Vec.get f.locals |> push t
      | Set_local i ->
        let f = Nonempty_list.hd t.function_stack in
        let v = pop t in
        Vec.grow_to_include f.locals i ~default:Runtime_value.Undefined;
        Vec.set f.locals i v
      | Get_global i -> push t (Hashtbl.find_exn t.globals i)
      | Set_global i ->
        let v = pop t in
        Hashtbl.set t.globals ~key:i ~data:v
      | Branch new_ip ->
        let curr_frame = Nonempty_list.hd t.function_stack in
        curr_frame.ip <- new_ip
      | Branch_if_true new_ip ->
        let curr_frame = Nonempty_list.hd t.function_stack in
        let v = pop t in
        (match v with
         | Int 0 -> ()
         | _ -> curr_frame.ip <- new_ip)
      | Branch_if_false new_ip ->
        let curr_frame = Nonempty_list.hd t.function_stack in
        let v = pop t in
        (match v with
         | Int 0 -> curr_frame.ip <- new_ip
         | _ -> ())
      | Branch_if_vtable_equals (new_ip, class_id) ->
        let curr_frame = Nonempty_list.hd t.function_stack in
        let v = pop_object t in
        if [%equal: Type.Class_id.t option] v.method_table (Some class_id)
        then curr_frame.ip <- new_ip
      | Return ->
        (match Nonempty_list.tl t.function_stack with
         | [] -> final_value := Some (pop t)
         | new_hd :: new_stack -> t.function_stack <- new_hd :: new_stack)
      | Call ->
        let f = pop_function t in
        t.function_stack <- Stack_frame.create f :: Nonempty_list.to_list t.function_stack
      | Get_array ->
        let index = pop_int t in
        let slice = pop_slice t in
        if index >= slice.len then failwith "index out of bounds";
        push t (Array.get slice.underlying (index + slice.start))
      | Set_array ->
        let index = pop_int t in
        let slice = pop_slice t in
        if index >= slice.len then failwith "index out of bounds";
        let v = pop t in
        Array.set slice.underlying (index + slice.start) v
      | Array_of_stack ->
        let length = pop_int t in
        let underlying = Array.init length ~f:(fun (_ : int) -> pop t) in
        Array.rev_inplace underlying;
        push t (Runtime_value.Slice { underlying; start = 0; len = length })
      | New_array ->
        let default = pop t in
        let length = pop_int t in
        let underlying = Array.create ~len:length default in
        push t (Runtime_value.Slice { underlying; start = 0; len = length })
      | Get_field i ->
        let obj = pop_object t in
        push t (Array.get obj.fields i)
      | Set_field i ->
        let obj = pop_object t in
        let v = pop t in
        Array.set obj.fields i v
      | Lookup_method i ->
        let obj = pop_object t in
        push t (Runtime_value.Object obj);
        let vtable = Hashtbl.find_exn t.methods (Option.value_exn obj.method_table) in
        push t (Runtime_value.Function (Array.get vtable i))
      | Lookup_method_static (i, class_id) ->
        let vtable = Hashtbl.find_exn t.methods class_id in
        push t (Runtime_value.Function (Array.get vtable i))
      | Get_this ->
        let f = Nonempty_list.hd t.function_stack in
        push t f.this
      | Set_this ->
        let f = Nonempty_list.hd t.function_stack in
        let this = pop t in
        f.this <- this
      | Update_this_vtable class_id ->
        let f = Nonempty_list.hd t.function_stack in
        (match f.this with
         | Object obj -> obj.method_table <- Some class_id
         | _ -> failwith "unreachable")
      | Create_object len_fields ->
        push
          t
          (Runtime_value.Object
             { method_table = None
             ; fields = Array.create ~len:len_fields Runtime_value.Undefined
             })
      | Get_constructor class_id ->
        push t (Runtime_value.Function (Hashtbl.find_exn t.constructors class_id))
      | Get_evolver class_id ->
        push t (Runtime_value.Function (Hashtbl.find_exn t.evolvers class_id))
      | Dup ->
        let v = pop t in
        push t v;
        push t v
      | Assert_nonzero ->
        let v = pop t in
        (match v with
         | Int 0 -> raise_s [%message "Unexpected null."]
         | _ -> ())
      | Get_external _ ->
        (* TODO: implement externals *)
        failwith "unimplemented"
      | Error_undefined ->
        failwith "patch instruction executed (should have been patched over)"
    done;
    !final_value |> Option.value_exn
  ;;
end
