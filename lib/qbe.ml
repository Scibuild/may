open! Core

module Block_name = struct
  include
    String_id.Make
      (struct
        let module_name = "Block_name"
      end)
      ()

  let to_string x = "@" ^ to_string x
end

module Const = struct
  type t =
    [ `Int of int
    | `Sp_float of float
    | `Dp_float of float
    | `Symbol of string
    ]
  [@@deriving sexp_of]
end

module Dyn_const = struct
  type t =
    [ `Thread_local of string
    | Const.t
    ]
  [@@deriving sexp_of]
end

module Value = struct
  type t =
    [ `Local of string
    | Dyn_const.t
    ]
  [@@deriving sexp_of]

  let to_string (t : t) =
    match t with
    | `Int i -> Int.to_string i
    | `Sp_float f -> "s_" ^ Float.to_string f
    | `Dp_float f -> "d_" ^ Float.to_string f
    | `Symbol s -> "$" ^ s
    | `Thread_local s -> "thread $" ^ s
    | `Local s -> "%" ^ s
  ;;
end

module Base_ty = struct
  type t =
    [ `W
    | `L
    | `S
    | `D
    ]
end

module Ext_ty = struct
  type t =
    [ Base_ty.t
    | `B
    | `H
    ]
end

module Abi_ty = struct
  type t =
    [ Base_ty.t
    | `SB
    | `UB
    | `SH
    | `UH
    | `Name of string
    ]
end

module Type = struct
  let to_string = function
    | `B -> "b"
    | `H -> "h"
    | `W -> "w"
    | `L -> "l"
    | `S -> "s"
    | `D -> "d"
    | `SB -> "sb"
    | `UB -> "ub"
    | `SH -> "sh"
    | `UH -> "uh"
    | `Name s -> ":" ^ s
  ;;
end

module Instr = struct
  type t =
    { result : (string * Base_ty.t) option
    ; op : string
    ; args : Value.t list
    ; comment : string option
    }

  let create ?comment ?ty ?v ~op ?(args = []) () =
    if Bool.(Option.is_some ty <> Option.is_some v)
    then raise_s [%message "both ty and v must either be specified or absent"];
    let result = Option.both v ty in
    { op
    ; result
    ; args
    ; comment = Option.map comment ~f:(String.substr_replace_all ~pattern:"\n" ~with_:" ")
    }
  ;;

  let to_string { result; op; args; comment } =
    let args_formatted = List.map args ~f:Value.to_string |> String.concat ~sep:", " in
    let op_with_args = [%string "%{op} %{args_formatted}"] in
    let op_with_result =
      match result with
      | None -> op_with_args
      | Some (varname, varty) -> [%string "%%{varname} =%{varty#Type} %{op_with_args}"]
    in
    match comment with
    | None -> op_with_result
    | Some c -> op_with_result ^ " # " ^ c
  ;;

  module Jump = struct
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
    [@@deriving sexp_of]

    let jnz ~v ~ifz ~ifnz = Jnz { v; ifz; ifnz }

    let to_string = function
      | Ret -> "ret"
      | Hlt -> "hlt"
      | Ret_val v -> "ret " ^ Value.to_string v
      | Jmp block -> "jmp " ^ Block_name.to_string block
      | Jnz { v; ifz; ifnz } ->
        [%string "jnz %{v#Value}, %{ifnz#Block_name}, %{ifz#Block_name}"]
    ;;
  end
end

module Block = struct
  type t =
    { name : Block_name.t
    ; instructions : Instr.t Vec.t
    ; mutable jump : Instr.Jump.t option
    }

  let create ~name = { name; instructions = Vec.create (); jump = None }
  let name t = t.name

  let add_jump t ~jump =
    match t.jump with
    | None -> t.jump <- Some jump
    | Some jump ->
      if true
      then ()
      else raise_s [%message "jump instruction already added" (jump : Instr.Jump.t)]
  ;;

  let add_instr t instr =
    match t.jump with
    | None -> Vec.push_back t.instructions instr
    | Some jump ->
      if true
      then ()
      else
        raise_s
          [%message
            "cannot add new instruction, jump instruction already added"
              (jump : Instr.Jump.t)]
  ;;

  let addi t ?comment ?ty ?v ~op ?args () =
    add_instr t (Instr.create ?comment ?ty ?v ~op ?args ())
  ;;

  let add_to_buffer b { name; instructions; jump } =
    let jump =
      Option.value_exn ~message:"Block does not end with a jump instruction" jump
    in
    let module B = Bigbuffer in
    B.add_string b (Block_name.to_string name);
    B.add_char b '\n';
    let add_instr b s =
      B.add_string b (String.make 8 ' ');
      B.add_string b s;
      B.add_char b '\n'
    in
    Vec.iter instructions ~f:(fun i -> add_instr b (Instr.to_string i));
    add_instr b (Instr.Jump.to_string jump);
    B.add_char b '\n'
  ;;

  let to_string block =
    let b = Bigbuffer.create 32 in
    add_to_buffer b block;
    Bigbuffer.contents b
  ;;
end

module Linkage = struct
  type t =
    | Export
    | Thread
    | Section of string

  let to_string = function
    | Export -> "export"
    | Thread -> "thread"
    | Section s -> [%string "section \"%{s}\""]
  ;;

  let add_list_to_buffer b =
    List.iter ~f:(fun l ->
      Bigbuffer.add_string b (to_string l);
      Bigbuffer.add_char b '\n')
  ;;
end

module Function = struct
  type t =
    { return_type : Abi_ty.t option
    ; parameters : (string * Abi_ty.t) list
    ; name : string
    ; blocks : Block.t Vec.t
    ; linkage : Linkage.t list
    }

  let create ?return_type ?(parameters = []) ~name ?(linkage = []) () =
    { return_type; parameters; name; linkage; blocks = Vec.create () }
  ;;

  let param_values t = List.map t.parameters ~f:(fun (s, _) -> `Local s)

  let add_block t ~name =
    let block = Block.create ~name:(Block_name.of_string name) in
    Vec.push_back t.blocks block;
    block
  ;;

  let add_to_buffer { return_type; parameters; name; blocks; linkage } b =
    let module B = Bigbuffer in
    Linkage.add_list_to_buffer b linkage;
    B.add_string b "function ";
    Option.iter return_type ~f:(fun ty ->
      B.add_string b (Type.to_string ty);
      B.add_char b ' ');
    B.add_char b '$';
    B.add_string b name;
    B.add_char b '(';
    List.iter parameters ~f:(fun (s, ty) ->
      B.add_string b (Type.to_string ty);
      B.add_string b " %";
      B.add_string b s;
      B.add_string b ", ");
    B.add_string b ")\n{\n";
    Vec.iter blocks ~f:(Block.add_to_buffer b);
    B.add_string b "}\n"
  ;;

  let to_string t =
    let b = Bigbuffer.create 100 in
    add_to_buffer t b;
    Bigbuffer.contents b
  ;;
end

module Data = struct
  module Item = struct
    type t =
      | Pad of int
      | I of Ext_ty.t * [ Const.t | `Symbol_plus of string * int | `String of string ]
  end

  type t =
    { name : string
    ; linkage : Linkage.t list
    ; items : Item.t list
    }

  let create ~name ?(linkage = []) items = { name; linkage; items }

  let add_to_buffer { name; linkage; items } b =
    let module B = Bigbuffer in
    Linkage.add_list_to_buffer b linkage;
    B.add_string b "data $";
    B.add_string b name;
    B.add_string b " = { ";
    List.iter items ~f:(fun item ->
      (match item with
       | Pad i ->
         B.add_string b "z ";
         B.add_string b (Int.to_string i)
       | I (ty, value) ->
         B.add_string b (Type.to_string ty);
         B.add_char b ' ';
         let value_string =
           match value with
           | `String s -> [%string "\"%{s}\""]
           | `Symbol_plus (symbol, offset) -> [%string "$%{symbol} + %{offset#Int}"]
           | (`Int _ | `Sp_float _ | `Dp_float _ | `Symbol _) as const ->
             Value.to_string const
         in
         B.add_string b value_string);
      B.add_string b ", ");
    B.add_string b "}\n"
  ;;

  let to_string t =
    let b = Bigbuffer.create 100 in
    add_to_buffer t b;
    Bigbuffer.contents b
  ;;
end

module Decl = struct
  type t =
    | Data of Data.t
    | Function of Function.t

  let add_to_buffer = function
    | Data d -> Data.add_to_buffer d
    | Function f -> Function.add_to_buffer f
  ;;
end

let%expect_test "test basic function creation" =
  let test_function =
    Function.create ~name:"add" ~return_type:`W ~parameters:[ "a", `W; "b", `W ] ()
  in
  let[@warning "-8"] [ a; b ] = Function.param_values test_function in
  let start_block = Function.add_block test_function ~name:"start" in
  Block.addi start_block ~v:"c" ~ty:`W ~op:"add" ~args:[ a; b ] ~comment:"Adds a and b" ();
  Block.add_jump start_block ~jump:Ret;
  (* Print output: *)
  print_string (Function.to_string test_function);
  [%expect
    {|
    function w $add(w %a, w %b, )
    {
    @start
            %c =w add %a, %b # Adds a and b
            ret

    }
    |}]
;;

let%expect_test "test basic data creation" =
  let fmt =
    Data.create
      ~name:"fmt"
      ~linkage:[ Export ]
      [ I (`B, `String "One and one make %d!\\n"); Pad 1 ]
  in
  Data.to_string fmt |> print_string;
  [%expect
    {|
    export
    data $fmt = { b "One and one make %d!\n", z 1, }
    |}]
;;
