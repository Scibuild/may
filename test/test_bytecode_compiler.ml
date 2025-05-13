open! Core
open! Composition_infix

let programs =
  [ "fun main(): unit { }"
  ; "const x = 10;"
  ; {|
    fun double(x : int): int { x * 2 }
    fun quadruple(x : int): int {double(double(x))}
    fun main(): int {quadruple(10)}
    |}
  ; {|
    fun sum(): int {
      let x = 10;
      let total = 0;
      while (x > 0) {
        total = total + x;
        x = x - 1;
      }
      total
    }

    fun main() : int { sum() }
    |}
  ; {| fun main() : char { 'A' - 'a' + 'c'} |}
  ; {|
    fun main() : int { 
      let a : []mut int = [2, 5, 7]; 
      a[0] = 10;
      a[0] + a[2]
    }
    |}
  ; {|
    module A { 
      module B { 
        fun add(x : int, y : int): int { 
          x + y + 1
        }
      }
    }
    fun main() : int {
      A.B.add(34, 34)
    }
    |}
  ; {|
    class Int {
      x : int;

      constructor(x:int) {
        this.x = x;
      }

      fun get(): int {
        this.x
      }
      
      fun add(other: Int) : Int {
        let a = this:get();
        let b = other:get();

        new Int(a + b)
      }
    }

    class Counter < Int {
      mut count : int;

      constructor(x : int) {
        super(x);
        this.count = 0;
      }

      overrides fun get(): int {
        this.x + this.count
      }

      fun inc(): unit {
        this.count = this.count + 1;
      }
    }
    
    fun main() : int {
      let x = new Counter(24);
      x:inc();
      let y = new Counter(16);
      y:inc();
      let z = x:add(y);
      z:get()
    }
    |}
  ; {|
    class A { 
      constructor () {}
      fun m() : int { 5 }
    }
    class B < A {
      constructor () { super() }
      overrides fun m() : int { super:m() + 10}
    }
    fun main() : int {
      (new B()):m()
    }
    |}
  ; {|
    class LinkedList {
      mut v : int;
      mut next : ?LinkedList;

      constructor(v : int) {
        this.v = v;
        this.next = null;
      }
      
      fun setNext(next : ?LinkedList) : unit {
        this.next = next;
      }

      fun snoc(v : int) : LinkedList {
        let new_head = new LinkedList(v);
        new_head:setNext(this);
        new_head
      }

      fun sum() : int {
        this.v + (if? n = this.next { n:sum() } else { 0 })
      }
    }

    fun main() : int {
      let list = new LinkedList(4):snoc(2):snoc(7):snoc(-1);
      list:sum()
    }
    |}
  ; {|
    class A { constructor () {} }

    fun main() : bool {
      let x = new A();
      x == x
    }
    |}
  ; {|
    class A { constructor () {} }

    fun main() : bool {
      let x = new A();
      let y = new A();
      x == y
    }
    |}
  ; {|
    class A { 
      x : int;

      constructor(x : int) {
        this.x = x;
      }
    }

    class B < A {
      y : int;

      constructor(x : int, y : int) {
        super(x);
        this.y = y;
      }

      constructor(y : int) evolves A {
        this.y = y;
      }

      fun sum() : int {
        this.x + this.y
      }
    }
    
    fun main() : int {
      let a = new A(20);
      if? b = a evolves B(22) {
        b:sum()
      } else { 
        -1 
      }
    }
    |}
  ]
;;

let programs_without_bytecode =
  [ {|
    class A { 
      x : int;

      constructor(x : int) {
        this.x = x;
      }
    }

    class B < A {
      y : int;

      constructor(x : int, y : int) {
        super(x);
        this.y = y;
      }

      constructor(y : int) evolves A {
        this.y = y;
      }

      fun sum() : int {
        this.x + this.y
      }
    }
    
    fun main() : int {
      let a = new A(20);
      if? b = a evolves B(22) {
        b:sum()
      } else { 
        -1 
      }
    }
    |}
  ; {|
    class A { 
      x : int;

      constructor(x : int) {
        this.x = x;
      }
    }

    class B < A {
      y : int;

      constructor(x : int, y : int) {
        super(x);
        this.y = y;
      }

      constructor(y : int) evolves A {
        this.y = y;
      }

      fun sum() : int {
        this.x + this.y
      }
    }
    
    fun main() : int {
      let a = new A(20);
      if? b = a evolves B(22) {
        if? c = a evolves B(10) {
          2
        } else {
          1
        }
      } else { 
        0
      }
    }
    |}
  ; {|
    class A { 
      x : int;

      constructor(x : int) {
        this.x = x;
      }
    }

    class B < A {
      y : int;

      constructor(x : int, y : int) {
        super(x);
        this.y = y;
      }

      constructor(y : int) evolves A {
        this.y = y;
      }

      fun sum() : int {
        this.x + this.y
      }
    }
    
    fun main() : bool {
      let a = new A(20);
      let b = a evolves B(22).?;
      b == a
    }
    |}
  ]
;;

let test ~with_bytecode program =
  let program = Utils.remove_indentation program in
  May.Resolved_ident.Global.Id.For_testing.reset_counter ();
  May.Type.Class_id.For_testing.reset_counter ();
  let check = May.Check.empty () in
  let checked_ast =
    program
    |> May.For_testing.parse_string
    |> Result.bind ~f:(fun ast -> May.Check.check_decls check ~decls:ast)
    |> Result.map_error ~f:May.Comp_error.to_string
    |> Result.ok_or_failwith
  in
  let compiled_program, entry_point =
    May.Bytecode_compiler.Compilation_unit.compile_program ~check ~decls:checked_ast
  in
  let result =
    Option.map
      entry_point
      ~f:
        (May.Bytecode_compiler.VM.of_compilation_unit compiled_program
         >> May.Bytecode_compiler.VM.execute_vm)
  in
  match with_bytecode with
  | false ->
    [%sexp { program : string; result : May.Bytecode_compiler.Runtime_value.t option }]
  | true ->
    let compiled_program_str =
      May.Bytecode_compiler.Compilation_unit.to_string compiled_program
    in
    [%sexp
      { program : string
      ; result : May.Bytecode_compiler.Runtime_value.t option
      ; compiled_program_str : string
      }]
;;

let%expect_test "bytecode_compiler_output" =
  programs
  |> List.map ~f:(test ~with_bytecode:true)
  |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────┬──────────┬─────────────────────────────────────┐
    │ program                                                  │ result   │ compiled_program_str                │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ fun main(): unit { }                                     │ (Int 0)  │ main:                               │
    │                                                          │          │    0: Const_0                       │
    │                                                          │          │    1: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ const x = 10;                                            │          │ (Int 10)                            │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ fun double(x : int): int { x * 2 }                       │ (Int 40) │ quadruple:                          │
    │ fun quadruple(x : int): int {double(double(x))}          │          │    0: (Set_local 0)                 │
    │ fun main(): int {quadruple(10)}                          │          │    1: (Get_local 0)                 │
    │                                                          │          │    2: (Get_global 0)                │
    │                                                          │          │    3: Call                          │
    │                                                          │          │    4: (Get_global 0)                │
    │                                                          │          │    5: Call                          │
    │                                                          │          │    6: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ main:                               │
    │                                                          │          │    0: (Const_int 10)                │
    │                                                          │          │    1: (Get_global 1)                │
    │                                                          │          │    2: Call                          │
    │                                                          │          │    3: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ double:                             │
    │                                                          │          │    0: (Set_local 0)                 │
    │                                                          │          │    1: (Get_local 0)                 │
    │                                                          │          │    2: (Const_int 2)                 │
    │                                                          │          │    3: (Mul Int)                     │
    │                                                          │          │    4: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ fun sum(): int {                                         │ (Int 55) │ main:                               │
    │   let x = 10;                                            │          │    0: (Get_global 0)                │
    │   let total = 0;                                         │          │    1: Call                          │
    │   while (x > 0) {                                        │          │    2: Return                        │
    │     total = total + x;                                   │          │                                     │
    │     x = x - 1;                                           │          │ sum:                                │
    │   }                                                      │          │    0: (Const_int 10)                │
    │   total                                                  │          │    1: (Set_local 0)                 │
    │ }                                                        │          │    2: Const_0                       │
    │                                                          │          │    3: (Set_local 1)                 │
    │ fun main() : int { sum() }                               │          │    4: (Get_local 0)                 │
    │                                                          │          │    5: Const_0                       │
    │                                                          │          │    6: (Cmp_gt Int)                  │
    │                                                          │          │    7: (Branch_if_false 17)          │
    │                                                          │          │    8: (Get_local 1)                 │
    │                                                          │          │    9: (Get_local 0)                 │
    │                                                          │          │   10: (Add Int)                     │
    │                                                          │          │   11: (Set_local 1)                 │
    │                                                          │          │   12: (Get_local 0)                 │
    │                                                          │          │   13: Const_1                       │
    │                                                          │          │   14: (Sub Int)                     │
    │                                                          │          │   15: (Set_local 0)                 │
    │                                                          │          │   16: (Branch 4)                    │
    │                                                          │          │   17: (Get_local 1)                 │
    │                                                          │          │   18: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ fun main() : char { 'A' - 'a' + 'c'}                     │ (Int 67) │ main:                               │
    │                                                          │          │    0: (Const_int 65)                │
    │                                                          │          │    1: (Const_int 97)                │
    │                                                          │          │    2: (Sub Char)                    │
    │                                                          │          │    3: (Const_int 99)                │
    │                                                          │          │    4: (Add Char)                    │
    │                                                          │          │    5: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ fun main() : int {                                       │ (Int 17) │ main:                               │
    │   let a : []mut int = [2, 5, 7];                         │          │    0: (Const_int 2)                 │
    │   a[0] = 10;                                             │          │    1: (Const_int 5)                 │
    │   a[0] + a[2]                                            │          │    2: (Const_int 7)                 │
    │ }                                                        │          │    3: (Const_int 3)                 │
    │                                                          │          │    4: Array_of_stack                │
    │                                                          │          │    5: (Set_local 0)                 │
    │                                                          │          │    6: (Const_int 10)                │
    │                                                          │          │    7: (Get_local 0)                 │
    │                                                          │          │    8: Const_0                       │
    │                                                          │          │    9: Set_array                     │
    │                                                          │          │   10: (Get_local 0)                 │
    │                                                          │          │   11: Const_0                       │
    │                                                          │          │   12: Get_array                     │
    │                                                          │          │   13: (Get_local 0)                 │
    │                                                          │          │   14: (Const_int 2)                 │
    │                                                          │          │   15: Get_array                     │
    │                                                          │          │   16: (Add Int)                     │
    │                                                          │          │   17: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ module A {                                               │ (Int 69) │ main:                               │
    │   module B {                                             │          │    0: (Const_int 34)                │
    │     fun add(x : int, y : int): int {                     │          │    1: (Const_int 34)                │
    │       x + y + 1                                          │          │    2: (Get_global 0)                │
    │     }                                                    │          │    3: Call                          │
    │   }                                                      │          │    4: Return                        │
    │ }                                                        │          │                                     │
    │ fun main() : int {                                       │          │ A.B.add:                            │
    │   A.B.add(34, 34)                                        │          │    0: (Set_local 1)                 │
    │ }                                                        │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: (Get_local 0)                 │
    │                                                          │          │    3: (Get_local 1)                 │
    │                                                          │          │    4: (Add Int)                     │
    │                                                          │          │    5: Const_1                       │
    │                                                          │          │    6: (Add Int)                     │
    │                                                          │          │    7: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class Int {                                              │ (Int 42) │ main:                               │
    │   x : int;                                               │          │    0: (Const_int 24)                │
    │                                                          │          │    1: (Create_object 2)             │
    │   constructor(x:int) {                                   │          │    2: (Get_constructor 1)           │
    │     this.x = x;                                          │          │    3: Call                          │
    │   }                                                      │          │    4: (Set_local 0)                 │
    │                                                          │          │    5: (Get_local 0)                 │
    │   fun get(): int {                                       │          │    6: (Lookup_method 2)             │
    │     this.x                                               │          │    7: Call                          │
    │   }                                                      │          │    8: Pop                           │
    │                                                          │          │    9: (Const_int 16)                │
    │   fun add(other: Int) : Int {                            │          │   10: (Create_object 2)             │
    │     let a = this:get();                                  │          │   11: (Get_constructor 1)           │
    │     let b = other:get();                                 │          │   12: Call                          │
    │                                                          │          │   13: (Set_local 1)                 │
    │     new Int(a + b)                                       │          │   14: (Get_local 1)                 │
    │   }                                                      │          │   15: (Lookup_method 2)             │
    │ }                                                        │          │   16: Call                          │
    │                                                          │          │   17: Pop                           │
    │ class Counter < Int {                                    │          │   18: (Get_local 1)                 │
    │   mut count : int;                                       │          │   19: (Get_local 0)                 │
    │                                                          │          │   20: (Lookup_method 0)             │
    │   constructor(x : int) {                                 │          │   21: Call                          │
    │     super(x);                                            │          │   22: (Set_local 2)                 │
    │     this.count = 0;                                      │          │   23: (Get_local 2)                 │
    │   }                                                      │          │   24: (Lookup_method 1)             │
    │                                                          │          │   25: Call                          │
    │   overrides fun get(): int {                             │          │   26: Return                        │
    │     this.x + this.count                                  │          │                                     │
    │   }                                                      │          │ Counter.constructor:                │
    │                                                          │          │    0: Set_this                      │
    │   fun inc(): unit {                                      │          │    1: (Set_local 0)                 │
    │     this.count = this.count + 1;                         │          │    2: (Get_local 0)                 │
    │   }                                                      │          │    3: Get_this                      │
    │ }                                                        │          │    4: (Get_constructor 0)           │
    │                                                          │          │    5: Call                          │
    │ fun main() : int {                                       │          │    6: Pop                           │
    │   let x = new Counter(24);                               │          │    7: Const_0                       │
    │   x:inc();                                               │          │    8: Get_this                      │
    │   let y = new Counter(16);                               │          │    9: (Set_field 1)                 │
    │   y:inc();                                               │          │   10: Const_0                       │
    │   let z = x:add(y);                                      │          │   11: (Update_this_vtable 1)        │
    │   z:get()                                                │          │   12: Pop                           │
    │ }                                                        │          │   13: Get_this                      │
    │                                                          │          │   14: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Int.constructor:                    │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: (Get_local 0)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Set_field 0)                 │
    │                                                          │          │    5: Const_0                       │
    │                                                          │          │    6: (Update_this_vtable 0)        │
    │                                                          │          │    7: Pop                           │
    │                                                          │          │    8: Get_this                      │
    │                                                          │          │    9: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Int.add:                            │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: Get_this                      │
    │                                                          │          │    3: (Lookup_method 1)             │
    │                                                          │          │    4: Call                          │
    │                                                          │          │    5: (Set_local 1)                 │
    │                                                          │          │    6: (Get_local 0)                 │
    │                                                          │          │    7: (Lookup_method 1)             │
    │                                                          │          │    8: Call                          │
    │                                                          │          │    9: (Set_local 2)                 │
    │                                                          │          │   10: (Get_local 1)                 │
    │                                                          │          │   11: (Get_local 2)                 │
    │                                                          │          │   12: (Add Int)                     │
    │                                                          │          │   13: (Create_object 2)             │
    │                                                          │          │   14: (Get_constructor 0)           │
    │                                                          │          │   15: Call                          │
    │                                                          │          │   16: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Counter.get:                        │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_field 0)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Get_field 1)                 │
    │                                                          │          │    5: (Add Int)                     │
    │                                                          │          │    6: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Counter.inc:                        │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_field 1)                 │
    │                                                          │          │    3: Const_1                       │
    │                                                          │          │    4: (Add Int)                     │
    │                                                          │          │    5: Get_this                      │
    │                                                          │          │    6: (Set_field 1)                 │
    │                                                          │          │    7: Const_0                       │
    │                                                          │          │    8: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Int.add:                            │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: Get_this                      │
    │                                                          │          │    3: (Lookup_method 1)             │
    │                                                          │          │    4: Call                          │
    │                                                          │          │    5: (Set_local 1)                 │
    │                                                          │          │    6: (Get_local 0)                 │
    │                                                          │          │    7: (Lookup_method 1)             │
    │                                                          │          │    8: Call                          │
    │                                                          │          │    9: (Set_local 2)                 │
    │                                                          │          │   10: (Get_local 1)                 │
    │                                                          │          │   11: (Get_local 2)                 │
    │                                                          │          │   12: (Add Int)                     │
    │                                                          │          │   13: (Create_object 2)             │
    │                                                          │          │   14: (Get_constructor 0)           │
    │                                                          │          │   15: Call                          │
    │                                                          │          │   16: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ Int.get:                            │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_field 0)                 │
    │                                                          │          │    3: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class A {                                                │ (Int 15) │ main:                               │
    │   constructor () {}                                      │          │    0: (Create_object 0)             │
    │   fun m() : int { 5 }                                    │          │    1: (Get_constructor 1)           │
    │ }                                                        │          │    2: Call                          │
    │ class B < A {                                            │          │    3: (Lookup_method 0)             │
    │   constructor () { super() }                             │          │    4: Call                          │
    │   overrides fun m() : int { super:m() + 10}              │          │    5: Return                        │
    │ }                                                        │          │                                     │
    │ fun main() : int {                                       │          │ B.constructor:                      │
    │   (new B()):m()                                          │          │    0: Set_this                      │
    │ }                                                        │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_constructor 0)           │
    │                                                          │          │    3: Call                          │
    │                                                          │          │    4: (Update_this_vtable 1)        │
    │                                                          │          │    5: Pop                           │
    │                                                          │          │    6: Get_this                      │
    │                                                          │          │    7: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ A.constructor:                      │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Const_0                       │
    │                                                          │          │    2: (Update_this_vtable 0)        │
    │                                                          │          │    3: Pop                           │
    │                                                          │          │    4: Get_this                      │
    │                                                          │          │    5: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ B.m:                                │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Lookup_method_static 0 0)    │
    │                                                          │          │    3: Call                          │
    │                                                          │          │    4: (Const_int 10)                │
    │                                                          │          │    5: (Add Int)                     │
    │                                                          │          │    6: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ A.m:                                │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Const_int 5)                 │
    │                                                          │          │    2: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class LinkedList {                                       │ (Int 12) │ main:                               │
    │   mut v : int;                                           │          │    0: Const_1                       │
    │   mut next : ?LinkedList;                                │          │    1: (Neg Int)                     │
    │                                                          │          │    2: (Const_int 7)                 │
    │   constructor(v : int) {                                 │          │    3: (Const_int 2)                 │
    │     this.v = v;                                          │          │    4: (Const_int 4)                 │
    │     this.next = null;                                    │          │    5: (Create_object 2)             │
    │   }                                                      │          │    6: (Get_constructor 0)           │
    │                                                          │          │    7: Call                          │
    │   fun setNext(next : ?LinkedList) : unit {               │          │    8: (Lookup_method 1)             │
    │     this.next = next;                                    │          │    9: Call                          │
    │   }                                                      │          │   10: (Lookup_method 1)             │
    │                                                          │          │   11: Call                          │
    │   fun snoc(v : int) : LinkedList {                       │          │   12: (Lookup_method 1)             │
    │     let new_head = new LinkedList(v);                    │          │   13: Call                          │
    │     new_head:setNext(this);                              │          │   14: (Set_local 0)                 │
    │     new_head                                             │          │   15: (Get_local 0)                 │
    │   }                                                      │          │   16: (Lookup_method 2)             │
    │                                                          │          │   17: Call                          │
    │   fun sum() : int {                                      │          │   18: Return                        │
    │     this.v + (if? n = this.next { n:sum() } else { 0 })  │          │                                     │
    │   }                                                      │          │ LinkedList.constructor:             │
    │ }                                                        │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │ fun main() : int {                                       │          │    2: (Get_local 0)                 │
    │   let list = new LinkedList(4):snoc(2):snoc(7):snoc(-1); │          │    3: Get_this                      │
    │   list:sum()                                             │          │    4: (Set_field 1)                 │
    │ }                                                        │          │    5: Const_0                       │
    │                                                          │          │    6: Get_this                      │
    │                                                          │          │    7: (Set_field 0)                 │
    │                                                          │          │    8: Const_0                       │
    │                                                          │          │    9: (Update_this_vtable 0)        │
    │                                                          │          │   10: Pop                           │
    │                                                          │          │   11: Get_this                      │
    │                                                          │          │   12: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ LinkedList.setNext:                 │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: (Get_local 0)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Set_field 0)                 │
    │                                                          │          │    5: Const_0                       │
    │                                                          │          │    6: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ LinkedList.snoc:                    │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: (Get_local 0)                 │
    │                                                          │          │    3: (Create_object 2)             │
    │                                                          │          │    4: (Get_constructor 0)           │
    │                                                          │          │    5: Call                          │
    │                                                          │          │    6: (Set_local 1)                 │
    │                                                          │          │    7: Get_this                      │
    │                                                          │          │    8: (Get_local 1)                 │
    │                                                          │          │    9: (Lookup_method 0)             │
    │                                                          │          │   10: Call                          │
    │                                                          │          │   11: Pop                           │
    │                                                          │          │   12: (Get_local 1)                 │
    │                                                          │          │   13: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ LinkedList.sum:                     │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_field 1)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Get_field 0)                 │
    │                                                          │          │    5: Dup                           │
    │                                                          │          │    6: (Branch_if_false 12)          │
    │                                                          │          │    7: (Set_local 0)                 │
    │                                                          │          │    8: (Get_local 0)                 │
    │                                                          │          │    9: (Lookup_method 2)             │
    │                                                          │          │   10: Call                          │
    │                                                          │          │   11: (Branch 14)                   │
    │                                                          │          │   12: Pop                           │
    │                                                          │          │   13: Const_0                       │
    │                                                          │          │   14: (Add Int)                     │
    │                                                          │          │   15: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class A { constructor () {} }                            │ (Int 1)  │ main:                               │
    │                                                          │          │    0: (Create_object 0)             │
    │ fun main() : bool {                                      │          │    1: (Get_constructor 0)           │
    │   let x = new A();                                       │          │    2: Call                          │
    │   x == x                                                 │          │    3: (Set_local 0)                 │
    │ }                                                        │          │    4: (Get_local 0)                 │
    │                                                          │          │    5: (Get_local 0)                 │
    │                                                          │          │    6: Cmp_eq                        │
    │                                                          │          │    7: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ A.constructor:                      │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Const_0                       │
    │                                                          │          │    2: (Update_this_vtable 0)        │
    │                                                          │          │    3: Pop                           │
    │                                                          │          │    4: Get_this                      │
    │                                                          │          │    5: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class A { constructor () {} }                            │ (Int 0)  │ main:                               │
    │                                                          │          │    0: (Create_object 0)             │
    │ fun main() : bool {                                      │          │    1: (Get_constructor 0)           │
    │   let x = new A();                                       │          │    2: Call                          │
    │   let y = new A();                                       │          │    3: (Set_local 0)                 │
    │   x == y                                                 │          │    4: (Create_object 0)             │
    │ }                                                        │          │    5: (Get_constructor 0)           │
    │                                                          │          │    6: Call                          │
    │                                                          │          │    7: (Set_local 1)                 │
    │                                                          │          │    8: (Get_local 0)                 │
    │                                                          │          │    9: (Get_local 1)                 │
    │                                                          │          │   10: Cmp_eq                        │
    │                                                          │          │   11: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ A.constructor:                      │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Const_0                       │
    │                                                          │          │    2: (Update_this_vtable 0)        │
    │                                                          │          │    3: Pop                           │
    │                                                          │          │    4: Get_this                      │
    │                                                          │          │    5: Return                        │
    │                                                          │          │                                     │
    ├──────────────────────────────────────────────────────────┼──────────┼─────────────────────────────────────┤
    │ class A {                                                │ (Int 42) │ main:                               │
    │   x : int;                                               │          │    0: (Const_int 20)                │
    │                                                          │          │    1: (Create_object 2)             │
    │   constructor(x : int) {                                 │          │    2: (Get_constructor 0)           │
    │     this.x = x;                                          │          │    3: Call                          │
    │   }                                                      │          │    4: (Set_local 0)                 │
    │ }                                                        │          │    5: (Const_int 22)                │
    │                                                          │          │    6: (Get_local 0)                 │
    │ class B < A {                                            │          │    7: (Get_evolver 1)               │
    │   y : int;                                               │          │    8: Call                          │
    │                                                          │          │    9: Dup                           │
    │   constructor(x : int, y : int) {                        │          │   10: (Branch_if_false 16)          │
    │     super(x);                                            │          │   11: (Set_local 1)                 │
    │     this.y = y;                                          │          │   12: (Get_local 1)                 │
    │   }                                                      │          │   13: (Lookup_method 0)             │
    │                                                          │          │   14: Call                          │
    │   constructor(y : int) evolves A {                       │          │   15: (Branch 19)                   │
    │     this.y = y;                                          │          │   16: Pop                           │
    │   }                                                      │          │   17: Const_1                       │
    │                                                          │          │   18: (Neg Int)                     │
    │   fun sum() : int {                                      │          │   19: Return                        │
    │     this.x + this.y                                      │          │                                     │
    │   }                                                      │          │ B.constructor:                      │
    │ }                                                        │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 1)                 │
    │ fun main() : int {                                       │          │    2: (Set_local 0)                 │
    │   let a = new A(20);                                     │          │    3: (Get_local 0)                 │
    │   if? b = a evolves B(22) {                              │          │    4: Get_this                      │
    │     b:sum()                                              │          │    5: (Get_constructor 0)           │
    │   } else {                                               │          │    6: Call                          │
    │     -1                                                   │          │    7: Pop                           │
    │   }                                                      │          │    8: (Get_local 1)                 │
    │ }                                                        │          │    9: Get_this                      │
    │                                                          │          │   10: (Set_field 1)                 │
    │                                                          │          │   11: Const_0                       │
    │                                                          │          │   12: (Update_this_vtable 1)        │
    │                                                          │          │   13: Pop                           │
    │                                                          │          │   14: Get_this                      │
    │                                                          │          │   15: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ A.constructor:                      │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: (Set_local 0)                 │
    │                                                          │          │    2: (Get_local 0)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Set_field 0)                 │
    │                                                          │          │    5: Const_0                       │
    │                                                          │          │    6: (Update_this_vtable 0)        │
    │                                                          │          │    7: Pop                           │
    │                                                          │          │    8: Get_this                      │
    │                                                          │          │    9: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ B.evolver:                          │
    │                                                          │          │    0: Dup                           │
    │                                                          │          │    1: (Branch_if_vtable_equals 5 0) │
    │                                                          │          │    2: Pop                           │
    │                                                          │          │    3: Const_0                       │
    │                                                          │          │    4: Return                        │
    │                                                          │          │    5: Set_this                      │
    │                                                          │          │    6: (Set_local 0)                 │
    │                                                          │          │    7: (Get_local 0)                 │
    │                                                          │          │    8: Get_this                      │
    │                                                          │          │    9: (Set_field 1)                 │
    │                                                          │          │   10: Const_0                       │
    │                                                          │          │   11: (Update_this_vtable 1)        │
    │                                                          │          │   12: Pop                           │
    │                                                          │          │   13: Get_this                      │
    │                                                          │          │   14: Return                        │
    │                                                          │          │                                     │
    │                                                          │          │ B.sum:                              │
    │                                                          │          │    0: Set_this                      │
    │                                                          │          │    1: Get_this                      │
    │                                                          │          │    2: (Get_field 0)                 │
    │                                                          │          │    3: Get_this                      │
    │                                                          │          │    4: (Get_field 1)                 │
    │                                                          │          │    5: (Add Int)                     │
    │                                                          │          │    6: Return                        │
    │                                                          │          │                                     │
    └──────────────────────────────────────────────────────────┴──────────┴─────────────────────────────────────┘
    |}]
;;

let%expect_test "bytecode_compiler_output_without_bytecode" =
  programs_without_bytecode
  |> List.map ~f:(test ~with_bytecode:false)
  |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌────────────────────────────────────┬──────────┐
    │ program                            │ result   │
    ├────────────────────────────────────┼──────────┤
    │ class A {                          │ (Int 42) │
    │   x : int;                         │          │
    │                                    │          │
    │   constructor(x : int) {           │          │
    │     this.x = x;                    │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ class B < A {                      │          │
    │   y : int;                         │          │
    │                                    │          │
    │   constructor(x : int, y : int) {  │          │
    │     super(x);                      │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   constructor(y : int) evolves A { │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   fun sum() : int {                │          │
    │     this.x + this.y                │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ fun main() : int {                 │          │
    │   let a = new A(20);               │          │
    │   if? b = a evolves B(22) {        │          │
    │     b:sum()                        │          │
    │   } else {                         │          │
    │     -1                             │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    ├────────────────────────────────────┼──────────┤
    │ class A {                          │ (Int 1)  │
    │   x : int;                         │          │
    │                                    │          │
    │   constructor(x : int) {           │          │
    │     this.x = x;                    │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ class B < A {                      │          │
    │   y : int;                         │          │
    │                                    │          │
    │   constructor(x : int, y : int) {  │          │
    │     super(x);                      │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   constructor(y : int) evolves A { │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   fun sum() : int {                │          │
    │     this.x + this.y                │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ fun main() : int {                 │          │
    │   let a = new A(20);               │          │
    │   if? b = a evolves B(22) {        │          │
    │     if? c = a evolves B(10) {      │          │
    │       2                            │          │
    │     } else {                       │          │
    │       1                            │          │
    │     }                              │          │
    │   } else {                         │          │
    │     0                              │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    ├────────────────────────────────────┼──────────┤
    │ class A {                          │ (Int 1)  │
    │   x : int;                         │          │
    │                                    │          │
    │   constructor(x : int) {           │          │
    │     this.x = x;                    │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ class B < A {                      │          │
    │   y : int;                         │          │
    │                                    │          │
    │   constructor(x : int, y : int) {  │          │
    │     super(x);                      │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   constructor(y : int) evolves A { │          │
    │     this.y = y;                    │          │
    │   }                                │          │
    │                                    │          │
    │   fun sum() : int {                │          │
    │     this.x + this.y                │          │
    │   }                                │          │
    │ }                                  │          │
    │                                    │          │
    │ fun main() : bool {                │          │
    │   let a = new A(20);               │          │
    │   let b = a evolves B(22).?;       │          │
    │   b == a                           │          │
    │ }                                  │          │
    │                                    │          │
    └────────────────────────────────────┴──────────┘
    |}]
;;
