open! Core
open! Async

let programs =
  [ "fun main(): unit { }"
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
    fun double(x : int): int { x * 2 }
    fun quadruple(x : int): int {double(double(x))}
    fun main(): int {quadruple(10)}
    |}
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
      A.B.add(34, 33)
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
  ; {|
    module Std {
      extern fun print([]char) : unit = "internal_may_print_string";
    }
    
    fun main() : int {
      Std.print("Hello world!\n");
      0
    }
    |}
  ; {|
    fun main() : int {
      return 10;
      return 20;
      0
    }
    |}
  ; {|
    module Std {
      module Int {
        extern fun println(int) : unit = "abort";
      }
    }

    fun main() : unit {
      let my_array = [1, 2, 3, 4, 5, 6];
      let my_sub_array = my_array[2..4];
      Std.Int.println(my_sub_array[0]);
      Std.Int.println(my_sub_array[1]);
      Std.Int.println(my_sub_array[2]);
      Std.Int.println(my_sub_array[3]);
    }
    |}
  ; {|
    fun intArray(): []mut int {
        new [3 + 3]int(5);
    }
    |}
  ; {|
    fun charArray(i : int): []mut char {
        new [i]char('a');
    }
    |}
  ]
;;

let ownership_programs =
  [ {|
    class A { public id: int; constructor(id: int) { this.id = id; } }
    fun main(): unit {
      let a_array: []mut !A = [ new A(1) ];
      let a: !A = new A(2);
      a >=< a_array[0];
      ()
    }
  |}
  ]
;;

let test ~mode program =
  let program = Utils.remove_indentation program in
  May.Resolved_ident.Global.Id.For_testing.reset_counter ();
  May.Type.Class_id.For_testing.reset_counter ();
  let check = May.Check.empty ~mode in
  let checked_ast =
    program
    |> May.For_testing.parse_string
    |> Result.bind ~f:(fun ast -> May.Check.check_decls check ~decls:ast)
    |> Result.map_error ~f:May.Comp_error.to_string
    |> Result.ok_or_failwith
  in
  let compiled_program, entry_point =
    May.Qbe_backend.Compilation_unit.compile_program ~check ~decls:checked_ast
  in
  let compiled_program_bytes =
    May.Qbe_backend.Compilation_unit.to_bytes compiled_program
  in
  let%bind qbe_process = Process.create_exn ~prog:"qbe" ~args:[] () in
  Writer.write_bytes (Process.stdin qbe_process) compiled_program_bytes;
  let%bind () = Writer.close (Process.stdin qbe_process) in
  let%bind asm_with_tab = Reader.contents (Process.stdout qbe_process) in
  let asm = asm_with_tab |> String.substr_replace_all ~pattern:"\t" ~with_:"  " in
  return
    [%sexp
      { program : string
      ; entry_point : string option
      ; compiled_program_bytes : bytes
      ; asm : string
      }]
;;

let%expect_test "qbe compiler generated code on sample programs" =
  let%bind () =
    programs
    |> Deferred.List.map ~how:`Sequential ~f:(test ~mode:May.Mode.Without)
    |> Deferred.map ~f:(Expectable.print ~separate_rows:true)
  in
  [%expect
    {|
    ┌──────────────────────────────────────────────────────┬─────────────┬──────────────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────┐
    │ program                                              │ entry_point │ compiled_program_bytes                                                       │ asm                                              │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun main(): unit { }                                 │ g_main_0    │ function w $g_main_0()                                                       │ .text                                            │
    │                                                      │             │ {                                                                            │ g_main_0:                                        │
    │                                                      │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         ret 0                                                                │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_0, @function                        │
    │                                                      │             │                                                                              │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │                                                                              │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun sum(): int {                                     │ g_main_1    │ function l $g_sum_0()                                                        │ .text                                            │
    │   let x = 10;                                        │             │ {                                                                            │ g_sum_0:                                         │
    │   let total = 0;                                     │             │ @start                                                                       │   pushq %rbp                                     │
    │   while (x > 0) {                                    │             │         %l_x_0 =l copy 10                                                    │   movq %rsp, %rbp                                │
    │     total = total + x;                               │             │         %l_total_1 =l copy 0                                                 │   movl $10, %ecx                                 │
    │     x = x - 1;                                       │             │         jmp @b_while_condition_0                                             │   movl $0, %eax                                  │
    │   }                                                  │             │                                                                              │ .Lbb2:                                           │
    │   total                                              │             │ @b_while_condition_0                                                         │   cmpq $0, %rcx                                  │
    │ }                                                    │             │         %t_1 =w csgtl %l_x_0, 0                                              │   jle .Lbb4                                      │
    │                                                      │             │         jnz %t_1, @b_while_block_2, @b_after_while_block_3                   │   addq %rcx, %rax                                │
    │ fun main() : int { sum() }                           │             │                                                                              │   subq $1, %rcx                                  │
    │                                                      │             │ @b_while_block_2                                                             │   jmp .Lbb2                                      │
    │                                                      │             │         %t_4 =l add %l_total_1, %l_x_0                                       │ .Lbb4:                                           │
    │                                                      │             │         %l_total_1 =l copy %t_4                                              │   leave                                          │
    │                                                      │             │         %t_5 =l sub %l_x_0, 1                                                │   ret                                            │
    │                                                      │             │         %l_x_0 =l copy %t_5                                                  │ .type g_sum_0, @function                         │
    │                                                      │             │         jmp @b_while_condition_0                                             │ .size g_sum_0, .-g_sum_0                         │
    │                                                      │             │                                                                              │ /* end function g_sum_0 */                       │
    │                                                      │             │ @b_after_while_block_3                                                       │                                                  │
    │                                                      │             │         ret %l_total_1                                                       │ .text                                            │
    │                                                      │             │                                                                              │ g_main_1:                                        │
    │                                                      │             │ }                                                                            │   pushq %rbp                                     │
    │                                                      │             │ function l $g_main_1()                                                       │   movq %rsp, %rbp                                │
    │                                                      │             │ {                                                                            │   callq g_sum_0                                  │
    │                                                      │             │ @start                                                                       │   leave                                          │
    │                                                      │             │         %t_0 =l call $g_sum_0()                                              │   ret                                            │
    │                                                      │             │         ret %t_0                                                             │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │ }                                                                            │ /* end function g_main_1 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun main() : char { 'A' - 'a' + 'c'}                 │ g_main_0    │ function ub $g_main_0()                                                      │ .text                                            │
    │                                                      │             │ {                                                                            │ g_main_0:                                        │
    │                                                      │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =w sub 65, 97                                                   │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_1 =w add %t_0, 99                                                 │   movl $67, %eax                                 │
    │                                                      │             │         ret %t_1                                                             │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │ }                                                                            │ .type g_main_0, @function                        │
    │                                                      │             │                                                                              │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │                                                                              │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun double(x : int): int { x * 2 }                   │ g_main_2    │ function l $g_double_0(l %l_x_0, )                                           │ .text                                            │
    │ fun quadruple(x : int): int {double(double(x))}      │             │ {                                                                            │ g_double_0:                                      │
    │ fun main(): int {quadruple(10)}                      │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =l mul %l_x_0, 2                                                │   movq %rsp, %rbp                                │
    │                                                      │             │         ret %t_0                                                             │   imulq $2, %rdi, %rax                           │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │ }                                                                            │   ret                                            │
    │                                                      │             │ function l $g_quadruple_1(l %l_x_0, )                                        │ .type g_double_0, @function                      │
    │                                                      │             │ {                                                                            │ .size g_double_0, .-g_double_0                   │
    │                                                      │             │ @start                                                                       │ /* end function g_double_0 */                    │
    │                                                      │             │         %t_0 =l call $g_double_0(l %l_x_0)                                   │                                                  │
    │                                                      │             │         %t_1 =l call $g_double_0(l %t_0)                                     │ .text                                            │
    │                                                      │             │         ret %t_1                                                             │ g_quadruple_1:                                   │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │ }                                                                            │   movq %rsp, %rbp                                │
    │                                                      │             │ function l $g_main_2()                                                       │   callq g_double_0                               │
    │                                                      │             │ {                                                                            │   movq %rax, %rdi                                │
    │                                                      │             │ @start                                                                       │   callq g_double_0                               │
    │                                                      │             │         %t_0 =l call $g_quadruple_1(l 10)                                    │   leave                                          │
    │                                                      │             │         ret %t_0                                                             │   ret                                            │
    │                                                      │             │                                                                              │ .type g_quadruple_1, @function                   │
    │                                                      │             │ }                                                                            │ .size g_quadruple_1, .-g_quadruple_1             │
    │                                                      │             │                                                                              │ /* end function g_quadruple_1 */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ g_main_2:                                        │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movl $10, %edi                                 │
    │                                                      │             │                                                                              │   callq g_quadruple_1                            │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_2, @function                        │
    │                                                      │             │                                                                              │ .size g_main_2, .-g_main_2                       │
    │                                                      │             │                                                                              │ /* end function g_main_2 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun main() : int {                                   │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   let a : []mut int = [2, 5, 7];                     │             │ {                                                                            │ g_main_0:                                        │
    │   a[0] = 10;                                         │             │ @start                                                                       │   pushq %rbp                                     │
    │   a[0] + a[2]                                        │             │         %t_0 =l call $malloc(l 16)                                           │   movq %rsp, %rbp                                │
    │ }                                                    │             │         %t_1 =l call $malloc(l 24)                                           │   subq $8, %rsp                                  │
    │                                                      │             │         %t_2 =l add %t_0, 0                                                  │   pushq %rbx                                     │
    │                                                      │             │         storel %t_1, %t_2                                                    │   movl $16, %edi                                 │
    │                                                      │             │         %t_3 =l add %t_0, 8                                                  │   callq malloc                                   │
    │                                                      │             │         storew 0, %t_3                                                       │   movq %rax, %rbx                                │
    │                                                      │             │         %t_4 =l add %t_0, 12                                                 │   movl $24, %edi                                 │
    │                                                      │             │         storew 3, %t_4                                                       │   callq malloc                                   │
    │                                                      │             │         %t_5 =l add %t_1, 0                                                  │   movq %rax, (%rbx)                              │
    │                                                      │             │         storel 2, %t_5                                                       │   movl $0, 8(%rbx)                               │
    │                                                      │             │         %t_6 =l add %t_1, 8                                                  │   movl $3, 12(%rbx)                              │
    │                                                      │             │         storel 5, %t_6                                                       │   movq $2, (%rax)                                │
    │                                                      │             │         %t_7 =l add %t_1, 16                                                 │   movq $5, 8(%rax)                               │
    │                                                      │             │         storel 7, %t_7                                                       │   movq $7, 16(%rax)                              │
    │                                                      │             │         %l_a_0 =l copy %t_0                                                  │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_8 =l add %l_a_0, 12                                               │   cmpq $0, %rax                                  │
    │                                                      │             │         %t_9 =l loaduw %t_8                                                  │   setg %al                                       │
    │                                                      │             │         %t_10 =w csgel 0, 0                                                  │   movzbl %al, %eax                               │
    │                                                      │             │         %t_11 =w csltl 0, %t_9                                               │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_12 =w mul %t_10, %t_11                                            │   cmpl $0, %eax                                  │
    │                                                      │             │         jnz %t_12, @b_good_index_14, @b_bad_index_13                         │   jnz .Lbb2                                      │
    │                                                      │             │                                                                              │   movl $5, %ecx                                  │
    │                                                      │             │ @b_bad_index_13                                                              │   movl $3, %edx                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 3, w 4, w 3, w 5)                  │   movl $4, %esi                                  │
    │                                                      │             │         hlt                                                                  │   movl $3, %edi                                  │
    │                                                      │             │                                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │ @b_good_index_14                                                             │   ud2                                            │
    │                                                      │             │         %t_15 =l loadl %l_a_0                                                │ .Lbb2:                                           │
    │                                                      │             │         %t_16 =l add %l_a_0, 8                                               │   movq (%rbx), %rax                              │
    │                                                      │             │         %t_17 =l loaduw %t_16                                                │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         %t_18 =l add 0, %t_17                                                │   addq $0, %rcx                                  │
    │                                                      │             │         %t_19 =l mul %t_18, 8                                                │   movq $10, (%rax, %rcx, 8)                      │
    │                                                      │             │         %t_20 =l add %t_15, %t_19                                            │   movl 12(%rbx), %esi                            │
    │                                                      │             │         %t_21 =l add %t_20, 0                                                │   cmpq $0, %rsi                                  │
    │                                                      │             │         storel 10, %t_21                                                     │   setg %al                                       │
    │                                                      │             │         %t_22 =l add %l_a_0, 12                                              │   movzbl %al, %eax                               │
    │                                                      │             │         %t_23 =l loaduw %t_22                                                │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_24 =w csgel 0, 0                                                  │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_25 =w csltl 0, %t_23                                              │   jnz .Lbb4                                      │
    │                                                      │             │         %t_26 =w mul %t_24, %t_25                                            │   movl $5, %ecx                                  │
    │                                                      │             │         jnz %t_26, @b_good_index_28, @b_bad_index_27                         │   movl $4, %edx                                  │
    │                                                      │             │                                                                              │   movl $4, %esi                                  │
    │                                                      │             │ @b_bad_index_27                                                              │   movl $4, %edi                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 4, w 4, w 4, w 5)                  │   callq panic_index_out_of_bounds                │
    │                                                      │             │         hlt                                                                  │   ud2                                            │
    │                                                      │             │                                                                              │ .Lbb4:                                           │
    │                                                      │             │ @b_good_index_28                                                             │   movq (%rbx), %rcx                              │
    │                                                      │             │         %t_29 =l loadl %l_a_0                                                │   movl 8(%rbx), %edx                             │
    │                                                      │             │         %t_30 =l add %l_a_0, 8                                               │   movl $0, %eax                                  │
    │                                                      │             │         %t_31 =l loaduw %t_30                                                │   addq %rdx, %rax                                │
    │                                                      │             │         %t_32 =l add 0, %t_31                                                │   movq (%rcx, %rax, 8), %rax                     │
    │                                                      │             │         %t_33 =l mul %t_32, 8                                                │   cmpq $2, %rsi                                  │
    │                                                      │             │         %t_34 =l add %t_29, %t_33                                            │   setg %sil                                      │
    │                                                      │             │         %t_35 =l loadl %t_34                                                 │   movzbl %sil, %esi                              │
    │                                                      │             │         %t_36 =l add %l_a_0, 12                                              │   imull $1, %esi, %esi                           │
    │                                                      │             │         %t_37 =l loaduw %t_36                                                │   cmpl $0, %esi                                  │
    │                                                      │             │         %t_38 =w csgel 2, 0                                                  │   jnz .Lbb6                                      │
    │                                                      │             │         %t_39 =w csltl 2, %t_37                                              │   movl $12, %ecx                                 │
    │                                                      │             │         %t_40 =w mul %t_38, %t_39                                            │   movl $4, %edx                                  │
    │                                                      │             │         jnz %t_40, @b_good_index_42, @b_bad_index_41                         │   movl $11, %esi                                 │
    │                                                      │             │                                                                              │   movl $4, %edi                                  │
    │                                                      │             │ @b_bad_index_41                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │         call $panic_index_out_of_bounds(w 4, w 11, w 4, w 12)                │   ud2                                            │
    │                                                      │             │         hlt                                                                  │ .Lbb6:                                           │
    │                                                      │             │                                                                              │   addq $2, %rdx                                  │
    │                                                      │             │ @b_good_index_42                                                             │   movq (%rcx, %rdx, 8), %rcx                     │
    │                                                      │             │         %t_43 =l loadl %l_a_0                                                │   addq %rcx, %rax                                │
    │                                                      │             │         %t_44 =l add %l_a_0, 8                                               │   popq %rbx                                      │
    │                                                      │             │         %t_45 =l loaduw %t_44                                                │   leave                                          │
    │                                                      │             │         %t_46 =l add 2, %t_45                                                │   ret                                            │
    │                                                      │             │         %t_47 =l mul %t_46, 8                                                │ .type g_main_0, @function                        │
    │                                                      │             │         %t_48 =l add %t_43, %t_47                                            │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │         %t_49 =l loadl %t_48                                                 │ /* end function g_main_0 */                      │
    │                                                      │             │         %t_50 =l add %t_35, %t_49                                            │                                                  │
    │                                                      │             │         ret %t_50                                                            │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ module A {                                           │ g_main_1    │ function l $g_A_B_add_0(l %l_x_0, l %l_y_1, )                                │ .text                                            │
    │   module B {                                         │             │ {                                                                            │ g_A_B_add_0:                                     │
    │     fun add(x : int, y : int): int {                 │             │ @start                                                                       │   pushq %rbp                                     │
    │       x + y + 1                                      │             │         %t_0 =l add %l_x_0, %l_y_1                                           │   movq %rsp, %rbp                                │
    │     }                                                │             │         %t_1 =l add %t_0, 1                                                  │   movq %rdi, %rax                                │
    │   }                                                  │             │         ret %t_1                                                             │   addq %rsi, %rax                                │
    │ }                                                    │             │                                                                              │   addq $1, %rax                                  │
    │                                                      │             │ }                                                                            │   leave                                          │
    │ fun main() : int {                                   │             │ function l $g_main_1()                                                       │   ret                                            │
    │   A.B.add(34, 33)                                    │             │ {                                                                            │ .type g_A_B_add_0, @function                     │
    │ }                                                    │             │ @start                                                                       │ .size g_A_B_add_0, .-g_A_B_add_0                 │
    │                                                      │             │         %t_0 =l call $g_A_B_add_0(l 34, l 33)                                │ /* end function g_A_B_add_0 */                   │
    │                                                      │             │         ret %t_0                                                             │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │ }                                                                            │ g_main_1:                                        │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movl $33, %esi                                 │
    │                                                      │             │                                                                              │   movl $34, %edi                                 │
    │                                                      │             │                                                                              │   callq g_A_B_add_0                              │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │                                                                              │ /* end function g_main_1 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class Int {                                          │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   x : int;                                           │             │ {                                                                            │ g_main_0:                                        │
    │                                                      │             │ @start                                                                       │   pushq %rbp                                     │
    │   constructor(x:int) {                               │             │         %t_0 =l call $malloc(l 24)                                           │   movq %rsp, %rbp                                │
    │     this.x = x;                                      │             │         %t_1 =l add %t_0, 0                                                  │   pushq %rbx                                     │
    │   }                                                  │             │         storel 0, %t_1                                                       │   pushq %r12                                     │
    │                                                      │             │         call $constructor_Counter(l %t_0, l 24)                              │   movl $24, %edi                                 │
    │   fun get(): int {                                   │             │         %l_x_0 =l copy %t_0                                                  │   callq malloc                                   │
    │     this.x                                           │             │         %t_2 =l loadl %l_x_0                                                 │   movq %rax, %rdi                                │
    │   }                                                  │             │         %t_3 =l add %t_2, 16                                                 │   movq $0, (%rdi)                                │
    │                                                      │             │         %t_4 =l loadl %t_3                                                   │   movl $24, %esi                                 │
    │   fun add(other: Int) : Int {                        │             │         %t_5 =l call %t_4(l %l_x_0)                                          │   movq %rdi, %rbx                                │
    │     let a = this:get();                              │             │         %t_6 =l call $malloc(l 24)                                           │   callq constructor_Counter                      │
    │     let b = other:get();                             │             │         %t_7 =l add %t_6, 0                                                  │   movq %rbx, %rdi                                │
    │                                                      │             │         storel 0, %t_7                                                       │   movq (%rdi), %rax                              │
    │     new Int(a + b)                                   │             │         call $constructor_Counter(l %t_6, l 16)                              │   movq 16(%rax), %rax                            │
    │   }                                                  │             │         %l_y_1 =l copy %t_6                                                  │   movq %rdi, %rbx                                │
    │ }                                                    │             │         %t_8 =l loadl %l_y_1                                                 │   callq *%rax                                    │
    │                                                      │             │         %t_9 =l add %t_8, 16                                                 │   movq %rbx, %rdi                                │
    │ class Counter < Int {                                │             │         %t_10 =l loadl %t_9                                                  │   movq %rdi, %rbx                                │
    │   mut count : int;                                   │             │         %t_11 =l call %t_10(l %l_y_1)                                        │   movl $24, %edi                                 │
    │                                                      │             │         %t_12 =l loadl %l_x_0                                                │   callq malloc                                   │
    │   constructor(x : int) {                             │             │         %t_13 =l add %t_12, 0                                                │   movq %rbx, %rdi                                │
    │     super(x);                                        │             │         %t_14 =l loadl %t_13                                                 │   movq %rax, %rsi                                │
    │     this.count = 0;                                  │             │         %t_15 =l call %t_14(l %l_x_0, l %l_y_1)                              │   movq $0, (%rsi)                                │
    │   }                                                  │             │         %l_z_2 =l copy %t_15                                                 │   movq %rsi, %r12                                │
    │                                                      │             │         %t_16 =l loadl %l_z_2                                                │   movl $16, %esi                                 │
    │   overrides fun get(): int {                         │             │         %t_17 =l add %t_16, 8                                                │   movq %rdi, %rbx                                │
    │     this.x + this.count                              │             │         %t_18 =l loadl %t_17                                                 │   movq %r12, %rdi                                │
    │   }                                                  │             │         %t_19 =l call %t_18(l %l_z_2)                                        │   callq constructor_Counter                      │
    │                                                      │             │         ret %t_19                                                            │   movq %rbx, %rdi                                │
    │   fun inc(): unit {                                  │             │                                                                              │   movq (%r12), %rax                              │
    │     this.count = this.count + 1;                     │             │ }                                                                            │   movq 16(%rax), %rax                            │
    │   }                                                  │             │ function $constructor_Int(l %this, l %l_x_0, )                               │   movq %rdi, %rbx                                │
    │ }                                                    │             │ {                                                                            │   movq %r12, %rdi                                │
    │                                                      │             │ @start                                                                       │   callq *%rax                                    │
    │ fun main() : int {                                   │             │         %t_0 =l add %this, 8                                                 │   movq %r12, %rsi                                │
    │   let x = new Counter(24);                           │             │         storel %l_x_0, %t_0                                                  │   movq %rbx, %rdi                                │
    │   x:inc();                                           │             │         %t_1 =l add %this, 0                                                 │   movq (%rdi), %rax                              │
    │   let y = new Counter(16);                           │             │         storel $vtable_Int, %t_1                                             │   movq (%rax), %rax                              │
    │   y:inc();                                           │             │         ret                                                                  │   callq *%rax                                    │
    │   let z = x:add(y);                                  │             │                                                                              │   movq %rax, %rdi                                │
    │   z:get()                                            │             │ }                                                                            │   movq (%rdi), %rax                              │
    │ }                                                    │             │ function l $method_Int_get(l %this, )                                        │   movq 8(%rax), %rax                             │
    │                                                      │             │ {                                                                            │   callq *%rax                                    │
    │                                                      │             │ @start                                                                       │   popq %r12                                      │
    │                                                      │             │         %t_0 =l add %this, 8                                                 │   popq %rbx                                      │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │   leave                                          │
    │                                                      │             │         ret %t_1                                                             │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_0, @function                        │
    │                                                      │             │ }                                                                            │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │ function l $method_Int_add(l %this, l %l_other_0, )                          │ /* end function g_main_0 */                      │
    │                                                      │             │ {                                                                            │                                                  │
    │                                                      │             │ @start                                                                       │ .text                                            │
    │                                                      │             │         %t_0 =l loadl %this                                                  │ constructor_Int:                                 │
    │                                                      │             │         %t_1 =l add %t_0, 8                                                  │   pushq %rbp                                     │
    │                                                      │             │         %t_2 =l loadl %t_1                                                   │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_3 =l call %t_2(l %this)                                           │   movq %rsi, 8(%rdi)                             │
    │                                                      │             │         %l_a_1 =l copy %t_3                                                  │   leaq vtable_Int(%rip), %rax                    │
    │                                                      │             │         %t_4 =l loadl %l_other_0                                             │   movq %rax, (%rdi)                              │
    │                                                      │             │         %t_5 =l add %t_4, 8                                                  │   leave                                          │
    │                                                      │             │         %t_6 =l loadl %t_5                                                   │   ret                                            │
    │                                                      │             │         %t_7 =l call %t_6(l %l_other_0)                                      │ .type constructor_Int, @function                 │
    │                                                      │             │         %l_b_2 =l copy %t_7                                                  │ .size constructor_Int, .-constructor_Int         │
    │                                                      │             │         %t_8 =l add %l_a_1, %l_b_2                                           │ /* end function constructor_Int */               │
    │                                                      │             │         %t_9 =l call $malloc(l 24)                                           │                                                  │
    │                                                      │             │         %t_10 =l add %t_9, 0                                                 │ .text                                            │
    │                                                      │             │         storel 0, %t_10                                                      │ method_Int_get:                                  │
    │                                                      │             │         call $constructor_Int(l %t_9, l %t_8)                                │   pushq %rbp                                     │
    │                                                      │             │         ret %t_9                                                             │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq 8(%rdi), %rax                             │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │ data $vtable_Int = { l $method_Int_add, l $method_Int_get, }                 │   ret                                            │
    │                                                      │             │ function $constructor_Counter(l %this, l %l_x_0, )                           │ .type method_Int_get, @function                  │
    │                                                      │             │ {                                                                            │ .size method_Int_get, .-method_Int_get           │
    │                                                      │             │ @start                                                                       │ /* end function method_Int_get */                │
    │                                                      │             │         %t_0 =w call $constructor_Int(l %this, l %l_x_0)                     │                                                  │
    │                                                      │             │         %t_1 =l add %this, 16                                                │ .text                                            │
    │                                                      │             │         storel 0, %t_1                                                       │ method_Int_add:                                  │
    │                                                      │             │         %t_2 =l add %this, 0                                                 │   pushq %rbp                                     │
    │                                                      │             │         storel $vtable_Counter, %t_2                                         │   movq %rsp, %rbp                                │
    │                                                      │             │         ret                                                                  │   subq $8, %rsp                                  │
    │                                                      │             │                                                                              │   pushq %rbx                                     │
    │                                                      │             │ }                                                                            │   movq %rsi, %rbx                                │
    │                                                      │             │ function l $method_Counter_get(l %this, )                                    │   movq (%rdi), %rax                              │
    │                                                      │             │ {                                                                            │   movq 8(%rax), %rax                             │
    │                                                      │             │ @start                                                                       │   callq *%rax                                    │
    │                                                      │             │         %t_0 =l add %this, 8                                                 │   movq %rbx, %rdi                                │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │   movq %rax, %rbx                                │
    │                                                      │             │         %t_2 =l add %this, 16                                                │   movq (%rdi), %rax                              │
    │                                                      │             │         %t_3 =l loadl %t_2                                                   │   movq 8(%rax), %rax                             │
    │                                                      │             │         %t_4 =l add %t_1, %t_3                                               │   callq *%rax                                    │
    │                                                      │             │         ret %t_4                                                             │   addq %rax, %rbx                                │
    │                                                      │             │                                                                              │   movl $24, %edi                                 │
    │                                                      │             │ }                                                                            │   callq malloc                                   │
    │                                                      │             │ function w $method_Counter_inc(l %this, )                                    │   movq %rbx, %rsi                                │
    │                                                      │             │ {                                                                            │   movq %rax, %rbx                                │
    │                                                      │             │ @start                                                                       │   movq $0, (%rbx)                                │
    │                                                      │             │         %t_0 =l add %this, 16                                                │   movq %rbx, %rdi                                │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │   callq constructor_Int                          │
    │                                                      │             │         %t_2 =l add %t_1, 1                                                  │   movq %rbx, %rax                                │
    │                                                      │             │         %t_3 =l add %this, 16                                                │   popq %rbx                                      │
    │                                                      │             │         storel %t_2, %t_3                                                    │   leave                                          │
    │                                                      │             │         ret 0                                                                │   ret                                            │
    │                                                      │             │                                                                              │ .type method_Int_add, @function                  │
    │                                                      │             │ }                                                                            │ .size method_Int_add, .-method_Int_add           │
    │                                                      │             │ data $vtable_Counter = { l $method_Int_add, l $method_Counter_get, l         │ /* end function method_Int_add */                │
    │                                                      │             │ $method_Counter_inc, }                                                       │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_Int:                                      │
    │                                                      │             │                                                                              │   .quad method_Int_add+0                         │
    │                                                      │             │                                                                              │   .quad method_Int_get+0                         │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ constructor_Counter:                             │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   subq $8, %rsp                                  │
    │                                                      │             │                                                                              │   pushq %rbx                                     │
    │                                                      │             │                                                                              │   movq %rdi, %rbx                                │
    │                                                      │             │                                                                              │   callq constructor_Int                          │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │                                                                              │   movq $0, 16(%rdi)                              │
    │                                                      │             │                                                                              │   leaq vtable_Counter(%rip), %rax                │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │                                                                              │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type constructor_Counter, @function             │
    │                                                      │             │                                                                              │ .size constructor_Counter, .-constructor_Counter │
    │                                                      │             │                                                                              │ /* end function constructor_Counter */           │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ method_Counter_get:                              │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq 8(%rdi), %rax                             │
    │                                                      │             │                                                                              │   movq 16(%rdi), %rcx                            │
    │                                                      │             │                                                                              │   addq %rcx, %rax                                │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_Counter_get, @function              │
    │                                                      │             │                                                                              │ .size method_Counter_get, .-method_Counter_get   │
    │                                                      │             │                                                                              │ /* end function method_Counter_get */            │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ method_Counter_inc:                              │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq 16(%rdi), %rax                            │
    │                                                      │             │                                                                              │   addq $1, %rax                                  │
    │                                                      │             │                                                                              │   movq %rax, 16(%rdi)                            │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_Counter_inc, @function              │
    │                                                      │             │                                                                              │ .size method_Counter_inc, .-method_Counter_inc   │
    │                                                      │             │                                                                              │ /* end function method_Counter_inc */            │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_Counter:                                  │
    │                                                      │             │                                                                              │   .quad method_Int_add+0                         │
    │                                                      │             │                                                                              │   .quad method_Counter_get+0                     │
    │                                                      │             │                                                                              │   .quad method_Counter_inc+0                     │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class A {                                            │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   constructor () {}                                  │             │ {                                                                            │ g_main_0:                                        │
    │   fun m() : int { 5 }                                │             │ @start                                                                       │   pushq %rbp                                     │
    │ }                                                    │             │         %t_0 =l call $malloc(l 8)                                            │   movq %rsp, %rbp                                │
    │ class B < A {                                        │             │         %t_1 =l add %t_0, 0                                                  │   subq $8, %rsp                                  │
    │   constructor () { super() }                         │             │         storel 0, %t_1                                                       │   pushq %rbx                                     │
    │   overrides fun m() : int { super:m() + 10}          │             │         call $constructor_B(l %t_0)                                          │   movl $8, %edi                                  │
    │ }                                                    │             │         %t_2 =l loadl %t_0                                                   │   callq malloc                                   │
    │ fun main() : int {                                   │             │         %t_3 =l add %t_2, 0                                                  │   movq %rax, %rdi                                │
    │   (new B()):m()                                      │             │         %t_4 =l loadl %t_3                                                   │   movq $0, (%rdi)                                │
    │ }                                                    │             │         %t_5 =l call %t_4(l %t_0)                                            │   movq %rdi, %rbx                                │
    │                                                      │             │         ret %t_5                                                             │   callq constructor_B                            │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │ }                                                                            │   movq (%rdi), %rax                              │
    │                                                      │             │ function $constructor_A(l %this, )                                           │   movq (%rax), %rax                              │
    │                                                      │             │ {                                                                            │   callq *%rax                                    │
    │                                                      │             │ @start                                                                       │   popq %rbx                                      │
    │                                                      │             │         %t_0 =l add %this, 0                                                 │   leave                                          │
    │                                                      │             │         storel $vtable_A, %t_0                                               │   ret                                            │
    │                                                      │             │         ret                                                                  │ .type g_main_0, @function                        │
    │                                                      │             │                                                                              │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │ }                                                                            │ /* end function g_main_0 */                      │
    │                                                      │             │ function l $method_A_m(l %this, )                                            │                                                  │
    │                                                      │             │ {                                                                            │ .text                                            │
    │                                                      │             │ @start                                                                       │ constructor_A:                                   │
    │                                                      │             │         ret 5                                                                │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │ }                                                                            │   leaq vtable_A(%rip), %rax                      │
    │                                                      │             │ data $vtable_A = { l $method_A_m, }                                          │   movq %rax, (%rdi)                              │
    │                                                      │             │ function $constructor_B(l %this, )                                           │   leave                                          │
    │                                                      │             │ {                                                                            │   ret                                            │
    │                                                      │             │ @start                                                                       │ .type constructor_A, @function                   │
    │                                                      │             │         %t_0 =w call $constructor_A(l %this)                                 │ .size constructor_A, .-constructor_A             │
    │                                                      │             │         %t_1 =l add %this, 0                                                 │ /* end function constructor_A */                 │
    │                                                      │             │         storel $vtable_B, %t_1                                               │                                                  │
    │                                                      │             │         ret                                                                  │ .text                                            │
    │                                                      │             │                                                                              │ method_A_m:                                      │
    │                                                      │             │ }                                                                            │   pushq %rbp                                     │
    │                                                      │             │ function l $method_B_m(l %this, )                                            │   movq %rsp, %rbp                                │
    │                                                      │             │ {                                                                            │   movl $5, %eax                                  │
    │                                                      │             │ @start                                                                       │   leave                                          │
    │                                                      │             │         call $method_A_m(l %this)                                            │   ret                                            │
    │                                                      │             │         %t_0 =l add 0, 10                                                    │ .type method_A_m, @function                      │
    │                                                      │             │         ret %t_0                                                             │ .size method_A_m, .-method_A_m                   │
    │                                                      │             │                                                                              │ /* end function method_A_m */                    │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │ data $vtable_B = { l $method_B_m, }                                          │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .quad method_A_m+0                             │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ constructor_B:                                   │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   subq $8, %rsp                                  │
    │                                                      │             │                                                                              │   pushq %rbx                                     │
    │                                                      │             │                                                                              │   movq %rdi, %rbx                                │
    │                                                      │             │                                                                              │   callq constructor_A                            │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │                                                                              │   leaq vtable_B(%rip), %rax                      │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │                                                                              │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type constructor_B, @function                   │
    │                                                      │             │                                                                              │ .size constructor_B, .-constructor_B             │
    │                                                      │             │                                                                              │ /* end function constructor_B */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ method_B_m:                                      │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   callq method_A_m                               │
    │                                                      │             │                                                                              │   movl $10, %eax                                 │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_B_m, @function                      │
    │                                                      │             │                                                                              │ .size method_B_m, .-method_B_m                   │
    │                                                      │             │                                                                              │ /* end function method_B_m */                    │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_B:                                        │
    │                                                      │             │                                                                              │   .quad method_B_m+0                             │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class LinkedList {                                   │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   mut v : int;                                       │             │ {                                                                            │ g_main_0:                                        │
    │   mut next : ?LinkedList;                            │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =l call $malloc(l 24)                                           │   movq %rsp, %rbp                                │
    │   constructor(v : int) {                             │             │         %t_1 =l add %t_0, 0                                                  │   subq $8, %rsp                                  │
    │     this.v = v;                                      │             │         storel 0, %t_1                                                       │   pushq %rbx                                     │
    │     this.next = null;                                │             │         call $constructor_LinkedList(l %t_0, l 4)                            │   movl $24, %edi                                 │
    │   }                                                  │             │         %t_2 =l loadl %t_0                                                   │   callq malloc                                   │
    │                                                      │             │         %t_3 =l add %t_2, 8                                                  │   movq %rax, %rdi                                │
    │   fun setNext(next : ?LinkedList) : unit {           │             │         %t_4 =l loadl %t_3                                                   │   movq $0, (%rdi)                                │
    │     this.next = next;                                │             │         %t_5 =l call %t_4(l %t_0, l 2)                                       │   movl $4, %esi                                  │
    │   }                                                  │             │         %t_6 =l loadl %t_5                                                   │   movq %rdi, %rbx                                │
    │                                                      │             │         %t_7 =l add %t_6, 8                                                  │   callq constructor_LinkedList                   │
    │   fun snoc(v : int) : LinkedList {                   │             │         %t_8 =l loadl %t_7                                                   │   movq %rbx, %rdi                                │
    │     let new_head = new LinkedList(v);                │             │         %t_9 =l call %t_8(l %t_5, l 7)                                       │   movq (%rdi), %rax                              │
    │     new_head:setNext(this);                          │             │         %t_10 =l neg 1                                                       │   movq 8(%rax), %rax                             │
    │     new_head                                         │             │         %t_11 =l loadl %t_9                                                  │   movl $2, %esi                                  │
    │   }                                                  │             │         %t_12 =l add %t_11, 8                                                │   callq *%rax                                    │
    │                                                      │             │         %t_13 =l loadl %t_12                                                 │   movq %rax, %rdi                                │
    │   fun sum() : int {                                  │             │         %t_14 =l call %t_13(l %t_9, l %t_10)                                 │   movq (%rdi), %rax                              │
    │     this.v + (if? n = this.next { n:sum() } else {   │             │         %l_list_0 =l copy %t_14                                              │   movq 8(%rax), %rax                             │
    │ 0 })                                                 │             │         %t_15 =l loadl %l_list_0                                             │   movl $7, %esi                                  │
    │   }                                                  │             │         %t_16 =l add %t_15, 16                                               │   callq *%rax                                    │
    │ }                                                    │             │         %t_17 =l loadl %t_16                                                 │   movq %rax, %rdi                                │
    │                                                      │             │         %t_18 =l call %t_17(l %l_list_0)                                     │   movq (%rdi), %rax                              │
    │ fun main() : int {                                   │             │         ret %t_18                                                            │   movq 8(%rax), %rax                             │
    │   let list = new                                     │             │                                                                              │   movq $-1, %rsi                                 │
    │ LinkedList(4):snoc(2):snoc(7):snoc(-1);              │             │ }                                                                            │   callq *%rax                                    │
    │   list:sum()                                         │             │ function $constructor_LinkedList(l %this, l %l_v_0, )                        │   movq %rax, %rdi                                │
    │ }                                                    │             │ {                                                                            │   movq (%rdi), %rax                              │
    │                                                      │             │ @start                                                                       │   movq 16(%rax), %rax                            │
    │                                                      │             │         %t_0 =l add %this, 16                                                │   callq *%rax                                    │
    │                                                      │             │         storel %l_v_0, %t_0                                                  │   popq %rbx                                      │
    │                                                      │             │         %t_1 =l add %this, 8                                                 │   leave                                          │
    │                                                      │             │         storel 0, %t_1                                                       │   ret                                            │
    │                                                      │             │         %t_2 =l add %this, 0                                                 │ .type g_main_0, @function                        │
    │                                                      │             │         storel $vtable_LinkedList, %t_2                                      │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │         ret                                                                  │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │ .text                                            │
    │                                                      │             │ function w $method_LinkedList_setNext(l %this, l %l_next_0, )                │ constructor_LinkedList:                          │
    │                                                      │             │ {                                                                            │   pushq %rbp                                     │
    │                                                      │             │ @start                                                                       │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_0 =l add %this, 8                                                 │   movq %rsi, 16(%rdi)                            │
    │                                                      │             │         storel %l_next_0, %t_0                                               │   movq $0, 8(%rdi)                               │
    │                                                      │             │         ret 0                                                                │   leaq vtable_LinkedList(%rip), %rax             │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │ function l $method_LinkedList_snoc(l %this, l %l_v_0, )                      │   ret                                            │
    │                                                      │             │ {                                                                            │ .type constructor_LinkedList, @function          │
    │                                                      │             │ @start                                                                       │ .size constructor_LinkedList,                    │
    │                                                      │             │         %t_0 =l call $malloc(l 24)                                           │ .-constructor_LinkedList                         │
    │                                                      │             │         %t_1 =l add %t_0, 0                                                  │ /* end function constructor_LinkedList */        │
    │                                                      │             │         storel 0, %t_1                                                       │                                                  │
    │                                                      │             │         call $constructor_LinkedList(l %t_0, l %l_v_0)                       │ .text                                            │
    │                                                      │             │         %l_new_head_1 =l copy %t_0                                           │ method_LinkedList_setNext:                       │
    │                                                      │             │         %t_2 =l loadl %l_new_head_1                                          │   pushq %rbp                                     │
    │                                                      │             │         %t_3 =l add %t_2, 0                                                  │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_4 =l loadl %t_3                                                   │   movq %rsi, 8(%rdi)                             │
    │                                                      │             │         %t_5 =l call %t_4(l %l_new_head_1, l %this)                          │   movl $0, %eax                                  │
    │                                                      │             │         ret %l_new_head_1                                                    │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │ }                                                                            │ .type method_LinkedList_setNext, @function       │
    │                                                      │             │ function l $method_LinkedList_sum(l %this, )                                 │ .size method_LinkedList_setNext,                 │
    │                                                      │             │ {                                                                            │ .-method_LinkedList_setNext                      │
    │                                                      │             │ @start                                                                       │ /* end function method_LinkedList_setNext */     │
    │                                                      │             │         %t_0 =l add %this, 16                                                │                                                  │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │ .text                                            │
    │                                                      │             │         %t_2 =l add %this, 8                                                 │ method_LinkedList_snoc:                          │
    │                                                      │             │         %t_3 =l loadl %t_2                                                   │   pushq %rbp                                     │
    │                                                      │             │         jnz %t_3, @b_if_value_4, @b_if_null_5                                │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   pushq %rbx                                     │
    │                                                      │             │ @b_if_value_4                                                                │   pushq %r12                                     │
    │                                                      │             │         %l_n_0 =l copy %t_3                                                  │   movq %rsi, %rbx                                │
    │                                                      │             │         %t_7 =l loadl %l_n_0                                                 │   movq %rdi, %r12                                │
    │                                                      │             │         %t_8 =l add %t_7, 16                                                 │   movl $24, %edi                                 │
    │                                                      │             │         %t_9 =l loadl %t_8                                                   │   callq malloc                                   │
    │                                                      │             │         %t_10 =l call %t_9(l %l_n_0)                                         │   movq %rbx, %rsi                                │
    │                                                      │             │         %t_11 =l copy %t_10                                                  │   movq %rax, %rbx                                │
    │                                                      │             │         jmp @b_after_ifq_6                                                   │   movq $0, (%rbx)                                │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │ @b_if_null_5                                                                 │   callq constructor_LinkedList                   │
    │                                                      │             │         %t_11 =l copy 0                                                      │   movq %r12, %rsi                                │
    │                                                      │             │         jmp @b_after_ifq_6                                                   │   movq %rbx, %rax                                │
    │                                                      │             │                                                                              │   movq %rax, %rbx                                │
    │                                                      │             │ @b_after_ifq_6                                                               │   movq (%rax), %rax                              │
    │                                                      │             │         %t_12 =l add %t_1, %t_11                                             │   movq (%rax), %rax                              │
    │                                                      │             │         ret %t_12                                                            │   movq %rbx, %rdi                                │
    │                                                      │             │                                                                              │   callq *%rax                                    │
    │                                                      │             │ }                                                                            │   movq %rbx, %rax                                │
    │                                                      │             │ data $vtable_LinkedList = { l $method_LinkedList_setNext, l                  │   popq %r12                                      │
    │                                                      │             │ $method_LinkedList_snoc, l $method_LinkedList_sum, }                         │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_LinkedList_snoc, @function          │
    │                                                      │             │                                                                              │ .size method_LinkedList_snoc,                    │
    │                                                      │             │                                                                              │ .-method_LinkedList_snoc                         │
    │                                                      │             │                                                                              │ /* end function method_LinkedList_snoc */        │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ method_LinkedList_sum:                           │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   subq $8, %rsp                                  │
    │                                                      │             │                                                                              │   pushq %rbx                                     │
    │                                                      │             │                                                                              │   movq 16(%rdi), %rbx                            │
    │                                                      │             │                                                                              │   movq 8(%rdi), %rdi                             │
    │                                                      │             │                                                                              │   cmpl $0, %edi                                  │
    │                                                      │             │                                                                              │   jnz .Lbb10                                     │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │                                                                              │   jmp .Lbb11                                     │
    │                                                      │             │                                                                              │ .Lbb10:                                          │
    │                                                      │             │                                                                              │   movq (%rdi), %rax                              │
    │                                                      │             │                                                                              │   movq 16(%rax), %rax                            │
    │                                                      │             │                                                                              │   callq *%rax                                    │
    │                                                      │             │                                                                              │ .Lbb11:                                          │
    │                                                      │             │                                                                              │   addq %rbx, %rax                                │
    │                                                      │             │                                                                              │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_LinkedList_sum, @function           │
    │                                                      │             │                                                                              │ .size method_LinkedList_sum,                     │
    │                                                      │             │                                                                              │ .-method_LinkedList_sum                          │
    │                                                      │             │                                                                              │ /* end function method_LinkedList_sum */         │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_LinkedList:                               │
    │                                                      │             │                                                                              │   .quad method_LinkedList_setNext+0              │
    │                                                      │             │                                                                              │   .quad method_LinkedList_snoc+0                 │
    │                                                      │             │                                                                              │   .quad method_LinkedList_sum+0                  │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class A { constructor () {} }                        │ g_main_0    │ function ub $g_main_0()                                                      │ .text                                            │
    │                                                      │             │ {                                                                            │ g_main_0:                                        │
    │ fun main() : bool {                                  │             │ @start                                                                       │   pushq %rbp                                     │
    │   let x = new A();                                   │             │         %t_0 =l call $malloc(l 8)                                            │   movq %rsp, %rbp                                │
    │   x == x                                             │             │         %t_1 =l add %t_0, 0                                                  │   subq $8, %rsp                                  │
    │ }                                                    │             │         storel 0, %t_1                                                       │   pushq %rbx                                     │
    │                                                      │             │         call $constructor_A(l %t_0)                                          │   movl $8, %edi                                  │
    │                                                      │             │         %l_x_0 =l copy %t_0                                                  │   callq malloc                                   │
    │                                                      │             │         %t_2 =w ceql %l_x_0, %l_x_0                                          │   movq %rax, %rbx                                │
    │                                                      │             │         ret %t_2                                                             │   movq $0, (%rbx)                                │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │ }                                                                            │   callq constructor_A                            │
    │                                                      │             │ function $constructor_A(l %this, )                                           │   cmpq %rbx, %rbx                                │
    │                                                      │             │ {                                                                            │   setz %al                                       │
    │                                                      │             │ @start                                                                       │   movzbl %al, %eax                               │
    │                                                      │             │         %t_0 =l add %this, 0                                                 │   popq %rbx                                      │
    │                                                      │             │         storel $vtable_A, %t_0                                               │   leave                                          │
    │                                                      │             │         ret                                                                  │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_0, @function                        │
    │                                                      │             │ }                                                                            │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │ data $vtable_A = { }                                                         │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ constructor_A:                                   │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   leaq vtable_A(%rip), %rax                      │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type constructor_A, @function                   │
    │                                                      │             │                                                                              │ .size constructor_A, .-constructor_A             │
    │                                                      │             │                                                                              │ /* end function constructor_A */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .bss                                             │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .fill 0,1,0                                    │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class A { constructor () {} }                        │ g_main_0    │ function ub $g_main_0()                                                      │ .text                                            │
    │                                                      │             │ {                                                                            │ g_main_0:                                        │
    │ fun main() : bool {                                  │             │ @start                                                                       │   pushq %rbp                                     │
    │   let x = new A();                                   │             │         %t_0 =l call $malloc(l 8)                                            │   movq %rsp, %rbp                                │
    │   let y = new A();                                   │             │         %t_1 =l add %t_0, 0                                                  │   pushq %rbx                                     │
    │   x == y                                             │             │         storel 0, %t_1                                                       │   pushq %r12                                     │
    │ }                                                    │             │         call $constructor_A(l %t_0)                                          │   movl $8, %edi                                  │
    │                                                      │             │         %l_x_0 =l copy %t_0                                                  │   callq malloc                                   │
    │                                                      │             │         %t_2 =l call $malloc(l 8)                                            │   movq %rax, %r12                                │
    │                                                      │             │         %t_3 =l add %t_2, 0                                                  │   movq $0, (%r12)                                │
    │                                                      │             │         storel 0, %t_3                                                       │   movq %r12, %rdi                                │
    │                                                      │             │         call $constructor_A(l %t_2)                                          │   callq constructor_A                            │
    │                                                      │             │         %l_y_1 =l copy %t_2                                                  │   movl $8, %edi                                  │
    │                                                      │             │         %t_4 =w ceql %l_x_0, %l_y_1                                          │   callq malloc                                   │
    │                                                      │             │         ret %t_4                                                             │   movq %rax, %rbx                                │
    │                                                      │             │                                                                              │   movq $0, (%rbx)                                │
    │                                                      │             │ }                                                                            │   movq %rbx, %rdi                                │
    │                                                      │             │ function $constructor_A(l %this, )                                           │   callq constructor_A                            │
    │                                                      │             │ {                                                                            │   cmpq %rbx, %r12                                │
    │                                                      │             │ @start                                                                       │   setz %al                                       │
    │                                                      │             │         %t_0 =l add %this, 0                                                 │   movzbl %al, %eax                               │
    │                                                      │             │         storel $vtable_A, %t_0                                               │   popq %r12                                      │
    │                                                      │             │         ret                                                                  │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │ }                                                                            │   ret                                            │
    │                                                      │             │ data $vtable_A = { }                                                         │ .type g_main_0, @function                        │
    │                                                      │             │                                                                              │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │                                                                              │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ constructor_A:                                   │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   leaq vtable_A(%rip), %rax                      │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type constructor_A, @function                   │
    │                                                      │             │                                                                              │ .size constructor_A, .-constructor_A             │
    │                                                      │             │                                                                              │ /* end function constructor_A */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .bss                                             │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .fill 0,1,0                                    │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ class A {                                            │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   x : int;                                           │             │ {                                                                            │ g_main_0:                                        │
    │                                                      │             │ @start                                                                       │   pushq %rbp                                     │
    │   constructor(x : int) {                             │             │         %t_0 =l call $malloc(l 24)                                           │   movq %rsp, %rbp                                │
    │     this.x = x;                                      │             │         %t_1 =l add %t_0, 0                                                  │   subq $8, %rsp                                  │
    │   }                                                  │             │         storel 0, %t_1                                                       │   pushq %rbx                                     │
    │ }                                                    │             │         call $constructor_A(l %t_0, l 20)                                    │   movl $24, %edi                                 │
    │                                                      │             │         %l_a_0 =l copy %t_0                                                  │   callq malloc                                   │
    │ class B < A {                                        │             │         %t_2 =l call $evolver_B(l %l_a_0, l 22)                              │   movq %rax, %rdi                                │
    │   y : int;                                           │             │         jnz %t_2, @b_if_value_3, @b_if_null_4                                │   movq $0, (%rdi)                                │
    │                                                      │             │                                                                              │   movl $20, %esi                                 │
    │   constructor(x : int, y : int) {                    │             │ @b_if_value_3                                                                │   movq %rdi, %rbx                                │
    │     super(x);                                        │             │         %l_b_1 =l copy %t_2                                                  │   callq constructor_A                            │
    │     this.y = y;                                      │             │         %t_6 =l loadl %l_b_1                                                 │   movq %rbx, %rdi                                │
    │   }                                                  │             │         %t_7 =l add %t_6, 0                                                  │   movl $22, %esi                                 │
    │                                                      │             │         %t_8 =l loadl %t_7                                                   │   callq evolver_B                                │
    │   constructor(y : int) evolves A {                   │             │         %t_9 =l call %t_8(l %l_b_1)                                          │   movq %rax, %rdi                                │
    │     this.y = y;                                      │             │         %t_10 =l copy %t_9                                                   │   cmpl $0, %edi                                  │
    │   }                                                  │             │         jmp @b_after_ifq_5                                                   │   jnz .Lbb2                                      │
    │                                                      │             │                                                                              │   movq $-1, %rax                                 │
    │   fun sum() : int {                                  │             │ @b_if_null_4                                                                 │   jmp .Lbb3                                      │
    │     this.x + this.y                                  │             │         %t_11 =l neg 1                                                       │ .Lbb2:                                           │
    │   }                                                  │             │         %t_10 =l copy %t_11                                                  │   movq (%rdi), %rax                              │
    │ }                                                    │             │         jmp @b_after_ifq_5                                                   │   movq (%rax), %rax                              │
    │                                                      │             │                                                                              │   callq *%rax                                    │
    │ fun main() : int {                                   │             │ @b_after_ifq_5                                                               │ .Lbb3:                                           │
    │   let a = new A(20);                                 │             │         ret %t_10                                                            │   popq %rbx                                      │
    │   if? b = a evolves B(22) {                          │             │                                                                              │   leave                                          │
    │     b:sum()                                          │             │ }                                                                            │   ret                                            │
    │   } else {                                           │             │ function $constructor_A(l %this, l %l_x_0, )                                 │ .type g_main_0, @function                        │
    │     -1                                               │             │ {                                                                            │ .size g_main_0, .-g_main_0                       │
    │   }                                                  │             │ @start                                                                       │ /* end function g_main_0 */                      │
    │ }                                                    │             │         %t_0 =l add %this, 8                                                 │                                                  │
    │                                                      │             │         storel %l_x_0, %t_0                                                  │ .text                                            │
    │                                                      │             │         %t_1 =l add %this, 0                                                 │ constructor_A:                                   │
    │                                                      │             │         storel $vtable_A, %t_1                                               │   pushq %rbp                                     │
    │                                                      │             │         ret                                                                  │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq %rsi, 8(%rdi)                             │
    │                                                      │             │ }                                                                            │   leaq vtable_A(%rip), %rax                      │
    │                                                      │             │ data $vtable_A = { }                                                         │   movq %rax, (%rdi)                              │
    │                                                      │             │ function $constructor_B(l %this, l %l_x_0, l %l_y_1, )                       │   leave                                          │
    │                                                      │             │ {                                                                            │   ret                                            │
    │                                                      │             │ @start                                                                       │ .type constructor_A, @function                   │
    │                                                      │             │         %t_0 =w call $constructor_A(l %this, l %l_x_0)                       │ .size constructor_A, .-constructor_A             │
    │                                                      │             │         %t_1 =l add %this, 16                                                │ /* end function constructor_A */                 │
    │                                                      │             │         storel %l_y_1, %t_1                                                  │                                                  │
    │                                                      │             │         %t_2 =l add %this, 0                                                 │ .bss                                             │
    │                                                      │             │         storel $vtable_B, %t_2                                               │ .balign 8                                        │
    │                                                      │             │         ret                                                                  │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .fill 0,1,0                                    │
    │                                                      │             │ }                                                                            │ /* end data */                                   │
    │                                                      │             │ function l $evolver_B(l %this, l %l_y_0, )                                   │                                                  │
    │                                                      │             │ {                                                                            │ .text                                            │
    │                                                      │             │ @start                                                                       │ constructor_B:                                   │
    │                                                      │             │         %t_0 =l add %this, 0                                                 │   pushq %rbp                                     │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_2 =w ceql %t_1, $vtable_A                                         │   pushq %rbx                                     │
    │                                                      │             │         jnz %t_2, @b_is_super_3, @b_not_super_4                              │   pushq %r12                                     │
    │                                                      │             │                                                                              │   movq %rdx, %r12                                │
    │                                                      │             │ @b_is_super_3                                                                │   movq %rdi, %rbx                                │
    │                                                      │             │         %t_5 =l add %this, 16                                                │   callq constructor_A                            │
    │                                                      │             │         storel %l_y_0, %t_5                                                  │   movq %r12, %rdx                                │
    │                                                      │             │         %t_6 =l add %this, 0                                                 │   movq %rbx, %rdi                                │
    │                                                      │             │         storel $vtable_B, %t_6                                               │   movq %rdx, 16(%rdi)                            │
    │                                                      │             │         ret %this                                                            │   leaq vtable_B(%rip), %rax                      │
    │                                                      │             │                                                                              │   movq %rax, (%rdi)                              │
    │                                                      │             │ @b_not_super_4                                                               │   popq %r12                                      │
    │                                                      │             │         ret 0                                                                │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │ }                                                                            │   ret                                            │
    │                                                      │             │ function l $method_B_sum(l %this, )                                          │ .type constructor_B, @function                   │
    │                                                      │             │ {                                                                            │ .size constructor_B, .-constructor_B             │
    │                                                      │             │ @start                                                                       │ /* end function constructor_B */                 │
    │                                                      │             │         %t_0 =l add %this, 8                                                 │                                                  │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │ .text                                            │
    │                                                      │             │         %t_2 =l add %this, 16                                                │ evolver_B:                                       │
    │                                                      │             │         %t_3 =l loadl %t_2                                                   │   pushq %rbp                                     │
    │                                                      │             │         %t_4 =l add %t_1, %t_3                                               │   movq %rsp, %rbp                                │
    │                                                      │             │         ret %t_4                                                             │   movq %rdi, %rax                                │
    │                                                      │             │                                                                              │   movq (%rax), %rdx                              │
    │                                                      │             │ }                                                                            │   leaq vtable_A(%rip), %rcx                      │
    │                                                      │             │ data $vtable_B = { l $method_B_sum, }                                        │   cmpq %rcx, %rdx                                │
    │                                                      │             │                                                                              │   jz .Lbb10                                      │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │                                                                              │   jmp .Lbb11                                     │
    │                                                      │             │                                                                              │ .Lbb10:                                          │
    │                                                      │             │                                                                              │   movq %rsi, 16(%rax)                            │
    │                                                      │             │                                                                              │   leaq vtable_B(%rip), %rcx                      │
    │                                                      │             │                                                                              │   movq %rcx, (%rax)                              │
    │                                                      │             │                                                                              │ .Lbb11:                                          │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type evolver_B, @function                       │
    │                                                      │             │                                                                              │ .size evolver_B, .-evolver_B                     │
    │                                                      │             │                                                                              │ /* end function evolver_B */                     │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ method_B_sum:                                    │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq 8(%rdi), %rax                             │
    │                                                      │             │                                                                              │   movq 16(%rdi), %rcx                            │
    │                                                      │             │                                                                              │   addq %rcx, %rax                                │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type method_B_sum, @function                    │
    │                                                      │             │                                                                              │ .size method_B_sum, .-method_B_sum               │
    │                                                      │             │                                                                              │ /* end function method_B_sum */                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_B:                                        │
    │                                                      │             │                                                                              │   .quad method_B_sum+0                           │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ module Std {                                         │ g_main_1    │ function w $g_Std_print_0(l %l_extern_0, )                                   │ .text                                            │
    │   extern fun print([]char) : unit =                  │             │ {                                                                            │ g_Std_print_0:                                   │
    │ "internal_may_print_string";                         │             │ @start                                                                       │   pushq %rbp                                     │
    │ }                                                    │             │         %t_0 =w call $internal_may_print_string(l %l_extern_0)               │   movq %rsp, %rbp                                │
    │                                                      │             │         ret %t_0                                                             │   callq internal_may_print_string                │
    │ fun main() : int {                                   │             │                                                                              │   leave                                          │
    │   Std.print("Hello world!\n");                       │             │ }                                                                            │   ret                                            │
    │   0                                                  │             │ function l $g_main_1()                                                       │ .type g_Std_print_0, @function                   │
    │ }                                                    │             │ {                                                                            │ .size g_Std_print_0, .-g_Std_print_0             │
    │                                                      │             │ @start                                                                       │ /* end function g_Std_print_0 */                 │
    │                                                      │             │         %t_2 =w call $g_Std_print_0(l $string_t_1)                           │                                                  │
    │                                                      │             │         ret 0                                                                │ .text                                            │
    │                                                      │             │                                                                              │ g_main_1:                                        │
    │                                                      │             │ }                                                                            │   pushq %rbp                                     │
    │                                                      │             │ data $raw_string_t_0 = { b "Hello world!\n", }                               │   movq %rsp, %rbp                                │
    │                                                      │             │ data $string_t_1 = { l $raw_string_t_0, w 0, w 13, }                         │   leaq string_t_1(%rip), %rdi                    │
    │                                                      │             │                                                                              │   callq g_Std_print_0                            │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │                                                                              │ /* end function g_main_1 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ raw_string_t_0:                                  │
    │                                                      │             │                                                                              │   .ascii "Hello world!\n"                        │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ string_t_1:                                      │
    │                                                      │             │                                                                              │   .quad raw_string_t_0+0                         │
    │                                                      │             │                                                                              │   .int 0                                         │
    │                                                      │             │                                                                              │   .int 13                                        │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun main() : int {                                   │ g_main_0    │ function l $g_main_0()                                                       │ .text                                            │
    │   return 10;                                         │             │ {                                                                            │ g_main_0:                                        │
    │   return 20;                                         │             │ @start                                                                       │   pushq %rbp                                     │
    │   0                                                  │             │         ret 10                                                               │   movq %rsp, %rbp                                │
    │ }                                                    │             │                                                                              │   movl $10, %eax                                 │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_0, @function                        │
    │                                                      │             │                                                                              │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │                                                                              │ /* end function g_main_0 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ module Std {                                         │ g_main_1    │ function w $g_Std_Int_println_0(l %l_extern_0, )                             │ .text                                            │
    │   module Int {                                       │             │ {                                                                            │ g_Std_Int_println_0:                             │
    │     extern fun println(int) : unit = "abort";        │             │ @start                                                                       │   pushq %rbp                                     │
    │   }                                                  │             │         %t_0 =w call $abort(l %l_extern_0)                                   │   movq %rsp, %rbp                                │
    │ }                                                    │             │         ret %t_0                                                             │   callq abort                                    │
    │                                                      │             │                                                                              │   leave                                          │
    │ fun main() : unit {                                  │             │ }                                                                            │   ret                                            │
    │   let my_array = [1, 2, 3, 4, 5, 6];                 │             │ function w $g_main_1()                                                       │ .type g_Std_Int_println_0, @function             │
    │   let my_sub_array = my_array[2..4];                 │             │ {                                                                            │ .size g_Std_Int_println_0, .-g_Std_Int_println_0 │
    │   Std.Int.println(my_sub_array[0]);                  │             │ @start                                                                       │ /* end function g_Std_Int_println_0 */           │
    │   Std.Int.println(my_sub_array[1]);                  │             │         %t_0 =l call $malloc(l 16)                                           │                                                  │
    │   Std.Int.println(my_sub_array[2]);                  │             │         %t_1 =l call $malloc(l 48)                                           │ .text                                            │
    │   Std.Int.println(my_sub_array[3]);                  │             │         %t_2 =l add %t_0, 0                                                  │ g_main_1:                                        │
    │ }                                                    │             │         storel %t_1, %t_2                                                    │   pushq %rbp                                     │
    │                                                      │             │         %t_3 =l add %t_0, 8                                                  │   movq %rsp, %rbp                                │
    │                                                      │             │         storew 0, %t_3                                                       │   pushq %rbx                                     │
    │                                                      │             │         %t_4 =l add %t_0, 12                                                 │   pushq %r12                                     │
    │                                                      │             │         storew 6, %t_4                                                       │   movl $16, %edi                                 │
    │                                                      │             │         %t_5 =l add %t_1, 0                                                  │   callq malloc                                   │
    │                                                      │             │         storel 1, %t_5                                                       │   movq %rax, %r12                                │
    │                                                      │             │         %t_6 =l add %t_1, 8                                                  │   movl $48, %edi                                 │
    │                                                      │             │         storel 2, %t_6                                                       │   callq malloc                                   │
    │                                                      │             │         %t_7 =l add %t_1, 16                                                 │   movq %rax, (%r12)                              │
    │                                                      │             │         storel 3, %t_7                                                       │   movl $0, 8(%r12)                               │
    │                                                      │             │         %t_8 =l add %t_1, 24                                                 │   movl $6, 12(%r12)                              │
    │                                                      │             │         storel 4, %t_8                                                       │   movq $1, (%rax)                                │
    │                                                      │             │         %t_9 =l add %t_1, 32                                                 │   movq $2, 8(%rax)                               │
    │                                                      │             │         storel 5, %t_9                                                       │   movq $3, 16(%rax)                              │
    │                                                      │             │         %t_10 =l add %t_1, 40                                                │   movq $4, 24(%rax)                              │
    │                                                      │             │         storel 6, %t_10                                                      │   movq $5, 32(%rax)                              │
    │                                                      │             │         %l_my_array_0 =l copy %t_0                                           │   movq $6, 40(%rax)                              │
    │                                                      │             │         %t_11 =l add %l_my_array_0, 12                                       │   movl 12(%r12), %eax                            │
    │                                                      │             │         %t_12 =l loaduw %t_11                                                │   cmpq $2, %rax                                  │
    │                                                      │             │         %t_13 =w csgel 2, 0                                                  │   setg %cl                                       │
    │                                                      │             │         %t_14 =w csltl 2, %t_12                                              │   movzbl %cl, %ecx                               │
    │                                                      │             │         %t_15 =w mul %t_13, %t_14                                            │   imull $1, %ecx, %ecx                           │
    │                                                      │             │         jnz %t_15, @b_good_index_17, @b_bad_index_16                         │   cmpl $0, %ecx                                  │
    │                                                      │             │                                                                              │   jnz .Lbb4                                      │
    │                                                      │             │ @b_bad_index_16                                                              │   movl $31, %ecx                                 │
    │                                                      │             │         call $panic_index_out_of_bounds(w 9, w 30, w 9, w 31)                │   movl $9, %edx                                  │
    │                                                      │             │         hlt                                                                  │   movl $30, %esi                                 │
    │                                                      │             │                                                                              │   movl $9, %edi                                  │
    │                                                      │             │ @b_good_index_17                                                             │   callq panic_index_out_of_bounds                │
    │                                                      │             │         %t_18 =l add %l_my_array_0, 12                                       │   ud2                                            │
    │                                                      │             │         %t_19 =l loaduw %t_18                                                │ .Lbb4:                                           │
    │                                                      │             │         %t_20 =w csgel 4, 0                                                  │   cmpq $4, %rax                                  │
    │                                                      │             │         %t_21 =w csltl 4, %t_19                                              │   setg %al                                       │
    │                                                      │             │         %t_22 =w mul %t_20, %t_21                                            │   movzbl %al, %eax                               │
    │                                                      │             │         jnz %t_22, @b_good_index_24, @b_bad_index_23                         │   imull $1, %eax, %eax                           │
    │                                                      │             │                                                                              │   cmpl $0, %eax                                  │
    │                                                      │             │ @b_bad_index_23                                                              │   jnz .Lbb6                                      │
    │                                                      │             │         call $panic_index_out_of_bounds(w 9, w 33, w 9, w 34)                │   movl $34, %ecx                                 │
    │                                                      │             │         hlt                                                                  │   movl $9, %edx                                  │
    │                                                      │             │                                                                              │   movl $33, %esi                                 │
    │                                                      │             │ @b_good_index_24                                                             │   movl $9, %edi                                  │
    │                                                      │             │         %t_25 =w cslel 2, 4                                                  │   callq panic_index_out_of_bounds                │
    │                                                      │             │         jnz %t_25, @b_assert_ok_26, @b_assert_fail_27                        │   ud2                                            │
    │                                                      │             │                                                                              │ .Lbb6:                                           │
    │                                                      │             │ @b_assert_ok_26                                                              │   movl $16, %edi                                 │
    │                                                      │             │         %t_28 =l call $malloc(l 16)                                          │   callq malloc                                   │
    │                                                      │             │         %t_29 =l add %l_my_array_0, 0                                        │   movq %rax, %rbx                                │
    │                                                      │             │         %t_30 =l loadl %t_29                                                 │   movq (%r12), %rax                              │
    │                                                      │             │         %t_31 =l add %l_my_array_0, 8                                        │   movl 8(%r12), %ecx                             │
    │                                                      │             │         %t_32 =w loadw %t_31                                                 │   addl $2, %ecx                                  │
    │                                                      │             │         %t_33 =w add 2, %t_32                                                │   movq %rax, (%rbx)                              │
    │                                                      │             │         %t_34 =w sub 4, 2                                                    │   movl %ecx, 8(%rbx)                             │
    │                                                      │             │         %t_35 =w add %t_34, 0                                                │   movl $2, 12(%rbx)                              │
    │                                                      │             │         %t_36 =l add %t_28, 0                                                │   movl %ecx, %ecx                                │
    │                                                      │             │         storel %t_30, %t_36                                                  │   addq $0, %rcx                                  │
    │                                                      │             │         %t_37 =l add %t_28, 8                                                │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │         storew %t_33, %t_37                                                  │   callq g_Std_Int_println_0                      │
    │                                                      │             │         %t_38 =l add %t_28, 12                                               │   movl 12(%rbx), %eax                            │
    │                                                      │             │         storew %t_35, %t_38                                                  │   cmpq $1, %rax                                  │
    │                                                      │             │         %l_my_sub_array_1 =l copy %t_28                                      │   setg %al                                       │
    │                                                      │             │         %t_39 =l add %l_my_sub_array_1, 12                                   │   movzbl %al, %eax                               │
    │                                                      │             │         %t_40 =l loaduw %t_39                                                │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_41 =w csgel 0, 0                                                  │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_42 =w csltl 0, %t_40                                              │   jnz .Lbb9                                      │
    │                                                      │             │         %t_43 =w mul %t_41, %t_42                                            │   movl $32, %ecx                                 │
    │                                                      │             │         jnz %t_43, @b_good_index_45, @b_bad_index_44                         │   movl $11, %edx                                 │
    │                                                      │             │                                                                              │   movl $31, %esi                                 │
    │                                                      │             │ @b_assert_fail_27                                                            │   movl $11, %edi                                 │
    │                                                      │             │         call $panic_subrange_invalid(w 9, w 33, w 9, w 34)                   │   callq panic_index_out_of_bounds                │
    │                                                      │             │         hlt                                                                  │   ud2                                            │
    │                                                      │             │                                                                              │ .Lbb9:                                           │
    │                                                      │             │ @b_bad_index_44                                                              │   movq (%rbx), %rax                              │
    │                                                      │             │         call $panic_index_out_of_bounds(w 10, w 31, w 10, w 32)              │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         hlt                                                                  │   addq $1, %rcx                                  │
    │                                                      │             │                                                                              │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │ @b_good_index_45                                                             │   callq g_Std_Int_println_0                      │
    │                                                      │             │         %t_46 =l loadl %l_my_sub_array_1                                     │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_47 =l add %l_my_sub_array_1, 8                                    │   cmpq $2, %rax                                  │
    │                                                      │             │         %t_48 =l loaduw %t_47                                                │   setg %al                                       │
    │                                                      │             │         %t_49 =l add 0, %t_48                                                │   movzbl %al, %eax                               │
    │                                                      │             │         %t_50 =l mul %t_49, 8                                                │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_51 =l add %t_46, %t_50                                            │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_52 =l loadl %t_51                                                 │   jnz .Lbb11                                     │
    │                                                      │             │         %t_53 =w call $g_Std_Int_println_0(l %t_52)                          │   movl $32, %ecx                                 │
    │                                                      │             │         %t_54 =l add %l_my_sub_array_1, 12                                   │   movl $12, %edx                                 │
    │                                                      │             │         %t_55 =l loaduw %t_54                                                │   movl $31, %esi                                 │
    │                                                      │             │         %t_56 =w csgel 1, 0                                                  │   movl $12, %edi                                 │
    │                                                      │             │         %t_57 =w csltl 1, %t_55                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │         %t_58 =w mul %t_56, %t_57                                            │   ud2                                            │
    │                                                      │             │         jnz %t_58, @b_good_index_60, @b_bad_index_59                         │ .Lbb11:                                          │
    │                                                      │             │                                                                              │   movq (%rbx), %rax                              │
    │                                                      │             │ @b_bad_index_59                                                              │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         call $panic_index_out_of_bounds(w 11, w 31, w 11, w 32)              │   addq $2, %rcx                                  │
    │                                                      │             │         hlt                                                                  │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │                                                                              │   callq g_Std_Int_println_0                      │
    │                                                      │             │ @b_good_index_60                                                             │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_61 =l loadl %l_my_sub_array_1                                     │   cmpq $3, %rax                                  │
    │                                                      │             │         %t_62 =l add %l_my_sub_array_1, 8                                    │   setg %al                                       │
    │                                                      │             │         %t_63 =l loaduw %t_62                                                │   movzbl %al, %eax                               │
    │                                                      │             │         %t_64 =l add 1, %t_63                                                │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_65 =l mul %t_64, 8                                                │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_66 =l add %t_61, %t_65                                            │   jnz .Lbb13                                     │
    │                                                      │             │         %t_67 =l loadl %t_66                                                 │   movl $32, %ecx                                 │
    │                                                      │             │         %t_68 =w call $g_Std_Int_println_0(l %t_67)                          │   movl $13, %edx                                 │
    │                                                      │             │         %t_69 =l add %l_my_sub_array_1, 12                                   │   movl $31, %esi                                 │
    │                                                      │             │         %t_70 =l loaduw %t_69                                                │   movl $13, %edi                                 │
    │                                                      │             │         %t_71 =w csgel 2, 0                                                  │   callq panic_index_out_of_bounds                │
    │                                                      │             │         %t_72 =w csltl 2, %t_70                                              │   ud2                                            │
    │                                                      │             │         %t_73 =w mul %t_71, %t_72                                            │ .Lbb13:                                          │
    │                                                      │             │         jnz %t_73, @b_good_index_75, @b_bad_index_74                         │   movq (%rbx), %rax                              │
    │                                                      │             │                                                                              │   movl 8(%rbx), %ecx                             │
    │                                                      │             │ @b_bad_index_74                                                              │   addq $3, %rcx                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 12, w 31, w 12, w 32)              │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │         hlt                                                                  │   callq g_Std_Int_println_0                      │
    │                                                      │             │                                                                              │   popq %r12                                      │
    │                                                      │             │ @b_good_index_75                                                             │   popq %rbx                                      │
    │                                                      │             │         %t_76 =l loadl %l_my_sub_array_1                                     │   leave                                          │
    │                                                      │             │         %t_77 =l add %l_my_sub_array_1, 8                                    │   ret                                            │
    │                                                      │             │         %t_78 =l loaduw %t_77                                                │ .type g_main_1, @function                        │
    │                                                      │             │         %t_79 =l add 2, %t_78                                                │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │         %t_80 =l mul %t_79, 8                                                │ /* end function g_main_1 */                      │
    │                                                      │             │         %t_81 =l add %t_76, %t_80                                            │                                                  │
    │                                                      │             │         %t_82 =l loadl %t_81                                                 │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │         %t_83 =w call $g_Std_Int_println_0(l %t_82)                          │                                                  │
    │                                                      │             │         %t_84 =l add %l_my_sub_array_1, 12                                   │                                                  │
    │                                                      │             │         %t_85 =l loaduw %t_84                                                │                                                  │
    │                                                      │             │         %t_86 =w csgel 3, 0                                                  │                                                  │
    │                                                      │             │         %t_87 =w csltl 3, %t_85                                              │                                                  │
    │                                                      │             │         %t_88 =w mul %t_86, %t_87                                            │                                                  │
    │                                                      │             │         jnz %t_88, @b_good_index_90, @b_bad_index_89                         │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_bad_index_89                                                              │                                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 13, w 31, w 13, w 32)              │                                                  │
    │                                                      │             │         hlt                                                                  │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_good_index_90                                                             │                                                  │
    │                                                      │             │         %t_91 =l loadl %l_my_sub_array_1                                     │                                                  │
    │                                                      │             │         %t_92 =l add %l_my_sub_array_1, 8                                    │                                                  │
    │                                                      │             │         %t_93 =l loaduw %t_92                                                │                                                  │
    │                                                      │             │         %t_94 =l add 3, %t_93                                                │                                                  │
    │                                                      │             │         %t_95 =l mul %t_94, 8                                                │                                                  │
    │                                                      │             │         %t_96 =l add %t_91, %t_95                                            │                                                  │
    │                                                      │             │         %t_97 =l loadl %t_96                                                 │                                                  │
    │                                                      │             │         %t_98 =w call $g_Std_Int_println_0(l %t_97)                          │                                                  │
    │                                                      │             │         ret %t_98                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun intArray(): []mut int {                          │             │ function l $g_intArray_0()                                                   │ .text                                            │
    │     new [3 + 3]int(5);                               │             │ {                                                                            │ g_intArray_0:                                    │
    │ }                                                    │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =l add 3, 3                                                     │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_1 =l copy %t_0                                                    │   subq $8, %rsp                                  │
    │                                                      │             │         %t_2 =l call $malloc(l 16)                                           │   pushq %rbx                                     │
    │                                                      │             │         %t_3 =l mul %t_0, 8                                                  │   movl $16, %edi                                 │
    │                                                      │             │         %t_4 =l call $malloc(l %t_3)                                         │   callq malloc                                   │
    │                                                      │             │         %t_5 =l add %t_2, 0                                                  │   movq %rax, %rbx                                │
    │                                                      │             │         storel %t_4, %t_5                                                    │   movl $48, %edi                                 │
    │                                                      │             │         %t_6 =l add %t_2, 8                                                  │   callq malloc                                   │
    │                                                      │             │         storew 0, %t_6                                                       │   movq %rax, %rcx                                │
    │                                                      │             │         %t_7 =l add %t_2, 12                                                 │   movq %rbx, %rax                                │
    │                                                      │             │         storew %t_0, %t_7                                                    │   movq %rcx, (%rax)                              │
    │                                                      │             │         jmp @b_array_init_loop_begin_8                                       │   movl $0, 8(%rax)                               │
    │                                                      │             │                                                                              │   movl $6, 12(%rax)                              │
    │                                                      │             │ @b_array_init_loop_begin_8                                                   │   movq %rax, %rbx                                │
    │                                                      │             │         %t_10 =l add %t_4, 0                                                 │   movq %rcx, %rax                                │
    │                                                      │             │         storel 5, %t_10                                                      │   movl $6, %ecx                                  │
    │                                                      │             │         %t_4 =l add %t_4, 8                                                  │ .Lbb2:                                           │
    │                                                      │             │         %t_1 =l sub %t_1, 1                                                  │   movq $5, (%rax)                                │
    │                                                      │             │         jnz %t_1, @b_array_init_loop_begin_8, @b_array_init_loop_exit_9      │   addq $8, %rax                                  │
    │                                                      │             │                                                                              │   subq $1, %rcx                                  │
    │                                                      │             │ @b_array_init_loop_exit_9                                                    │   jnz .Lbb2                                      │
    │                                                      │             │         ret %t_2                                                             │   movq %rbx, %rax                                │
    │                                                      │             │                                                                              │   popq %rbx                                      │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_intArray_0, @function                    │
    │                                                      │             │                                                                              │ .size g_intArray_0, .-g_intArray_0               │
    │                                                      │             │                                                                              │ /* end function g_intArray_0 */                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun charArray(i : int): []mut char {                 │             │ function l $g_charArray_0(l %l_i_0, )                                        │ .text                                            │
    │     new [i]char('a');                                │             │ {                                                                            │ g_charArray_0:                                   │
    │ }                                                    │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =l copy %l_i_0                                                  │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_1 =l call $malloc(l 16)                                           │   pushq %rbx                                     │
    │                                                      │             │         %t_2 =l mul %l_i_0, 1                                                │   pushq %r12                                     │
    │                                                      │             │         %t_3 =l call $malloc(l %t_2)                                         │   movq %rdi, %rbx                                │
    │                                                      │             │         %t_4 =l add %t_1, 0                                                  │   movl $16, %edi                                 │
    │                                                      │             │         storel %t_3, %t_4                                                    │   callq malloc                                   │
    │                                                      │             │         %t_5 =l add %t_1, 8                                                  │   movq %rbx, %rdi                                │
    │                                                      │             │         storew 0, %t_5                                                       │   movq %rax, %rbx                                │
    │                                                      │             │         %t_6 =l add %t_1, 12                                                 │   movq %rdi, %r12                                │
    │                                                      │             │         storew %l_i_0, %t_6                                                  │   callq malloc                                   │
    │                                                      │             │         jmp @b_array_init_loop_begin_7                                       │   movq %r12, %rdi                                │
    │                                                      │             │                                                                              │   movq %rax, %rcx                                │
    │                                                      │             │ @b_array_init_loop_begin_7                                                   │   movq %rbx, %rax                                │
    │                                                      │             │         %t_9 =l add %t_3, 0                                                  │   movq %rcx, (%rax)                              │
    │                                                      │             │         storeb 97, %t_9                                                      │   movl $0, 8(%rax)                               │
    │                                                      │             │         %t_3 =l add %t_3, 1                                                  │   movl %edi, 12(%rax)                            │
    │                                                      │             │         %t_0 =l sub %t_0, 1                                                  │   movq %rax, %rbx                                │
    │                                                      │             │         jnz %t_0, @b_array_init_loop_begin_7, @b_array_init_loop_exit_8      │   movq %rcx, %rax                                │
    │                                                      │             │                                                                              │ .Lbb2:                                           │
    │                                                      │             │ @b_array_init_loop_exit_8                                                    │   movb $97, (%rax)                               │
    │                                                      │             │         ret %t_1                                                             │   addq $1, %rax                                  │
    │                                                      │             │                                                                              │   subq $1, %rdi                                  │
    │                                                      │             │ }                                                                            │   jnz .Lbb2                                      │
    │                                                      │             │                                                                              │   movq %rbx, %rax                                │
    │                                                      │             │                                                                              │   popq %r12                                      │
    │                                                      │             │                                                                              │   popq %rbx                                      │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_charArray_0, @function                   │
    │                                                      │             │                                                                              │ .size g_charArray_0, .-g_charArray_0             │
    │                                                      │             │                                                                              │ /* end function g_charArray_0 */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    └──────────────────────────────────────────────────────┴─────────────┴──────────────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────┘
    |}];
  return ()
;;

let%expect_test "qbe compiler generated code on sample programs" =
  let%bind () =
    ownership_programs
    |> Deferred.List.map ~how:`Sequential ~f:(test ~mode:May.Mode.With_ownership)
    |> Deferred.map ~f:(Expectable.print ~separate_rows:true)
  in
  [%expect {|
    ┌──────────────────────────────────────────────────────────────────────┬─────────────┬────────────────────────────────────────────────────────────────┬───────────────────────────────────────┐
    │ program                                                              │ entry_point │ compiled_program_bytes                                         │ asm                                   │
    ├──────────────────────────────────────────────────────────────────────┼─────────────┼────────────────────────────────────────────────────────────────┼───────────────────────────────────────┤
    │   class A { public id: int; constructor(id: int) { this.id = id; } } │ g_main_0    │ function w $g_main_0()                                         │ .text                                 │
    │   fun main(): unit {                                                 │             │ {                                                              │ g_main_0:                             │
    │     let a_array: []mut !A = [ new A(1) ];                            │             │ @start                                                         │   pushq %rbp                          │
    │     let a: !A = new A(2);                                            │             │         %t_0 =l call $malloc(l 16)                             │   movq %rsp, %rbp                     │
    │     a >=< a_array[0];                                                │             │         %t_1 =l call $malloc(l 8)                              │   subq $8, %rsp                       │
    │     ()                                                               │             │         %t_2 =l add %t_0, 0                                    │   pushq %rbx                          │
    │   }                                                                  │             │         storel %t_1, %t_2                                      │   pushq %r12                          │
    │                                                                      │             │         %t_3 =l add %t_0, 8                                    │   pushq %r13                          │
    │                                                                      │             │         storew 0, %t_3                                         │   movl $16, %edi                      │
    │                                                                      │             │         %t_4 =l add %t_0, 12                                   │   callq malloc                        │
    │                                                                      │             │         storew 1, %t_4                                         │   movq %rax, %r12                     │
    │                                                                      │             │         %t_5 =l call $malloc(l 16)                             │   movl $8, %edi                       │
    │                                                                      │             │         %t_6 =l add %t_5, 0                                    │   callq malloc                        │
    │                                                                      │             │         storel 0, %t_6                                         │   movq %rax, %r13                     │
    │                                                                      │             │         call $constructor_A(l %t_5, l 1)                       │   movq %r13, (%r12)                   │
    │                                                                      │             │         %t_7 =l add %t_1, 0                                    │   movl $0, 8(%r12)                    │
    │                                                                      │             │         storel %t_5, %t_7                                      │   movl $1, 12(%r12)                   │
    │                                                                      │             │         %l_a_array_0 =l copy %t_0                              │   movl $16, %edi                      │
    │                                                                      │             │         %t_8 =l call $malloc(l 16)                             │   callq malloc                        │
    │                                                                      │             │         %t_9 =l add %t_8, 0                                    │   movq %rax, %rbx                     │
    │                                                                      │             │         storel 0, %t_9                                         │   movq $0, (%rbx)                     │
    │                                                                      │             │         call $constructor_A(l %t_8, l 2)                       │   movl $1, %esi                       │
    │                                                                      │             │         %l_a_1 =l copy %t_8                                    │   movq %rbx, %rdi                     │
    │                                                                      │             │         %t_10 =l add %l_a_array_0, 12                          │   callq constructor_A                 │
    │                                                                      │             │         %t_11 =l loaduw %t_10                                  │   movq %rbx, (%r13)                   │
    │                                                                      │             │         %t_12 =w csgel 0, 0                                    │   movl $16, %edi                      │
    │                                                                      │             │         %t_13 =w csltl 0, %t_11                                │   callq malloc                        │
    │                                                                      │             │         %t_14 =w mul %t_12, %t_13                              │   movq %rax, %rbx                     │
    │                                                                      │             │         jnz %t_14, @b_good_index_16, @b_bad_index_15           │   movq $0, (%rbx)                     │
    │                                                                      │             │                                                                │   movl $2, %esi                       │
    │                                                                      │             │ @b_bad_index_15                                                │   movq %rbx, %rdi                     │
    │                                                                      │             │         call $panic_index_out_of_bounds(w 5, w 18, w 5, w 19)  │   callq constructor_A                 │
    │                                                                      │             │         hlt                                                    │   movl 12(%r12), %eax                 │
    │                                                                      │             │                                                                │   cmpq $0, %rax                       │
    │                                                                      │             │ @b_good_index_16                                               │   setg %al                            │
    │                                                                      │             │         %t_17 =l loadl %l_a_array_0                            │   movzbl %al, %eax                    │
    │                                                                      │             │         %t_18 =l add %l_a_array_0, 8                           │   imull $1, %eax, %eax                │
    │                                                                      │             │         %t_19 =l loaduw %t_18                                  │   cmpl $0, %eax                       │
    │                                                                      │             │         %t_20 =l add 0, %t_19                                  │   jnz .Lbb2                           │
    │                                                                      │             │         %t_21 =l mul %t_20, 8                                  │   movl $19, %ecx                      │
    │                                                                      │             │         %t_22 =l add %t_17, %t_21                              │   movl $5, %edx                       │
    │                                                                      │             │         %t_23 =l copy %l_a_1                                   │   movl $18, %esi                      │
    │                                                                      │             │         %t_24 =l add %t_22, 0                                  │   movl $5, %edi                       │
    │                                                                      │             │         %t_25 =l loadl %t_24                                   │   callq panic_index_out_of_bounds     │
    │                                                                      │             │         %t_26 =l add %t_22, 0                                  │   ud2                                 │
    │                                                                      │             │         storel %t_23, %t_26                                    │ .Lbb2:                                │
    │                                                                      │             │         %l_a_1 =l copy %t_25                                   │   movq (%r12), %rax                   │
    │                                                                      │             │         ret 0                                                  │   movl 8(%r12), %ecx                  │
    │                                                                      │             │                                                                │   addq $0, %rcx                       │
    │                                                                      │             │ }                                                              │   movq %rbx, (%rax, %rcx, 8)          │
    │                                                                      │             │ function $constructor_A(l %this, l %l_id_0, )                  │   movl $0, %eax                       │
    │                                                                      │             │ {                                                              │   popq %r13                           │
    │                                                                      │             │ @start                                                         │   popq %r12                           │
    │                                                                      │             │         %t_0 =l add %this, 8                                   │   popq %rbx                           │
    │                                                                      │             │         storel %l_id_0, %t_0                                   │   leave                               │
    │                                                                      │             │         %t_1 =l add %this, 0                                   │   ret                                 │
    │                                                                      │             │         storel $vtable_A, %t_1                                 │ .type g_main_0, @function             │
    │                                                                      │             │         ret                                                    │ .size g_main_0, .-g_main_0            │
    │                                                                      │             │                                                                │ /* end function g_main_0 */           │
    │                                                                      │             │ }                                                              │                                       │
    │                                                                      │             │ data $vtable_A = { }                                           │ .text                                 │
    │                                                                      │             │                                                                │ constructor_A:                        │
    │                                                                      │             │                                                                │   pushq %rbp                          │
    │                                                                      │             │                                                                │   movq %rsp, %rbp                     │
    │                                                                      │             │                                                                │   movq %rsi, 8(%rdi)                  │
    │                                                                      │             │                                                                │   leaq vtable_A(%rip), %rax           │
    │                                                                      │             │                                                                │   movq %rax, (%rdi)                   │
    │                                                                      │             │                                                                │   leave                               │
    │                                                                      │             │                                                                │   ret                                 │
    │                                                                      │             │                                                                │ .type constructor_A, @function        │
    │                                                                      │             │                                                                │ .size constructor_A, .-constructor_A  │
    │                                                                      │             │                                                                │ /* end function constructor_A */      │
    │                                                                      │             │                                                                │                                       │
    │                                                                      │             │                                                                │ .bss                                  │
    │                                                                      │             │                                                                │ .balign 8                             │
    │                                                                      │             │                                                                │ vtable_A:                             │
    │                                                                      │             │                                                                │   .fill 0,1,0                         │
    │                                                                      │             │                                                                │ /* end data */                        │
    │                                                                      │             │                                                                │                                       │
    │                                                                      │             │                                                                │ .section .note.GNU-stack,"",@progbits │
    │                                                                      │             │                                                                │                                       │
    └──────────────────────────────────────────────────────────────────────┴─────────────┴────────────────────────────────────────────────────────────────┴───────────────────────────────────────┘
    |}];
  return ()
;;
