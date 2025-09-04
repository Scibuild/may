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
  ; {|
    const x : int = 10;

    fun main(): int { x }
  |}
  ; {|
    const dialog_options : [][]char = 
    [ "Wait"
    , "Are our bodies really piles of dirt?"
    , "And is the soul just a metaphor?"
    , "I keep my head from looking too far up,"
    , "I fear that there is a heaven above."
      ];

    fun main(i: int): []char { dialog_options[i] }
  |}
  ; {| fun leq(x: int, y: int) : bool { x <= y }|}
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
    |> Result.bind ~f:(fun ast ->
      May.Check.check_decls
        check
        ~decls:ast
        ~load_file:(Utils.load_file ~mappings:[])
        ~starting_file:Utils.starting_file)
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
    │                                                      │             │         %t_15 =l add %l_a_0, 0                                               │ .Lbb2:                                           │
    │                                                      │             │         %t_16 =l loadl %t_15                                                 │   movq (%rbx), %rax                              │
    │                                                      │             │         %t_17 =l add %l_a_0, 8                                               │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         %t_18 =l loaduw %t_17                                                │   addq $0, %rcx                                  │
    │                                                      │             │         %t_19 =l add 0, %t_18                                                │   movq $10, (%rax, %rcx, 8)                      │
    │                                                      │             │         %t_20 =l mul %t_19, 8                                                │   movl 12(%rbx), %esi                            │
    │                                                      │             │         %t_21 =l add %t_16, %t_20                                            │   cmpq $0, %rsi                                  │
    │                                                      │             │         %t_22 =l add %t_21, 0                                                │   setg %al                                       │
    │                                                      │             │         storel 10, %t_22                                                     │   movzbl %al, %eax                               │
    │                                                      │             │         %t_23 =l add %l_a_0, 12                                              │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_24 =l loaduw %t_23                                                │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_25 =w csgel 0, 0                                                  │   jnz .Lbb4                                      │
    │                                                      │             │         %t_26 =w csltl 0, %t_24                                              │   movl $5, %ecx                                  │
    │                                                      │             │         %t_27 =w mul %t_25, %t_26                                            │   movl $4, %edx                                  │
    │                                                      │             │         jnz %t_27, @b_good_index_29, @b_bad_index_28                         │   movl $4, %esi                                  │
    │                                                      │             │                                                                              │   movl $4, %edi                                  │
    │                                                      │             │ @b_bad_index_28                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │         call $panic_index_out_of_bounds(w 4, w 4, w 4, w 5)                  │   ud2                                            │
    │                                                      │             │         hlt                                                                  │ .Lbb4:                                           │
    │                                                      │             │                                                                              │   movq (%rbx), %rcx                              │
    │                                                      │             │ @b_good_index_29                                                             │   movl 8(%rbx), %edx                             │
    │                                                      │             │         %t_30 =l add %l_a_0, 0                                               │   movl $0, %eax                                  │
    │                                                      │             │         %t_31 =l loadl %t_30                                                 │   addq %rdx, %rax                                │
    │                                                      │             │         %t_32 =l add %l_a_0, 8                                               │   movq (%rcx, %rax, 8), %rax                     │
    │                                                      │             │         %t_33 =l loaduw %t_32                                                │   cmpq $2, %rsi                                  │
    │                                                      │             │         %t_34 =l add 0, %t_33                                                │   setg %sil                                      │
    │                                                      │             │         %t_35 =l mul %t_34, 8                                                │   movzbl %sil, %esi                              │
    │                                                      │             │         %t_36 =l add %t_31, %t_35                                            │   imull $1, %esi, %esi                           │
    │                                                      │             │         %t_37 =l loadl %t_36                                                 │   cmpl $0, %esi                                  │
    │                                                      │             │         %t_38 =l add %l_a_0, 12                                              │   jnz .Lbb6                                      │
    │                                                      │             │         %t_39 =l loaduw %t_38                                                │   movl $12, %ecx                                 │
    │                                                      │             │         %t_40 =w csgel 2, 0                                                  │   movl $4, %edx                                  │
    │                                                      │             │         %t_41 =w csltl 2, %t_39                                              │   movl $11, %esi                                 │
    │                                                      │             │         %t_42 =w mul %t_40, %t_41                                            │   movl $4, %edi                                  │
    │                                                      │             │         jnz %t_42, @b_good_index_44, @b_bad_index_43                         │   callq panic_index_out_of_bounds                │
    │                                                      │             │                                                                              │   ud2                                            │
    │                                                      │             │ @b_bad_index_43                                                              │ .Lbb6:                                           │
    │                                                      │             │         call $panic_index_out_of_bounds(w 4, w 11, w 4, w 12)                │   addq $2, %rdx                                  │
    │                                                      │             │         hlt                                                                  │   movq (%rcx, %rdx, 8), %rcx                     │
    │                                                      │             │                                                                              │   addq %rcx, %rax                                │
    │                                                      │             │ @b_good_index_44                                                             │   popq %rbx                                      │
    │                                                      │             │         %t_45 =l add %l_a_0, 0                                               │   leave                                          │
    │                                                      │             │         %t_46 =l loadl %t_45                                                 │   ret                                            │
    │                                                      │             │         %t_47 =l add %l_a_0, 8                                               │ .type g_main_0, @function                        │
    │                                                      │             │         %t_48 =l loaduw %t_47                                                │ .size g_main_0, .-g_main_0                       │
    │                                                      │             │         %t_49 =l add 2, %t_48                                                │ /* end function g_main_0 */                      │
    │                                                      │             │         %t_50 =l mul %t_49, 8                                                │                                                  │
    │                                                      │             │         %t_51 =l add %t_46, %t_50                                            │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │         %t_52 =l loadl %t_51                                                 │                                                  │
    │                                                      │             │         %t_53 =l add %t_37, %t_52                                            │                                                  │
    │                                                      │             │         ret %t_53                                                            │                                                  │
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
    │   }                                                  │             │         %t_3 =l add %t_2, 24                                                 │   movq $0, (%rdi)                                │
    │                                                      │             │         %t_4 =l loadl %t_3                                                   │   movl $24, %esi                                 │
    │   fun add(other: Int) : Int {                        │             │         %t_5 =l call %t_4(l %l_x_0)                                          │   movq %rdi, %rbx                                │
    │     let a = this:get();                              │             │         %t_6 =l call $malloc(l 24)                                           │   callq constructor_Counter                      │
    │     let b = other:get();                             │             │         %t_7 =l add %t_6, 0                                                  │   movq %rbx, %rdi                                │
    │                                                      │             │         storel 0, %t_7                                                       │   movq (%rdi), %rax                              │
    │     new Int(a + b)                                   │             │         call $constructor_Counter(l %t_6, l 16)                              │   movq 24(%rax), %rax                            │
    │   }                                                  │             │         %l_y_1 =l copy %t_6                                                  │   movq %rdi, %rbx                                │
    │ }                                                    │             │         %t_8 =l loadl %l_y_1                                                 │   callq *%rax                                    │
    │                                                      │             │         %t_9 =l add %t_8, 24                                                 │   movq %rbx, %rdi                                │
    │ class Counter < Int {                                │             │         %t_10 =l loadl %t_9                                                  │   movq %rdi, %rbx                                │
    │   mut count : int;                                   │             │         %t_11 =l call %t_10(l %l_y_1)                                        │   movl $24, %edi                                 │
    │                                                      │             │         %t_12 =l loadl %l_x_0                                                │   callq malloc                                   │
    │   constructor(x : int) {                             │             │         %t_13 =l add %t_12, 8                                                │   movq %rbx, %rdi                                │
    │     super(x);                                        │             │         %t_14 =l loadl %t_13                                                 │   movq %rax, %rsi                                │
    │     this.count = 0;                                  │             │         %t_15 =l call %t_14(l %l_x_0, l %l_y_1)                              │   movq $0, (%rsi)                                │
    │   }                                                  │             │         %l_z_2 =l copy %t_15                                                 │   movq %rsi, %r12                                │
    │                                                      │             │         %t_16 =l loadl %l_z_2                                                │   movl $16, %esi                                 │
    │   overrides fun get(): int {                         │             │         %t_17 =l add %t_16, 16                                               │   movq %rdi, %rbx                                │
    │     this.x + this.count                              │             │         %t_18 =l loadl %t_17                                                 │   movq %r12, %rdi                                │
    │   }                                                  │             │         %t_19 =l call %t_18(l %l_z_2)                                        │   callq constructor_Counter                      │
    │                                                      │             │         ret %t_19                                                            │   movq %rbx, %rdi                                │
    │   fun inc(): unit {                                  │             │                                                                              │   movq (%r12), %rax                              │
    │     this.count = this.count + 1;                     │             │ }                                                                            │   movq 24(%rax), %rax                            │
    │   }                                                  │             │ function $constructor_Int(l %this, l %l_x_0, )                               │   movq %rdi, %rbx                                │
    │ }                                                    │             │ {                                                                            │   movq %r12, %rdi                                │
    │                                                      │             │ @start                                                                       │   callq *%rax                                    │
    │ fun main() : int {                                   │             │         %t_0 =l add %this, 8                                                 │   movq %r12, %rsi                                │
    │   let x = new Counter(24);                           │             │         storel %l_x_0, %t_0                                                  │   movq %rbx, %rdi                                │
    │   x:inc();                                           │             │         %t_1 =l add %this, 0                                                 │   movq (%rdi), %rax                              │
    │   let y = new Counter(16);                           │             │         storel $vtable_Int, %t_1                                             │   movq 8(%rax), %rax                             │
    │   y:inc();                                           │             │         ret                                                                  │   callq *%rax                                    │
    │   let z = x:add(y);                                  │             │                                                                              │   movq %rax, %rdi                                │
    │   z:get()                                            │             │ }                                                                            │   movq (%rdi), %rax                              │
    │ }                                                    │             │ function l $method_Int_get(l %this, )                                        │   movq 16(%rax), %rax                            │
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
    │                                                      │             │         %t_1 =l add %t_0, 16                                                 │   pushq %rbp                                     │
    │                                                      │             │         %t_2 =l loadl %t_1                                                   │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_3 =l call %t_2(l %this)                                           │   movq %rsi, 8(%rdi)                             │
    │                                                      │             │         %l_a_1 =l copy %t_3                                                  │   leaq vtable_Int(%rip), %rax                    │
    │                                                      │             │         %t_4 =l loadl %l_other_0                                             │   movq %rax, (%rdi)                              │
    │                                                      │             │         %t_5 =l add %t_4, 16                                                 │   leave                                          │
    │                                                      │             │         %t_6 =l loadl %t_5                                                   │   ret                                            │
    │                                                      │             │         %t_7 =l call %t_6(l %l_other_0)                                      │ .type constructor_Int, @function                 │
    │                                                      │             │         %l_b_2 =l copy %t_7                                                  │ .size constructor_Int, .-constructor_Int         │
    │                                                      │             │         %t_8 =l add %l_a_1, %l_b_2                                           │ /* end function constructor_Int */               │
    │                                                      │             │         %t_9 =l call $malloc(l 16)                                           │                                                  │
    │                                                      │             │         %t_10 =l add %t_9, 0                                                 │ .text                                            │
    │                                                      │             │         storel 0, %t_10                                                      │ method_Int_get:                                  │
    │                                                      │             │         call $constructor_Int(l %t_9, l %t_8)                                │   pushq %rbp                                     │
    │                                                      │             │         ret %t_9                                                             │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movq 8(%rdi), %rax                             │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │ data $vtable_Int = { l 0, l $method_Int_add, l $method_Int_get, }            │   ret                                            │
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
    │                                                      │             │ {                                                                            │   movq 16(%rax), %rax                            │
    │                                                      │             │ @start                                                                       │   callq *%rax                                    │
    │                                                      │             │         %t_0 =l add %this, 8                                                 │   movq %rbx, %rdi                                │
    │                                                      │             │         %t_1 =l loadl %t_0                                                   │   movq %rax, %rbx                                │
    │                                                      │             │         %t_2 =l add %this, 16                                                │   movq (%rdi), %rax                              │
    │                                                      │             │         %t_3 =l loadl %t_2                                                   │   movq 16(%rax), %rax                            │
    │                                                      │             │         %t_4 =l add %t_1, %t_3                                               │   callq *%rax                                    │
    │                                                      │             │         ret %t_4                                                             │   addq %rax, %rbx                                │
    │                                                      │             │                                                                              │   movl $16, %edi                                 │
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
    │                                                      │             │ data $vtable_Counter = { l 0, l $method_Int_add, l $method_Counter_get, l    │ /* end function method_Int_add */                │
    │                                                      │             │ $method_Counter_inc, }                                                       │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_Int:                                      │
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │ fun main() : int {                                   │             │         %t_3 =l add %t_2, 8                                                  │   movq %rax, %rdi                                │
    │   (new B()):m()                                      │             │         %t_4 =l loadl %t_3                                                   │   movq $0, (%rdi)                                │
    │ }                                                    │             │         %t_5 =l call %t_4(l %t_0)                                            │   movq %rdi, %rbx                                │
    │                                                      │             │         ret %t_5                                                             │   callq constructor_B                            │
    │                                                      │             │                                                                              │   movq %rbx, %rdi                                │
    │                                                      │             │ }                                                                            │   movq (%rdi), %rax                              │
    │                                                      │             │ function $constructor_A(l %this, )                                           │   movq 8(%rax), %rax                             │
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
    │                                                      │             │ data $vtable_A = { l 0, l $method_A_m, }                                     │   movq %rax, (%rdi)                              │
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
    │                                                      │             │ data $vtable_B = { l 0, l $method_B_m, }                                     │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │         %t_3 =l add %t_2, 16                                                 │   movq %rax, %rdi                                │
    │   fun setNext(next : ?LinkedList) : unit {           │             │         %t_4 =l loadl %t_3                                                   │   movq $0, (%rdi)                                │
    │     this.next = next;                                │             │         %t_5 =l call %t_4(l %t_0, l 2)                                       │   movl $4, %esi                                  │
    │   }                                                  │             │         %t_6 =l loadl %t_5                                                   │   movq %rdi, %rbx                                │
    │                                                      │             │         %t_7 =l add %t_6, 16                                                 │   callq constructor_LinkedList                   │
    │   fun snoc(v : int) : LinkedList {                   │             │         %t_8 =l loadl %t_7                                                   │   movq %rbx, %rdi                                │
    │     let new_head = new LinkedList(v);                │             │         %t_9 =l call %t_8(l %t_5, l 7)                                       │   movq (%rdi), %rax                              │
    │     new_head:setNext(this);                          │             │         %t_10 =l neg 1                                                       │   movq 16(%rax), %rax                            │
    │     new_head                                         │             │         %t_11 =l loadl %t_9                                                  │   movl $2, %esi                                  │
    │   }                                                  │             │         %t_12 =l add %t_11, 16                                               │   callq *%rax                                    │
    │                                                      │             │         %t_13 =l loadl %t_12                                                 │   movq %rax, %rdi                                │
    │   fun sum() : int {                                  │             │         %t_14 =l call %t_13(l %t_9, l %t_10)                                 │   movq (%rdi), %rax                              │
    │     this.v + (if? n = this.next { n:sum() } else {   │             │         %l_list_0 =l copy %t_14                                              │   movq 16(%rax), %rax                            │
    │ 0 })                                                 │             │         %t_15 =l loadl %l_list_0                                             │   movl $7, %esi                                  │
    │   }                                                  │             │         %t_16 =l add %t_15, 24                                               │   callq *%rax                                    │
    │ }                                                    │             │         %t_17 =l loadl %t_16                                                 │   movq %rax, %rdi                                │
    │                                                      │             │         %t_18 =l call %t_17(l %l_list_0)                                     │   movq (%rdi), %rax                              │
    │ fun main() : int {                                   │             │         ret %t_18                                                            │   movq 16(%rax), %rax                            │
    │   let list = new                                     │             │                                                                              │   movq $-1, %rsi                                 │
    │ LinkedList(4):snoc(2):snoc(7):snoc(-1);              │             │ }                                                                            │   callq *%rax                                    │
    │   list:sum()                                         │             │ function $constructor_LinkedList(l %this, l %l_v_0, )                        │   movq %rax, %rdi                                │
    │ }                                                    │             │ {                                                                            │   movq (%rdi), %rax                              │
    │                                                      │             │ @start                                                                       │   movq 24(%rax), %rax                            │
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
    │                                                      │             │         %t_3 =l add %t_2, 8                                                  │   movq %rsp, %rbp                                │
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
    │                                                      │             │         %t_8 =l add %t_7, 24                                                 │   movl $24, %edi                                 │
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
    │                                                      │             │         %t_12 =l add %t_1, %t_11                                             │   movq 8(%rax), %rax                             │
    │                                                      │             │         ret %t_12                                                            │   movq %rbx, %rdi                                │
    │                                                      │             │                                                                              │   callq *%rax                                    │
    │                                                      │             │ }                                                                            │   movq %rbx, %rax                                │
    │                                                      │             │ data $vtable_LinkedList = { l 0, l $method_LinkedList_setNext, l             │   popq %r12                                      │
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
    │                                                      │             │                                                                              │   movq 24(%rax), %rax                            │
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
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │ data $vtable_A = { l 0, }                                                    │ /* end function g_main_0 */                      │
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
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │ data $vtable_A = { l 0, }                                                    │ .type g_main_0, @function                        │
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
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │   }                                                  │             │         %t_7 =l add %t_6, 8                                                  │   movl $22, %esi                                 │
    │                                                      │             │         %t_8 =l loadl %t_7                                                   │   callq evolver_B                                │
    │   constructor(y : int) evolves A {                   │             │         %t_9 =l call %t_8(l %l_b_1)                                          │   movq %rax, %rdi                                │
    │     this.y = y;                                      │             │         %t_10 =l copy %t_9                                                   │   cmpl $0, %edi                                  │
    │   }                                                  │             │         jmp @b_after_ifq_5                                                   │   jnz .Lbb2                                      │
    │                                                      │             │                                                                              │   movq $-1, %rax                                 │
    │   fun sum() : int {                                  │             │ @b_if_null_4                                                                 │   jmp .Lbb3                                      │
    │     this.x + this.y                                  │             │         %t_11 =l neg 1                                                       │ .Lbb2:                                           │
    │   }                                                  │             │         %t_10 =l copy %t_11                                                  │   movq (%rdi), %rax                              │
    │ }                                                    │             │         jmp @b_after_ifq_5                                                   │   movq 8(%rax), %rax                             │
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
    │                                                      │             │ data $vtable_A = { l 0, }                                                    │   movq %rax, (%rdi)                              │
    │                                                      │             │ function $constructor_B(l %this, l %l_x_0, l %l_y_1, )                       │   leave                                          │
    │                                                      │             │ {                                                                            │   ret                                            │
    │                                                      │             │ @start                                                                       │ .type constructor_A, @function                   │
    │                                                      │             │         %t_0 =w call $constructor_A(l %this, l %l_x_0)                       │ .size constructor_A, .-constructor_A             │
    │                                                      │             │         %t_1 =l add %this, 16                                                │ /* end function constructor_A */                 │
    │                                                      │             │         storel %l_y_1, %t_1                                                  │                                                  │
    │                                                      │             │         %t_2 =l add %this, 0                                                 │ .data                                            │
    │                                                      │             │         storel $vtable_B, %t_2                                               │ .balign 8                                        │
    │                                                      │             │         ret                                                                  │ vtable_A:                                        │
    │                                                      │             │                                                                              │   .quad 0                                        │
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
    │                                                      │             │ data $vtable_B = { l 0, l $method_B_sum, }                                   │   cmpq %rcx, %rdx                                │
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
    │                                                      │             │                                                                              │   .quad 0                                        │
    │                                                      │             │                                                                              │   .quad method_B_sum+0                           │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ module Std {                                         │ g_main_1    │ data $.static.1 = { l $.static.2, w 0, w 13, }                               │ .data                                            │
    │   extern fun print([]char) : unit =                  │             │ data $.static.2 = { b "Hello world!\n", }                                    │ .balign 8                                        │
    │ "internal_may_print_string";                         │             │ function w $g_Std_print_0(l %l_extern_0, )                                   │ .static.1:                                       │
    │ }                                                    │             │ {                                                                            │   .quad .static.2+0                              │
    │                                                      │             │ @start                                                                       │   .int 0                                         │
    │ fun main() : int {                                   │             │         %t_0 =w call $internal_may_print_string(l %l_extern_0)               │   .int 13                                        │
    │   Std.print("Hello world!\n");                       │             │         ret %t_0                                                             │ /* end data */                                   │
    │   0                                                  │             │                                                                              │                                                  │
    │ }                                                    │             │ }                                                                            │ .data                                            │
    │                                                      │             │ function l $g_main_1()                                                       │ .balign 8                                        │
    │                                                      │             │ {                                                                            │ .static.2:                                       │
    │                                                      │             │ @start                                                                       │   .ascii "Hello world!\n"                        │
    │                                                      │             │         %t_0 =w call $g_Std_print_0(l $.static.1)                            │ /* end data */                                   │
    │                                                      │             │         ret 0                                                                │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │ }                                                                            │ g_Std_print_0:                                   │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   callq internal_may_print_string                │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_Std_print_0, @function                   │
    │                                                      │             │                                                                              │ .size g_Std_print_0, .-g_Std_print_0             │
    │                                                      │             │                                                                              │ /* end function g_Std_print_0 */                 │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ g_main_1:                                        │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   leaq .static.1(%rip), %rdi                     │
    │                                                      │             │                                                                              │   callq g_Std_print_0                            │
    │                                                      │             │                                                                              │   movl $0, %eax                                  │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │                                                                              │ /* end function g_main_1 */                      │
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
    │                                                      │             │         storew 0, %t_3                                                       │   subq $8, %rsp                                  │
    │                                                      │             │         %t_4 =l add %t_0, 12                                                 │   pushq %rbx                                     │
    │                                                      │             │         storew 6, %t_4                                                       │   pushq %r12                                     │
    │                                                      │             │         %t_5 =l add %t_1, 0                                                  │   pushq %r13                                     │
    │                                                      │             │         storel 1, %t_5                                                       │   movl $16, %edi                                 │
    │                                                      │             │         %t_6 =l add %t_1, 8                                                  │   callq malloc                                   │
    │                                                      │             │         storel 2, %t_6                                                       │   movq %rax, %rbx                                │
    │                                                      │             │         %t_7 =l add %t_1, 16                                                 │   movl $48, %edi                                 │
    │                                                      │             │         storel 3, %t_7                                                       │   callq malloc                                   │
    │                                                      │             │         %t_8 =l add %t_1, 24                                                 │   movq %rax, (%rbx)                              │
    │                                                      │             │         storel 4, %t_8                                                       │   movl $0, 8(%rbx)                               │
    │                                                      │             │         %t_9 =l add %t_1, 32                                                 │   movl $6, 12(%rbx)                              │
    │                                                      │             │         storel 5, %t_9                                                       │   movq $1, (%rax)                                │
    │                                                      │             │         %t_10 =l add %t_1, 40                                                │   movq $2, 8(%rax)                               │
    │                                                      │             │         storel 6, %t_10                                                      │   movq $3, 16(%rax)                              │
    │                                                      │             │         %l_my_array_0 =l copy %t_0                                           │   movq $4, 24(%rax)                              │
    │                                                      │             │         %t_11 =l add %l_my_array_0, 12                                       │   movq $5, 32(%rax)                              │
    │                                                      │             │         %t_12 =l loaduw %t_11                                                │   movq $6, 40(%rax)                              │
    │                                                      │             │         %t_13 =w csgel 2, 0                                                  │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_14 =w csltl 2, %t_12                                              │   cmpq $2, %rax                                  │
    │                                                      │             │         %t_15 =w mul %t_13, %t_14                                            │   setg %cl                                       │
    │                                                      │             │         jnz %t_15, @b_good_index_17, @b_bad_index_16                         │   movzbl %cl, %ecx                               │
    │                                                      │             │                                                                              │   imull $1, %ecx, %ecx                           │
    │                                                      │             │ @b_bad_index_16                                                              │   cmpl $0, %ecx                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 9, w 30, w 9, w 31)                │   jnz .Lbb4                                      │
    │                                                      │             │         hlt                                                                  │   movl $31, %ecx                                 │
    │                                                      │             │                                                                              │   movl $9, %edx                                  │
    │                                                      │             │ @b_good_index_17                                                             │   movl $30, %esi                                 │
    │                                                      │             │         %t_18 =l add %l_my_array_0, 12                                       │   movl $9, %edi                                  │
    │                                                      │             │         %t_19 =l loaduw %t_18                                                │   callq panic_index_out_of_bounds                │
    │                                                      │             │         %t_20 =w csgel 4, 0                                                  │   ud2                                            │
    │                                                      │             │         %t_21 =w csltl 4, %t_19                                              │ .Lbb4:                                           │
    │                                                      │             │         %t_22 =w mul %t_20, %t_21                                            │   cmpq $4, %rax                                  │
    │                                                      │             │         jnz %t_22, @b_good_index_24, @b_bad_index_23                         │   setg %al                                       │
    │                                                      │             │                                                                              │   movzbl %al, %eax                               │
    │                                                      │             │ @b_bad_index_23                                                              │   imull $1, %eax, %eax                           │
    │                                                      │             │         call $panic_index_out_of_bounds(w 9, w 33, w 9, w 34)                │   cmpl $0, %eax                                  │
    │                                                      │             │         hlt                                                                  │   jnz .Lbb6                                      │
    │                                                      │             │                                                                              │   movl $34, %ecx                                 │
    │                                                      │             │ @b_good_index_24                                                             │   movl $9, %edx                                  │
    │                                                      │             │         %t_25 =w cslel 2, 4                                                  │   movl $33, %esi                                 │
    │                                                      │             │         jnz %t_25, @b_assert_ok_26, @b_assert_fail_27                        │   movl $9, %edi                                  │
    │                                                      │             │                                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │ @b_assert_ok_26                                                              │   ud2                                            │
    │                                                      │             │         %t_28 =l add %l_my_array_0, 0                                        │ .Lbb6:                                           │
    │                                                      │             │         %t_29 =l loadl %t_28                                                 │   movq (%rbx), %r12                              │
    │                                                      │             │         %t_30 =l add %l_my_array_0, 8                                        │   movl 8(%rbx), %eax                             │
    │                                                      │             │         %t_31 =w loadw %t_30                                                 │   movl $2, %r13d                                 │
    │                                                      │             │         %t_32 =w add 2, %t_31                                                │   addl %eax, %r13d                               │
    │                                                      │             │         %t_33 =w sub 4, 2                                                    │   movl $16, %edi                                 │
    │                                                      │             │         %t_34 =w add %t_33, 1                                                │   callq malloc                                   │
    │                                                      │             │         %t_35 =l call $malloc(l 16)                                          │   movq %rax, %rbx                                │
    │                                                      │             │         %t_36 =l add %t_35, 0                                                │   movq %r12, (%rbx)                              │
    │                                                      │             │         storel %t_29, %t_36                                                  │   movl %r13d, 8(%rbx)                            │
    │                                                      │             │         %t_37 =l add %t_35, 8                                                │   movl $3, 12(%rbx)                              │
    │                                                      │             │         storew %t_32, %t_37                                                  │   movl %r13d, %eax                               │
    │                                                      │             │         %t_38 =l add %t_35, 12                                               │   addq $0, %rax                                  │
    │                                                      │             │         storew %t_34, %t_38                                                  │   movq (%r12, %rax, 8), %rdi                     │
    │                                                      │             │         %l_my_sub_array_1 =l copy %t_35                                      │   callq g_Std_Int_println_0                      │
    │                                                      │             │         %t_39 =l add %l_my_sub_array_1, 12                                   │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_40 =l loaduw %t_39                                                │   cmpq $1, %rax                                  │
    │                                                      │             │         %t_41 =w csgel 0, 0                                                  │   setg %al                                       │
    │                                                      │             │         %t_42 =w csltl 0, %t_40                                              │   movzbl %al, %eax                               │
    │                                                      │             │         %t_43 =w mul %t_41, %t_42                                            │   imull $1, %eax, %eax                           │
    │                                                      │             │         jnz %t_43, @b_good_index_45, @b_bad_index_44                         │   cmpl $0, %eax                                  │
    │                                                      │             │                                                                              │   jnz .Lbb9                                      │
    │                                                      │             │ @b_assert_fail_27                                                            │   movl $32, %ecx                                 │
    │                                                      │             │         call $panic_subrange_invalid(w 9, w 33, w 9, w 34)                   │   movl $11, %edx                                 │
    │                                                      │             │         hlt                                                                  │   movl $31, %esi                                 │
    │                                                      │             │                                                                              │   movl $11, %edi                                 │
    │                                                      │             │ @b_bad_index_44                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │         call $panic_index_out_of_bounds(w 10, w 31, w 10, w 32)              │   ud2                                            │
    │                                                      │             │         hlt                                                                  │ .Lbb9:                                           │
    │                                                      │             │                                                                              │   movq (%rbx), %rax                              │
    │                                                      │             │ @b_good_index_45                                                             │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         %t_46 =l add %l_my_sub_array_1, 0                                    │   addq $1, %rcx                                  │
    │                                                      │             │         %t_47 =l loadl %t_46                                                 │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │         %t_48 =l add %l_my_sub_array_1, 8                                    │   callq g_Std_Int_println_0                      │
    │                                                      │             │         %t_49 =l loaduw %t_48                                                │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_50 =l add 0, %t_49                                                │   cmpq $2, %rax                                  │
    │                                                      │             │         %t_51 =l mul %t_50, 8                                                │   setg %al                                       │
    │                                                      │             │         %t_52 =l add %t_47, %t_51                                            │   movzbl %al, %eax                               │
    │                                                      │             │         %t_53 =l loadl %t_52                                                 │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_54 =w call $g_Std_Int_println_0(l %t_53)                          │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_55 =l add %l_my_sub_array_1, 12                                   │   jnz .Lbb11                                     │
    │                                                      │             │         %t_56 =l loaduw %t_55                                                │   movl $32, %ecx                                 │
    │                                                      │             │         %t_57 =w csgel 1, 0                                                  │   movl $12, %edx                                 │
    │                                                      │             │         %t_58 =w csltl 1, %t_56                                              │   movl $31, %esi                                 │
    │                                                      │             │         %t_59 =w mul %t_57, %t_58                                            │   movl $12, %edi                                 │
    │                                                      │             │         jnz %t_59, @b_good_index_61, @b_bad_index_60                         │   callq panic_index_out_of_bounds                │
    │                                                      │             │                                                                              │   ud2                                            │
    │                                                      │             │ @b_bad_index_60                                                              │ .Lbb11:                                          │
    │                                                      │             │         call $panic_index_out_of_bounds(w 11, w 31, w 11, w 32)              │   movq (%rbx), %rax                              │
    │                                                      │             │         hlt                                                                  │   movl 8(%rbx), %ecx                             │
    │                                                      │             │                                                                              │   addq $2, %rcx                                  │
    │                                                      │             │ @b_good_index_61                                                             │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │         %t_62 =l add %l_my_sub_array_1, 0                                    │   callq g_Std_Int_println_0                      │
    │                                                      │             │         %t_63 =l loadl %t_62                                                 │   movl 12(%rbx), %eax                            │
    │                                                      │             │         %t_64 =l add %l_my_sub_array_1, 8                                    │   cmpq $3, %rax                                  │
    │                                                      │             │         %t_65 =l loaduw %t_64                                                │   setg %al                                       │
    │                                                      │             │         %t_66 =l add 1, %t_65                                                │   movzbl %al, %eax                               │
    │                                                      │             │         %t_67 =l mul %t_66, 8                                                │   imull $1, %eax, %eax                           │
    │                                                      │             │         %t_68 =l add %t_63, %t_67                                            │   cmpl $0, %eax                                  │
    │                                                      │             │         %t_69 =l loadl %t_68                                                 │   jnz .Lbb13                                     │
    │                                                      │             │         %t_70 =w call $g_Std_Int_println_0(l %t_69)                          │   movl $32, %ecx                                 │
    │                                                      │             │         %t_71 =l add %l_my_sub_array_1, 12                                   │   movl $13, %edx                                 │
    │                                                      │             │         %t_72 =l loaduw %t_71                                                │   movl $31, %esi                                 │
    │                                                      │             │         %t_73 =w csgel 2, 0                                                  │   movl $13, %edi                                 │
    │                                                      │             │         %t_74 =w csltl 2, %t_72                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │         %t_75 =w mul %t_73, %t_74                                            │   ud2                                            │
    │                                                      │             │         jnz %t_75, @b_good_index_77, @b_bad_index_76                         │ .Lbb13:                                          │
    │                                                      │             │                                                                              │   movq (%rbx), %rax                              │
    │                                                      │             │ @b_bad_index_76                                                              │   movl 8(%rbx), %ecx                             │
    │                                                      │             │         call $panic_index_out_of_bounds(w 12, w 31, w 12, w 32)              │   addq $3, %rcx                                  │
    │                                                      │             │         hlt                                                                  │   movq (%rax, %rcx, 8), %rdi                     │
    │                                                      │             │                                                                              │   callq g_Std_Int_println_0                      │
    │                                                      │             │ @b_good_index_77                                                             │   popq %r13                                      │
    │                                                      │             │         %t_78 =l add %l_my_sub_array_1, 0                                    │   popq %r12                                      │
    │                                                      │             │         %t_79 =l loadl %t_78                                                 │   popq %rbx                                      │
    │                                                      │             │         %t_80 =l add %l_my_sub_array_1, 8                                    │   leave                                          │
    │                                                      │             │         %t_81 =l loaduw %t_80                                                │   ret                                            │
    │                                                      │             │         %t_82 =l add 2, %t_81                                                │ .type g_main_1, @function                        │
    │                                                      │             │         %t_83 =l mul %t_82, 8                                                │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │         %t_84 =l add %t_79, %t_83                                            │ /* end function g_main_1 */                      │
    │                                                      │             │         %t_85 =l loadl %t_84                                                 │                                                  │
    │                                                      │             │         %t_86 =w call $g_Std_Int_println_0(l %t_85)                          │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │         %t_87 =l add %l_my_sub_array_1, 12                                   │                                                  │
    │                                                      │             │         %t_88 =l loaduw %t_87                                                │                                                  │
    │                                                      │             │         %t_89 =w csgel 3, 0                                                  │                                                  │
    │                                                      │             │         %t_90 =w csltl 3, %t_88                                              │                                                  │
    │                                                      │             │         %t_91 =w mul %t_89, %t_90                                            │                                                  │
    │                                                      │             │         jnz %t_91, @b_good_index_93, @b_bad_index_92                         │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_bad_index_92                                                              │                                                  │
    │                                                      │             │         call $panic_index_out_of_bounds(w 13, w 31, w 13, w 32)              │                                                  │
    │                                                      │             │         hlt                                                                  │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_good_index_93                                                             │                                                  │
    │                                                      │             │         %t_94 =l add %l_my_sub_array_1, 0                                    │                                                  │
    │                                                      │             │         %t_95 =l loadl %t_94                                                 │                                                  │
    │                                                      │             │         %t_96 =l add %l_my_sub_array_1, 8                                    │                                                  │
    │                                                      │             │         %t_97 =l loaduw %t_96                                                │                                                  │
    │                                                      │             │         %t_98 =l add 3, %t_97                                                │                                                  │
    │                                                      │             │         %t_99 =l mul %t_98, 8                                                │                                                  │
    │                                                      │             │         %t_100 =l add %t_95, %t_99                                           │                                                  │
    │                                                      │             │         %t_101 =l loadl %t_100                                               │                                                  │
    │                                                      │             │         %t_102 =w call $g_Std_Int_println_0(l %t_101)                        │                                                  │
    │                                                      │             │         ret %t_102                                                           │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun intArray(): []mut int {                          │             │ function l $g_intArray_0()                                                   │ .text                                            │
    │     new [3 + 3]int(5);                               │             │ {                                                                            │ g_intArray_0:                                    │
    │ }                                                    │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =l add 3, 3                                                     │   movq %rsp, %rbp                                │
    │                                                      │             │         %t_1 =w csltl %t_0, 0                                                │   subq $8, %rsp                                  │
    │                                                      │             │         jnz %t_1, @b_negative_array_size_2, @b_non_negative_array_size_3     │   pushq %rbx                                     │
    │                                                      │             │                                                                              │   movl $48, %edi                                 │
    │                                                      │             │ @b_negative_array_size_2                                                     │   callq malloc                                   │
    │                                                      │             │         call $panic_cannot_allocate_negative_array_size(w 2, w 9, w 2, w     │   movq %rax, %rbx                                │
    │                                                      │             │ 14)                                                                          │   movl $16, %edi                                 │
    │                                                      │             │         hlt                                                                  │   callq malloc                                   │
    │                                                      │             │                                                                              │   movq %rax, %rcx                                │
    │                                                      │             │ @b_non_negative_array_size_3                                                 │   movq %rbx, (%rcx)                              │
    │                                                      │             │         jnz %t_0, @b_positive_array_size_5, @b_zero_array_size_4             │   movl $0, 8(%rcx)                               │
    │                                                      │             │                                                                              │   movl $6, 12(%rcx)                              │
    │                                                      │             │ @b_zero_array_size_4                                                         │   movl $6, %eax                                  │
    │                                                      │             │         %t_8 =l call $malloc(l 16)                                           │ .Lbb3:                                           │
    │                                                      │             │         %t_9 =l add %t_8, 0                                                  │   movq $5, (%rbx)                                │
    │                                                      │             │         storel 0, %t_9                                                       │   addq $8, %rbx                                  │
    │                                                      │             │         %t_10 =l add %t_8, 8                                                 │   subq $1, %rax                                  │
    │                                                      │             │         storew 0, %t_10                                                      │   jnz .Lbb3                                      │
    │                                                      │             │         %t_11 =l add %t_8, 12                                                │   movq %rcx, %rax                                │
    │                                                      │             │         storew 0, %t_11                                                      │   popq %rbx                                      │
    │                                                      │             │         %t_7 =l copy %t_8                                                    │   leave                                          │
    │                                                      │             │         jmp @b_init_loop_exit_6                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_intArray_0, @function                    │
    │                                                      │             │ @b_positive_array_size_5                                                     │ .size g_intArray_0, .-g_intArray_0               │
    │                                                      │             │         %t_12 =l copy %t_0                                                   │ /* end function g_intArray_0 */                  │
    │                                                      │             │         %t_13 =l mul %t_0, 8                                                 │                                                  │
    │                                                      │             │         %t_14 =l call $malloc(l %t_13)                                       │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │         %t_15 =l call $malloc(l 16)                                          │                                                  │
    │                                                      │             │         %t_16 =l add %t_15, 0                                                │                                                  │
    │                                                      │             │         storel %t_14, %t_16                                                  │                                                  │
    │                                                      │             │         %t_17 =l add %t_15, 8                                                │                                                  │
    │                                                      │             │         storew 0, %t_17                                                      │                                                  │
    │                                                      │             │         %t_18 =l add %t_15, 12                                               │                                                  │
    │                                                      │             │         storew %t_0, %t_18                                                   │                                                  │
    │                                                      │             │         jmp @b_array_init_loop_begin_19                                      │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_init_loop_exit_6                                                          │                                                  │
    │                                                      │             │         ret %t_7                                                             │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_array_init_loop_begin_19                                                  │                                                  │
    │                                                      │             │         %t_21 =l add %t_14, 0                                                │                                                  │
    │                                                      │             │         storel 5, %t_21                                                      │                                                  │
    │                                                      │             │         %t_14 =l add %t_14, 8                                                │                                                  │
    │                                                      │             │         %t_12 =l sub %t_12, 1                                                │                                                  │
    │                                                      │             │         jnz %t_12, @b_array_init_loop_begin_19, @b_array_init_loop_exit_20   │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ @b_array_init_loop_exit_20                                                   │                                                  │
    │                                                      │             │         %t_7 =l copy %t_15                                                   │                                                  │
    │                                                      │             │         jmp @b_init_loop_exit_6                                              │                                                  │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun charArray(i : int): []mut char {                 │             │ function l $g_charArray_0(l %l_i_0, )                                        │ .text                                            │
    │     new [i]char('a');                                │             │ {                                                                            │ g_charArray_0:                                   │
    │ }                                                    │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =w csltl %l_i_0, 0                                              │   movq %rsp, %rbp                                │
    │                                                      │             │         jnz %t_0, @b_negative_array_size_1, @b_non_negative_array_size_2     │   pushq %rbx                                     │
    │                                                      │             │                                                                              │   pushq %r12                                     │
    │                                                      │             │ @b_negative_array_size_1                                                     │   cmpq $0, %rdi                                  │
    │                                                      │             │         call $panic_cannot_allocate_negative_array_size(w 2, w 9, w 2, w     │   jl .Lbb6                                       │
    │                                                      │             │ 10)                                                                          │   cmpl $0, %edi                                  │
    │                                                      │             │         hlt                                                                  │   jnz .Lbb3                                      │
    │                                                      │             │                                                                              │   movl $16, %edi                                 │
    │                                                      │             │ @b_non_negative_array_size_2                                                 │   callq malloc                                   │
    │                                                      │             │         jnz %l_i_0, @b_positive_array_size_4, @b_zero_array_size_3           │   movq $0, (%rax)                                │
    │                                                      │             │                                                                              │   movl $0, 8(%rax)                               │
    │                                                      │             │ @b_zero_array_size_3                                                         │   movl $0, 12(%rax)                              │
    │                                                      │             │         %t_7 =l call $malloc(l 16)                                           │   jmp .Lbb5                                      │
    │                                                      │             │         %t_8 =l add %t_7, 0                                                  │ .Lbb3:                                           │
    │                                                      │             │         storel 0, %t_8                                                       │   movq %rdi, %rbx                                │
    │                                                      │             │         %t_9 =l add %t_7, 8                                                  │   callq malloc                                   │
    │                                                      │             │         storew 0, %t_9                                                       │   movq %rbx, %rdi                                │
    │                                                      │             │         %t_10 =l add %t_7, 12                                                │   movq %rax, %rbx                                │
    │                                                      │             │         storew 0, %t_10                                                      │   movq %rdi, %r12                                │
    │                                                      │             │         %t_6 =l copy %t_7                                                    │   movl $16, %edi                                 │
    │                                                      │             │         jmp @b_init_loop_exit_5                                              │   callq malloc                                   │
    │                                                      │             │                                                                              │   movq %r12, %rdi                                │
    │                                                      │             │ @b_positive_array_size_4                                                     │   movq %rbx, (%rax)                              │
    │                                                      │             │         %t_11 =l copy %l_i_0                                                 │   movl $0, 8(%rax)                               │
    │                                                      │             │         %t_12 =l mul %l_i_0, 1                                               │   movl %edi, 12(%rax)                            │
    │                                                      │             │         %t_13 =l call $malloc(l %t_12)                                       │ .Lbb4:                                           │
    │                                                      │             │         %t_14 =l call $malloc(l 16)                                          │   movb $97, (%rbx)                               │
    │                                                      │             │         %t_15 =l add %t_14, 0                                                │   addq $1, %rbx                                  │
    │                                                      │             │         storel %t_13, %t_15                                                  │   subq $1, %rdi                                  │
    │                                                      │             │         %t_16 =l add %t_14, 8                                                │   jnz .Lbb4                                      │
    │                                                      │             │         storew 0, %t_16                                                      │ .Lbb5:                                           │
    │                                                      │             │         %t_17 =l add %t_14, 12                                               │   popq %r12                                      │
    │                                                      │             │         storew %l_i_0, %t_17                                                 │   popq %rbx                                      │
    │                                                      │             │         jmp @b_array_init_loop_begin_18                                      │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │ @b_init_loop_exit_5                                                          │ .Lbb6:                                           │
    │                                                      │             │         ret %t_6                                                             │   movl $10, %ecx                                 │
    │                                                      │             │                                                                              │   movl $2, %edx                                  │
    │                                                      │             │ @b_array_init_loop_begin_18                                                  │   movl $9, %esi                                  │
    │                                                      │             │         %t_20 =l add %t_13, 0                                                │   movl $2, %edi                                  │
    │                                                      │             │         storeb 97, %t_20                                                     │   callq                                          │
    │                                                      │             │         %t_13 =l add %t_13, 1                                                │ panic_cannot_allocate_negative_array_size        │
    │                                                      │             │         %t_11 =l sub %t_11, 1                                                │   ud2                                            │
    │                                                      │             │         jnz %t_11, @b_array_init_loop_begin_18, @b_array_init_loop_exit_19   │ .type g_charArray_0, @function                   │
    │                                                      │             │                                                                              │ .size g_charArray_0, .-g_charArray_0             │
    │                                                      │             │ @b_array_init_loop_exit_19                                                   │ /* end function g_charArray_0 */                 │
    │                                                      │             │         %t_6 =l copy %t_14                                                   │                                                  │
    │                                                      │             │         jmp @b_init_loop_exit_5                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │ }                                                                            │                                                  │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │   const x : int = 10;                                │ g_main_1    │ function l $g_main_1()                                                       │ .text                                            │
    │                                                      │             │ {                                                                            │ g_main_1:                                        │
    │   fun main(): int { x }                              │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         ret 10                                                               │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movl $10, %eax                                 │
    │                                                      │             │ }                                                                            │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │                                                                              │ /* end function g_main_1 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │   const dialog_options : [][]char =                  │ g_main_1    │ data $.static.2 = { l $.static.3, w 0, w 4, }                                │ .data                                            │
    │   [ "Wait"                                           │             │ data $.static.3 = { b "Wait", }                                              │ .balign 8                                        │
    │   , "Are our bodies really piles of dirt?"           │             │ data $.static.4 = { l $.static.5, w 0, w 36, }                               │ .static.2:                                       │
    │   , "And is the soul just a metaphor?"               │             │ data $.static.5 = { b "Are our bodies really piles of dirt?", }              │   .quad .static.3+0                              │
    │   , "I keep my head from looking too far up,"        │             │ data $.static.6 = { l $.static.7, w 0, w 32, }                               │   .int 0                                         │
    │   , "I fear that there is a heaven above."           │             │ data $.static.7 = { b "And is the soul just a metaphor?", }                  │   .int 4                                         │
    │     ];                                               │             │ data $.static.8 = { l $.static.9, w 0, w 39, }                               │ /* end data */                                   │
    │                                                      │             │ data $.static.9 = { b "I keep my head from looking too far up,", }           │                                                  │
    │   fun main(i: int): []char { dialog_options[i] }     │             │ data $.static.10 = { l $.static.11, w 0, w 36, }                             │ .data                                            │
    │                                                      │             │ data $.static.11 = { b "I fear that there is a heaven above.", }             │ .balign 8                                        │
    │                                                      │             │ data $g_dialog_options_0 = { l $.static.1, w 0, w 5, }                       │ .static.3:                                       │
    │                                                      │             │ data $g_dialog_options_0 = { l $.static.2, l $.static.4, l $.static.6, l     │   .ascii "Wait"                                  │
    │                                                      │             │ $.static.8, l $.static.10, }                                                 │ /* end data */                                   │
    │                                                      │             │ function l $g_main_1(l %l_i_0, )                                             │                                                  │
    │                                                      │             │ {                                                                            │ .data                                            │
    │                                                      │             │ @start                                                                       │ .balign 8                                        │
    │                                                      │             │         %t_0 =l add $g_dialog_options_0, 12                                  │ .static.4:                                       │
    │                                                      │             │         %t_1 =l loaduw %t_0                                                  │   .quad .static.5+0                              │
    │                                                      │             │         %t_2 =w csgel %l_i_0, 0                                              │   .int 0                                         │
    │                                                      │             │         %t_3 =w csltl %l_i_0, %t_1                                           │   .int 36                                        │
    │                                                      │             │         %t_4 =w mul %t_2, %t_3                                               │ /* end data */                                   │
    │                                                      │             │         jnz %t_4, @b_good_index_6, @b_bad_index_5                            │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │ @b_bad_index_5                                                               │ .balign 8                                        │
    │                                                      │             │         call $panic_index_out_of_bounds(w 9, w 44, w 9, w 45)                │ .static.5:                                       │
    │                                                      │             │         hlt                                                                  │   .ascii "Are our bodies really piles of dirt?"  │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │ @b_good_index_6                                                              │                                                  │
    │                                                      │             │         %t_7 =l add $g_dialog_options_0, 0                                   │ .data                                            │
    │                                                      │             │         %t_8 =l loadl %t_7                                                   │ .balign 8                                        │
    │                                                      │             │         %t_9 =l add $g_dialog_options_0, 8                                   │ .static.6:                                       │
    │                                                      │             │         %t_10 =l loaduw %t_9                                                 │   .quad .static.7+0                              │
    │                                                      │             │         %t_11 =l add %l_i_0, %t_10                                           │   .int 0                                         │
    │                                                      │             │         %t_12 =l mul %t_11, 8                                                │   .int 32                                        │
    │                                                      │             │         %t_13 =l add %t_8, %t_12                                             │ /* end data */                                   │
    │                                                      │             │         %t_14 =l loadl %t_13                                                 │                                                  │
    │                                                      │             │         ret %t_14                                                            │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │ }                                                                            │ .static.7:                                       │
    │                                                      │             │                                                                              │   .ascii "And is the soul just a metaphor?"      │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ .static.8:                                       │
    │                                                      │             │                                                                              │   .quad .static.9+0                              │
    │                                                      │             │                                                                              │   .int 0                                         │
    │                                                      │             │                                                                              │   .int 39                                        │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ .static.9:                                       │
    │                                                      │             │                                                                              │   .ascii "I keep my head from looking too far    │
    │                                                      │             │                                                                              │ up,"                                             │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ .static.10:                                      │
    │                                                      │             │                                                                              │   .quad .static.11+0                             │
    │                                                      │             │                                                                              │   .int 0                                         │
    │                                                      │             │                                                                              │   .int 36                                        │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ .static.11:                                      │
    │                                                      │             │                                                                              │   .ascii "I fear that there is a heaven above."  │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ g_dialog_options_0:                              │
    │                                                      │             │                                                                              │   .quad .static.1+0                              │
    │                                                      │             │                                                                              │   .int 0                                         │
    │                                                      │             │                                                                              │   .int 5                                         │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .data                                            │
    │                                                      │             │                                                                              │ .balign 8                                        │
    │                                                      │             │                                                                              │ g_dialog_options_0:                              │
    │                                                      │             │                                                                              │   .quad .static.2+0                              │
    │                                                      │             │                                                                              │   .quad .static.4+0                              │
    │                                                      │             │                                                                              │   .quad .static.6+0                              │
    │                                                      │             │                                                                              │   .quad .static.8+0                              │
    │                                                      │             │                                                                              │   .quad .static.10+0                             │
    │                                                      │             │                                                                              │ /* end data */                                   │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .text                                            │
    │                                                      │             │                                                                              │ g_main_1:                                        │
    │                                                      │             │                                                                              │   pushq %rbp                                     │
    │                                                      │             │                                                                              │   movq %rsp, %rbp                                │
    │                                                      │             │                                                                              │   movl g_dialog_options_0+12(%rip), %ecx         │
    │                                                      │             │                                                                              │   cmpq $0, %rdi                                  │
    │                                                      │             │                                                                              │   setge %al                                      │
    │                                                      │             │                                                                              │   movzbl %al, %eax                               │
    │                                                      │             │                                                                              │   cmpq %rcx, %rdi                                │
    │                                                      │             │                                                                              │   setl %cl                                       │
    │                                                      │             │                                                                              │   movzbl %cl, %ecx                               │
    │                                                      │             │                                                                              │   imull %ecx, %eax                               │
    │                                                      │             │                                                                              │   cmpl $0, %eax                                  │
    │                                                      │             │                                                                              │   jnz .Lbb2                                      │
    │                                                      │             │                                                                              │   movl $45, %ecx                                 │
    │                                                      │             │                                                                              │   movl $9, %edx                                  │
    │                                                      │             │                                                                              │   movl $44, %esi                                 │
    │                                                      │             │                                                                              │   movl $9, %edi                                  │
    │                                                      │             │                                                                              │   callq panic_index_out_of_bounds                │
    │                                                      │             │                                                                              │   ud2                                            │
    │                                                      │             │                                                                              │ .Lbb2:                                           │
    │                                                      │             │                                                                              │   movq g_dialog_options_0(%rip), %rax            │
    │                                                      │             │                                                                              │   movl g_dialog_options_0+8(%rip), %ecx          │
    │                                                      │             │                                                                              │   addq %rdi, %rcx                                │
    │                                                      │             │                                                                              │   movq (%rax, %rcx, 8), %rax                     │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_main_1, @function                        │
    │                                                      │             │                                                                              │ .size g_main_1, .-g_main_1                       │
    │                                                      │             │                                                                              │ /* end function g_main_1 */                      │
    │                                                      │             │                                                                              │                                                  │
    │                                                      │             │                                                                              │ .section .note.GNU-stack,"",@progbits            │
    │                                                      │             │                                                                              │                                                  │
    ├──────────────────────────────────────────────────────┼─────────────┼──────────────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
    │ fun leq(x: int, y: int) : bool { x <= y }            │             │ function ub $g_leq_0(l %l_x_0, l %l_y_1, )                                   │ .text                                            │
    │                                                      │             │ {                                                                            │ g_leq_0:                                         │
    │                                                      │             │ @start                                                                       │   pushq %rbp                                     │
    │                                                      │             │         %t_0 =w cslel %l_x_0, %l_y_1                                         │   movq %rsp, %rbp                                │
    │                                                      │             │         ret %t_0                                                             │   cmpq %rsi, %rdi                                │
    │                                                      │             │                                                                              │   setle %al                                      │
    │                                                      │             │ }                                                                            │   movzbl %al, %eax                               │
    │                                                      │             │                                                                              │   leave                                          │
    │                                                      │             │                                                                              │   ret                                            │
    │                                                      │             │                                                                              │ .type g_leq_0, @function                         │
    │                                                      │             │                                                                              │ .size g_leq_0, .-g_leq_0                         │
    │                                                      │             │                                                                              │ /* end function g_leq_0 */                       │
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
  [%expect
    {|
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
    │                                                                      │             │         %t_17 =l add %l_a_array_0, 0                           │   movzbl %al, %eax                    │
    │                                                                      │             │         %t_18 =l loadl %t_17                                   │   imull $1, %eax, %eax                │
    │                                                                      │             │         %t_19 =l add %l_a_array_0, 8                           │   cmpl $0, %eax                       │
    │                                                                      │             │         %t_20 =l loaduw %t_19                                  │   jnz .Lbb2                           │
    │                                                                      │             │         %t_21 =l add 0, %t_20                                  │   movl $19, %ecx                      │
    │                                                                      │             │         %t_22 =l mul %t_21, 8                                  │   movl $5, %edx                       │
    │                                                                      │             │         %t_23 =l add %t_18, %t_22                              │   movl $18, %esi                      │
    │                                                                      │             │         %t_24 =l copy %l_a_1                                   │   movl $5, %edi                       │
    │                                                                      │             │         %t_25 =l add %t_23, 0                                  │   callq panic_index_out_of_bounds     │
    │                                                                      │             │         %t_26 =l loadl %t_25                                   │   ud2                                 │
    │                                                                      │             │         %t_27 =l add %t_23, 0                                  │ .Lbb2:                                │
    │                                                                      │             │         storel %t_24, %t_27                                    │   movq (%r12), %rax                   │
    │                                                                      │             │         %l_a_1 =l copy %t_26                                   │   movl 8(%r12), %ecx                  │
    │                                                                      │             │         ret 0                                                  │   addq $0, %rcx                       │
    │                                                                      │             │                                                                │   movq %rbx, (%rax, %rcx, 8)          │
    │                                                                      │             │ }                                                              │   movl $0, %eax                       │
    │                                                                      │             │ function $constructor_A(l %this, l %l_id_0, )                  │   popq %r13                           │
    │                                                                      │             │ {                                                              │   popq %r12                           │
    │                                                                      │             │ @start                                                         │   popq %rbx                           │
    │                                                                      │             │         %t_0 =l add %this, 8                                   │   leave                               │
    │                                                                      │             │         storel %l_id_0, %t_0                                   │   ret                                 │
    │                                                                      │             │         %t_1 =l add %this, 0                                   │ .type g_main_0, @function             │
    │                                                                      │             │         storel $vtable_A, %t_1                                 │ .size g_main_0, .-g_main_0            │
    │                                                                      │             │         ret                                                    │ /* end function g_main_0 */           │
    │                                                                      │             │                                                                │                                       │
    │                                                                      │             │ }                                                              │ .text                                 │
    │                                                                      │             │ data $vtable_A = { l 0, }                                      │ constructor_A:                        │
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
    │                                                                      │             │                                                                │ .data                                 │
    │                                                                      │             │                                                                │ .balign 8                             │
    │                                                                      │             │                                                                │ vtable_A:                             │
    │                                                                      │             │                                                                │   .quad 0                             │
    │                                                                      │             │                                                                │ /* end data */                        │
    │                                                                      │             │                                                                │                                       │
    │                                                                      │             │                                                                │ .section .note.GNU-stack,"",@progbits │
    │                                                                      │             │                                                                │                                       │
    └──────────────────────────────────────────────────────────────────────┴─────────────┴────────────────────────────────────────────────────────────────┴───────────────────────────────────────┘
    |}];
  return ()
;;
