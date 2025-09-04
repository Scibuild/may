open! Core

let ast_or_error_to_string result =
  match result with
  | Ok ast -> May.Ast.Expr.to_string ast
  | Error e -> May.Comp_error.to_string e
;;

let ast_decl_or_error_to_string result =
  match result with
  | Ok ast -> List.map ast ~f:May.Ast.Decl.to_string |> String.concat_lines
  | Error e -> May.Comp_error.to_string e
;;

let%expect_test "simple expr tests" =
  let programs =
    [ "10"
    ; "1 + 2"
    ; "5 * 3"
    ; "5 + 3 * 7"
    ; "6 / 3 - 1"
    ; "-5"
    ; "5 + - 2"
    ; "5 - - 2"
    ; "-2 * -3"
    ; "4 + (2 * 3 - 1)"
    ; "1 / 2 + 3 * 4 - 5 / (6 + 7)"
    ; "3 < 4 && 5 <= 6 || !(1 == 2) && 5 != 7"
    ; "'a'"
    ; "40c"
    ; "[1, 4, 2, 6, 4 + 2]"
    ; "null"
    ; "f + x orelse 4 + 2"
    ; "\"hello world!\""
    ; "\"\\n\\x20\\x41\""
    ; "1 <= 2"
    ]
  in
  let test program =
    let ast = program |> May.For_testing.expr_parse_string |> ast_or_error_to_string in
    [%sexp { program : string; ast : string }]
  in
  programs |> List.map ~f:test |> Expectable.print;
  [%expect
    {|
    ┌────────────────────────────────────────┬──────────────────────────────────────────────────────┐
    │ program                                │ ast                                                  │
    ├────────────────────────────────────────┼──────────────────────────────────────────────────────┤
    │ 10                                     │ 10                                                   │
    │ 1 + 2                                  │ (1 + 2)                                              │
    │ 5 * 3                                  │ (5 * 3)                                              │
    │ 5 + 3 * 7                              │ (5 + (3 * 7))                                        │
    │ 6 / 3 - 1                              │ ((6 / 3) - 1)                                        │
    │ -5                                     │ (-5)                                                 │
    │ 5 + - 2                                │ (5 + (-2))                                           │
    │ 5 - - 2                                │ (5 - (-2))                                           │
    │ -2 * -3                                │ ((-2) * (-3))                                        │
    │ 4 + (2 * 3 - 1)                        │ (4 + ((2 * 3) - 1))                                  │
    │ 1 / 2 + 3 * 4 - 5 / (6 + 7)            │ (((1 / 2) + (3 * 4)) - (5 / (6 + 7)))                │
    │ 3 < 4 && 5 <= 6 || !(1 == 2) && 5 != 7 │ (((3 < 4) && (5 <= 6)) || ((!(1 == 2)) && (5 != 7))) │
    │ 'a'                                    │ 'a'                                                  │
    │ 40c                                    │ '('                                                  │
    │ [1, 4, 2, 6, 4 + 2]                    │ [1, 4, 2, 6, (4 + 2)]                                │
    │ null                                   │ null                                                 │
    │ f + x orelse 4 + 2                     │ (f + x) orelse (4 + 2)                               │
    │ "hello world!"                         │ "hello world!"                                       │
    │ "\n\x20\x41"                           │ "                                                    │
    │                                        │  A"                                                  │
    │ 1 <= 2                                 │ (1 <= 2)                                             │
    └────────────────────────────────────────┴──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "complex expr tests" =
  let programs =
    [ "{}"
    ; "( )"
    ; "{ let x = 10; }"
    ; "{ x = 10; }"
    ; "if x + 5 { 4 } else { 2 }"
    ; "f(1, 2)"
    ; "{ let xyz = 10; while (xyz > 0) { print(xyz); x = x - 1; } }"
    ; "i.love().to_chain[my].accessors"
    ; "-x()"
    ; "print(hello_world)"
    ; "new A(10, 30)"
    ; "new A(skdfj).?"
    ; "if? x = null { nah } else { yay }"
    ]
  in
  let test program =
    let ast = program |> May.For_testing.expr_parse_string |> ast_or_error_to_string in
    [%sexp { program : string; ast : string }]
  in
  programs |> List.map ~f:test |> Expectable.print;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────────┐
    │ program                                                      │ ast                                                                     │
    ├──────────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────┤
    │ {}                                                           │ {  }                                                                    │
    │ ( )                                                          │ ()                                                                      │
    │ { let x = 10; }                                              │ { (let x = 10) }                                                        │
    │ { x = 10; }                                                  │ { (x = 10) }                                                            │
    │ if x + 5 { 4 } else { 2 }                                    │ (if ((x + 5)) { 4 } else { 2 })                                         │
    │ f(1, 2)                                                      │ (f(1, 2))                                                               │
    │ { let xyz = 10; while (xyz > 0) { print(xyz); x = x - 1; } } │ { (let xyz = 10); (while ((xyz > 0)) { (print(xyz)); (x = (x - 1)) }) } │
    │ i.love().to_chain[my].accessors                              │ (((((i.love)()).to_chain)[my]).accessors)                               │
    │ -x()                                                         │ (-(x()))                                                                │
    │ print(hello_world)                                           │ (print(hello_world))                                                    │
    │ new A(10, 30)                                                │ (new A(10, 30))                                                         │
    │ new A(skdfj).?                                               │ (new A(skdfj)).?                                                        │
    │ if? x = null { nah } else { yay }                            │ (if? x = (null) { nah } else { yay })                                   │
    └──────────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "decl tests" =
  let programs =
    [ "fun my_func(x : int, y : string) : unit { let z = x + y; g(z) }"
    ; {|
      const my_const = 100 + 200; 
      fun static_function(something : int) : unit { 
       something * 2
      }
      |}
    ; "const my_array: []int = [4, 2, 7];"
    ; "const my_array: []mut int = [4, 2, 7];"
    ; {|
      class A { 
        value : int; 
        other_value : string; 
        fun print() : unit {
          print(this.value);  
          print(this.other_value)
        } 
      }
      |}
    ; {|
      class B < A { 
        public overrides other_value : string; 
        public mut new_field : bool;

        private fun print2() : unit {
          print(this.value);  
          print(this.other_value)
        } 
      }
      |}
    ; {|
      class C < A { 
        i : int;
        s : string;

        constructor(i : int, s : string) { 
          super();
          this.i = i;
          this.s = s;
        }

        constructor(i : int, s : string) evolves A { 
          this.i = i;
          this.s = s;
        }

        fun print2() : unit {
          print(this.i);  
          print(this.s);
        } 
      }
      |}
    ]
  in
  let test program =
    let program = Utils.remove_indentation program in
    let ast = program |> May.For_testing.parse_string |> ast_decl_or_error_to_string in
    [%sexp { program : string; ast : string }]
  in
  programs |> List.map ~f:test |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────┐
    │ program                                                         │ ast                                                                                  │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ fun my_func(x : int, y : string) : unit { let z = x + y; g(z) } │ fun my_func(x : int, y : string) : unit { (let z = (x + y)); (g(z)) }                │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ const my_const = 100 + 200;                                     │ const my_const =  (100 + 200);                                                       │
    │ fun static_function(something : int) : unit {                   │ fun static_function(something : int) : unit { (something * 2) }                      │
    │  something * 2                                                  │                                                                                      │
    │ }                                                               │                                                                                      │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ const my_array: []int = [4, 2, 7];                              │ const my_array : ([]int) =  [4, 2, 7];                                               │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ const my_array: []mut int = [4, 2, 7];                          │ const my_array : ([]mut int) =  [4, 2, 7];                                           │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ class A {                                                       │                                                                                      │
    │   value : int;                                                  │ class A {                                                                            │
    │   other_value : string;                                         │   value : int;                                                                       │
    │   fun print() : unit {                                          │   other_value : string;                                                              │
    │     print(this.value);                                          │   fun print() : unit { (print((this.value))); (print((this.other_value))) }          │
    │     print(this.other_value)                                     │ }                                                                                    │
    │   }                                                             │                                                                                      │
    │ }                                                               │                                                                                      │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ class B < A {                                                   │                                                                                      │
    │   public overrides other_value : string;                        │ class B < A {                                                                        │
    │   public mut new_field : bool;                                  │   public overrides other_value : string;                                             │
    │                                                                 │   public mut new_field : bool;                                                       │
    │   private fun print2() : unit {                                 │   private fun print2() : unit { (print((this.value))); (print((this.other_value))) } │
    │     print(this.value);                                          │ }                                                                                    │
    │     print(this.other_value)                                     │                                                                                      │
    │   }                                                             │                                                                                      │
    │ }                                                               │                                                                                      │
    │                                                                 │                                                                                      │
    ├─────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────┤
    │ class C < A {                                                   │                                                                                      │
    │   i : int;                                                      │ class C < A {                                                                        │
    │   s : string;                                                   │   i : int;                                                                           │
    │                                                                 │   s : string;                                                                        │
    │   constructor(i : int, s : string) {                            │   constructor(i : int, s : string) { (super()); ((this.i) = i); ((this.s) = s) }     │
    │     super();                                                    │   constructor(i : int, s : string) evolves A { ((this.i) = i); ((this.s) = s) }      │
    │     this.i = i;                                                 │   fun print2() : unit { (print((this.i))); (print((this.s))) }                       │
    │     this.s = s;                                                 │ }                                                                                    │
    │   }                                                             │                                                                                      │
    │                                                                 │                                                                                      │
    │   constructor(i : int, s : string) evolves A {                  │                                                                                      │
    │     this.i = i;                                                 │                                                                                      │
    │     this.s = s;                                                 │                                                                                      │
    │   }                                                             │                                                                                      │
    │                                                                 │                                                                                      │
    │   fun print2() : unit {                                         │                                                                                      │
    │     print(this.i);                                              │                                                                                      │
    │     print(this.s);                                              │                                                                                      │
    │   }                                                             │                                                                                      │
    │ }                                                               │                                                                                      │
    │                                                                 │                                                                                      │
    └─────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
