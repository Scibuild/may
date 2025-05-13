open! Core

let%expect_test "type checking expressions/statements" =
  let programs =
    [ "-1 / 2 + 3 * 4 - 5 / (6 + 7)"
    ; "3 < 4 && 5 <= 6 || !(1 == 2) && 5 != 7"
    ; "' ' + 65c"
    ; "{ let x = 10; x }"
    ; "{ let x : int = 10; x }"
    ; "true == false"
    ; "{ let x : unit = () ; x}"
    ; "{}"
    ; "{ let x : int = if (5 < 10) { 5 } else { 7 }; x }"
    ; " {\n\
      \   let x = 10; \n\
      \   let total = 0; \n\
      \   while (x > 0) {\n\
      \     total = total + x; \n\
      \     x = x - 1; \n\
      \   } \n\
      \   total \n\
      \ }"
    ; "{ let x = 10; {let x = 20; ()} }"
    ; "[4, 2, 7, 5]"
    ; "{ let a : []int = []; a}"
    ; "{ let a : []int = [2, 5, 7]; a}"
    ; "{ let a : []mut int = [2, 5, 7]; a[1] = 10; a[1]}"
    ; "{ let x : ?object = null; if? y = x { 10 } else { 20 }}"
    ]
  in
  let test program =
    let ast =
      program
      |> May.For_testing.expr_parse_string
      |> Result.map_error ~f:May.Comp_error.to_string
      |> Result.ok_or_failwith
    in
    let check =
      May.Check.For_testing.with_function_check
        (May.Check.empty ())
        (May.Check.For_testing.Function_check.create ~return_type:May.Type.Unit ())
    in
    let type_ =
      May.Check.infer_expr check ~term:ast
      |> Result.map_error ~f:May.Comp_error.to_string
      |> Result.ok_or_failwith
      |> May.Tast.Expr.ty
      |> May.Check.For_testing.Type.to_string check
    in
    [%sexp { program : string; type_ : string }]
  in
  programs |> List.map ~f:test |> Expectable.print;
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────┬───────────┐
    │ program                                                 │ type_     │
    ├─────────────────────────────────────────────────────────┼───────────┤
    │ -1 / 2 + 3 * 4 - 5 / (6 + 7)                            │ int       │
    │ 3 < 4 && 5 <= 6 || !(1 == 2) && 5 != 7                  │ bool      │
    │ ' ' + 65c                                               │ char      │
    │ { let x = 10; x }                                       │ int       │
    │ { let x : int = 10; x }                                 │ int       │
    │ true == false                                           │ bool      │
    │ { let x : unit = () ; x}                                │ unit      │
    │ {}                                                      │ unit      │
    │ { let x : int = if (5 < 10) { 5 } else { 7 }; x }       │ int       │
    │  {                                                      │ int       │
    │    let x = 10;                                          │           │
    │    let total = 0;                                       │           │
    │    while (x > 0) {                                      │           │
    │      total = total + x;                                 │           │
    │      x = x - 1;                                         │           │
    │    }                                                    │           │
    │    total                                                │           │
    │  }                                                      │           │
    │ { let x = 10; {let x = 20; ()} }                        │ unit      │
    │ [4, 2, 7, 5]                                            │ []mut int │
    │ { let a : []int = []; a}                                │ []int     │
    │ { let a : []int = [2, 5, 7]; a}                         │ []int     │
    │ { let a : []mut int = [2, 5, 7]; a[1] = 10; a[1]}       │ int       │
    │ { let x : ?object = null; if? y = x { 10 } else { 20 }} │ int       │
    └─────────────────────────────────────────────────────────┴───────────┘
    |}]
;;

let%expect_test "type checking expressions/statements errors" =
  let programs =
    [ "5 + x"
    ; "(4 == 3) + 2"
    ; "{ 5 + 3; true}"
    ; "{ let x : bool = 10; x }"
    ; "!10"
    ; "-true"
    ; "{ 5 = 10; }"
    ; "{ let x = 10; let x = 20; x }"
    ; "{ return 5; }"
    ; "[]"
    ; "{ let a : []int = [2, 5, 7]; a[1] = 10; a[1]}"
    ; "null"
    ]
  in
  let test program =
    let ast =
      program
      |> May.For_testing.expr_parse_string
      |> Result.map_error ~f:May.Comp_error.to_string
      |> Result.ok_or_failwith
    in
    let check =
      May.Check.For_testing.with_function_check
        (May.Check.empty ())
        (May.Check.For_testing.Function_check.create ~return_type:May.Type.Unit ())
    in
    let error =
      May.Check.infer_expr check ~term:ast
      |> Result.error
      |> Option.value_exn
      |> May.Comp_error.to_string
    in
    [%sexp { program : string; error : string }]
  in
  programs |> List.map ~f:test |> Expectable.print;
  [%expect
    {|
    ┌───────────────────────────────────────────────┬────────────────────────────────────────────────────────────────────────────────────────┐
    │ program                                       │ error                                                                                  │
    ├───────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────┤
    │ 5 + x                                         │ Type error at 1:5-1:6: Unknown identifier x                                            │
    │ (4 == 3) + 2                                  │ Type error at 1:1-1:13: Unexpected types bool and int as operands to binary operator + │
    │ { 5 + 3; true}                                │ Type error at 1:3-1:8: Type int is not included in type unit                           │
    │ { let x : bool = 10; x }                      │ Type error at 1:18-1:20: Type int is not included in type bool                         │
    │ !10                                           │ Type error at 1:1-1:4: Unexpected type int as operand to unary operator !              │
    │ -true                                         │ Type error at 1:1-1:6: Unexpected type bool as operand to unary operator -             │
    │ { 5 = 10; }                                   │ Type error at 1:3-1:4: Cannot assign to expression                                     │
    │ { let x = 10; let x = 20; x }                 │ Type error at 1:15-1:26: Identifier defined multiple times within the same scope x     │
    │ { return 5; }                                 │ Type error at 1:10-1:11: Type int is not included in type unit                         │
    │ []                                            │ Type error at 1:1-1:3: Cannot infer type of empty array.                               │
    │ { let a : []int = [2, 5, 7]; a[1] = 10; a[1]} │ Type error at 1:30-1:34: Cannot write to immutable array []int                         │
    │ null                                          │ Type error at 1:1-1:5: Cannot infer type of null                                       │
    └───────────────────────────────────────────────┴────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "type checking const and fn decls" =
  let programs =
    [ "const x = 10;"
    ; " fun double(x : int): int { x * 2 } \n\
      \ fun quadruple(x : int): int {double(double(x))}"
    ; "module Sub { const x = 1; } const y = 2; "
    ; "fun yay(): unit { return; }"
    ; "fun yay(): int { return 5; 1 }"
    ; "fun yay(): int { let x = { return 5; } ; 1 }" (* Is this a reasonable program? *)
    ; {|
      module A {
          const x = 10;
          module B {
            const y = 'y';
          }
      }
      fun x(): char {
          A.B.y
      }|}
    ; "class A { constructor() {} }"
    ; {|
      class Counter {
        id : int;
        mut value : int;

        constructor(id : int) {
          this.id = id;
          this.value = 0;
        }
      }
      |}
    ; {|
      class Super { constructor () {}}
      class Sub < Super { 
        constructor () { super() }
      }
      class A {
        id : int;
        mut value : int;
        other : Super;

        constructor() { 
          this.id = 10; 
          this.value = 0; 
          this.other = new Super(); 
        }
        fun something(x : int) : []int { [x] }
      }

      class B < A {
        y : int;
        overrides other : Sub;

        constructor () { 
          super(); 
          this.y = -1; 
          this.other = new Sub(); 
        }

        overrides 
        fun something(x : int) : []mut int { 
          [x, this.y] 
        }
      }
      |}
    ; {|
      class A {
        constructor () {}
        fun something(x : int) : []int { [x] }
      }
        
      fun f(v : A): []int {
        v:something(10)
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

        fun cons(v : int) : LinkedList {
          let new_head = new LinkedList(v);
          new_head:setNext(this);
          new_head
        }

        fun sum() : int {
          this.v + (if? n = this.next { n:sum() } else { 0 })
        }
      }
      |}
    ]
  in
  let test program =
    let program = Utils.remove_indentation program in
    May.Resolved_ident.Global.Id.For_testing.reset_counter ();
    May.Type.Class_id.For_testing.reset_counter ();
    let ast =
      program
      |> May.For_testing.parse_string
      |> Result.map_error ~f:May.Comp_error.to_string
      |> Result.ok_or_failwith
    in
    let check = May.Check.empty () in
    let error =
      May.Check.check_decls check ~decls:ast
      |> Result.map_error ~f:May.Comp_error.to_string
      |> Result.error
    in
    let check_string = May.Check.For_testing.to_string check in
    [%sexp { program : string; check_string : string; error : string option }]
  in
  programs |> List.map ~f:test |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────────────────┬───────┐
    │ program                                                 │ check_string                                                                    │ error │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ const x = 10;                                           │ Globals:                                                                        │       │
    │                                                         │ x: int                                                                          │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun double(x : int): int { x * 2 }                      │ Globals:                                                                        │       │
    │ fun quadruple(x : int): int {double(double(x))}         │ quadruple: (int) -> int                                                         │       │
    │                                                         │ double: (int) -> int                                                            │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ module Sub { const x = 1; } const y = 2;                │ Globals:                                                                        │       │
    │                                                         │ y: int                                                                          │       │
    │                                                         │ Sub.x: int                                                                      │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): unit { return; }                             │ Globals:                                                                        │       │
    │                                                         │ yay: () -> unit                                                                 │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): int { return 5; 1 }                          │ Globals:                                                                        │       │
    │                                                         │ yay: () -> int                                                                  │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): int { let x = { return 5; } ; 1 }            │ Globals:                                                                        │       │
    │                                                         │ yay: () -> int                                                                  │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ module A {                                              │ Globals:                                                                        │       │
    │     const x = 10;                                       │ A.B.y: char                                                                     │       │
    │     module B {                                          │ x: () -> char                                                                   │       │
    │       const y = 'y';                                    │ A.x: int                                                                        │       │
    │     }                                                   │                                                                                 │       │
    │ }                                                       │ Classes:                                                                        │       │
    │ fun x(): char {                                         │                                                                                 │       │
    │     A.B.y                                               │                                                                                 │       │
    │ }                                                       │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class A { constructor() {} }                            │ Globals:                                                                        │       │
    │                                                         │                                                                                 │       │
    │                                                         │ Classes:                                                                        │       │
    │                                                         │ A: ((id 0) (fields ()) (constructor ((args ()))) (methods ()) (super ()))       │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class Counter {                                         │ Globals:                                                                        │       │
    │   id : int;                                             │                                                                                 │       │
    │   mut value : int;                                      │ Classes:                                                                        │       │
    │                                                         │ Counter: ((id 0)                                                                │       │
    │   constructor(id : int) {                               │  (fields                                                                        │       │
    │     this.id = id;                                       │   ((id                                                                          │       │
    │     this.value = 0;                                     │     ((ty (Numeric Int)) (mut false) (overrides false) (visibility Private)))    │       │
    │   }                                                     │    (value                                                                       │       │
    │ }                                                       │     ((ty (Numeric Int)) (mut true) (overrides false) (visibility Private)))))   │       │
    │                                                         │  (constructor ((args ((Numeric Int))))) (methods ()) (super ()))                │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class Super { constructor () {}}                        │ Globals:                                                                        │       │
    │ class Sub < Super {                                     │                                                                                 │       │
    │   constructor () { super() }                            │ Classes:                                                                        │       │
    │ }                                                       │ Sub: ((id 1) (fields ()) (constructor ((args ()))) (methods ()) (super (0)))    │       │
    │ class A {                                               │ B: ((id 3)                                                                      │       │
    │   id : int;                                             │  (fields                                                                        │       │
    │   mut value : int;                                      │   ((other                                                                       │       │
    │   other : Super;                                        │     ((ty (Object 1)) (mut false) (overrides true) (visibility Private)))        │       │
    │                                                         │    (y                                                                           │       │
    │   constructor() {                                       │     ((ty (Numeric Int)) (mut false) (overrides false) (visibility Private)))))  │       │
    │     this.id = 10;                                       │  (constructor ((args ())))                                                      │       │
    │     this.value = 0;                                     │  (methods                                                                       │       │
    │     this.other = new Super();                           │   ((something                                                                   │       │
    │   }                                                     │     ((visibility Public)                                                        │       │
    │   fun something(x : int) : []int { [x] }                │      (function_                                                                 │       │
    │ }                                                       │       ((args ((Numeric Int))) (ret (Array (mut true) (elt (Numeric Int))))))    │       │
    │                                                         │      (overrides true)))))                                                       │       │
    │ class B < A {                                           │  (super (2)))                                                                   │       │
    │   y : int;                                              │ A: ((id 2)                                                                      │       │
    │   overrides other : Sub;                                │  (fields                                                                        │       │
    │                                                         │   ((id                                                                          │       │
    │   constructor () {                                      │     ((ty (Numeric Int)) (mut false) (overrides false) (visibility Private)))    │       │
    │     super();                                            │    (other                                                                       │       │
    │     this.y = -1;                                        │     ((ty (Object 0)) (mut false) (overrides false) (visibility Private)))       │       │
    │     this.other = new Sub();                             │    (value                                                                       │       │
    │   }                                                     │     ((ty (Numeric Int)) (mut true) (overrides false) (visibility Private)))))   │       │
    │                                                         │  (constructor ((args ())))                                                      │       │
    │   overrides                                             │  (methods                                                                       │       │
    │   fun something(x : int) : []mut int {                  │   ((something                                                                   │       │
    │     [x, this.y]                                         │     ((visibility Public)                                                        │       │
    │   }                                                     │      (function_                                                                 │       │
    │ }                                                       │       ((args ((Numeric Int))) (ret (Array (mut false) (elt (Numeric Int))))))   │       │
    │                                                         │      (overrides false)))))                                                      │       │
    │                                                         │  (super ()))                                                                    │       │
    │                                                         │ Super: ((id 0) (fields ()) (constructor ((args ()))) (methods ()) (super ()))   │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class A {                                               │ Globals:                                                                        │       │
    │   constructor () {}                                     │ f: (A) -> []int                                                                 │       │
    │   fun something(x : int) : []int { [x] }                │                                                                                 │       │
    │ }                                                       │ Classes:                                                                        │       │
    │                                                         │ A: ((id 0) (fields ()) (constructor ((args ())))                                │       │
    │ fun f(v : A): []int {                                   │  (methods                                                                       │       │
    │   v:something(10)                                       │   ((something                                                                   │       │
    │ }                                                       │     ((visibility Public)                                                        │       │
    │                                                         │      (function_                                                                 │       │
    │                                                         │       ((args ((Numeric Int))) (ret (Array (mut false) (elt (Numeric Int))))))   │       │
    │                                                         │      (overrides false)))))                                                      │       │
    │                                                         │  (super ()))                                                                    │       │
    │                                                         │                                                                                 │       │
    ├─────────────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class LinkedList {                                      │ Globals:                                                                        │       │
    │   mut v : int;                                          │                                                                                 │       │
    │   mut next : ?LinkedList;                               │ Classes:                                                                        │       │
    │                                                         │ LinkedList: ((id 0)                                                             │       │
    │   constructor(v : int) {                                │  (fields                                                                        │       │
    │     this.v = v;                                         │   ((next                                                                        │       │
    │     this.next = null;                                   │     ((ty (Option (Object 0))) (mut true) (overrides false)                      │       │
    │   }                                                     │      (visibility Private)))                                                     │       │
    │                                                         │    (v ((ty (Numeric Int)) (mut true) (overrides false) (visibility Private))))) │       │
    │   fun setNext(next : ?LinkedList) : unit {              │  (constructor ((args ((Numeric Int)))))                                         │       │
    │     this.next = next;                                   │  (methods                                                                       │       │
    │   }                                                     │   ((cons                                                                        │       │
    │                                                         │     ((visibility Public)                                                        │       │
    │   fun cons(v : int) : LinkedList {                      │      (function_ ((args ((Numeric Int))) (ret (Object 0)))) (overrides false)))  │       │
    │     let new_head = new LinkedList(v);                   │    (setNext                                                                     │       │
    │     new_head:setNext(this);                             │     ((visibility Public)                                                        │       │
    │     new_head                                            │      (function_ ((args ((Option (Object 0)))) (ret Unit))) (overrides false)))  │       │
    │   }                                                     │    (sum                                                                         │       │
    │                                                         │     ((visibility Public) (function_ ((args ()) (ret (Numeric Int))))            │       │
    │   fun sum() : int {                                     │      (overrides false)))))                                                      │       │
    │     this.v + (if? n = this.next { n:sum() } else { 0 }) │  (super ()))                                                                    │       │
    │   }                                                     │                                                                                 │       │
    │ }                                                       │                                                                                 │       │
    │                                                         │                                                                                 │       │
    └─────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────────────────┴───────┘
    |}]
;;

let%expect_test "type checking declaration errors" =
  let programs =
    [ {|
      class A { x : int; constructor() {this.x = 0; } } 
      class B { overrides x : int; constructor() {this.x = 10;} }
      |}
    ; {|
      class A { mut x : int; constructor() {this.x = 1;} } 
      class B < A { overrides x : int; constructor() {this.x = 2;} } 
      |}
    ; {|
      class A { x : int; constructor() {this.x=1;} } 
      class B < A { overrides x : bool; constructor() {this.x = true;} }
      |}
    ; {|
      class A { x : int; constructor() {this.x = 1;}} 
      class B < A { overrides y : int; constructor() {this.y = false;}}
      |}
    ; "fun f(x : int): int {x} fun g(): int { f(10, 10) }"
    ]
  in
  let test program =
    let program = Utils.remove_indentation program in
    let ast =
      program
      |> May.For_testing.parse_string
      |> Result.map_error ~f:May.Comp_error.to_string
    in
    let error =
      Result.bind ast ~f:(fun ast ->
        May.Check.check_decls (May.Check.empty ()) ~decls:ast
        |> Result.map_error ~f:May.Comp_error.to_string)
      |> Result.error
      |> Option.value_exn
    in
    [%sexp { program : string; error : string }]
  in
  programs |> List.map ~f:test |> Expectable.print;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────┬────────────────────────────────────────────────────────────────────────────┐
    │ program                                                            │ error                                                                      │
    ├────────────────────────────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────────────┤
    │ class A { x : int; constructor() {this.x = 0; } }                  │ Type error at 2:20-2:29: Field marked as override but B has no super class │
    │ class B { overrides x : int; constructor() {this.x = 10;} }        │                                                                            │
    │                                                                    │                                                                            │
    │ class A { mut x : int; constructor() {this.x = 1;} }               │ Type error at 2:24-2:33: Cannot override mutable field.                    │
    │ class B < A { overrides x : int; constructor() {this.x = 2;} }     │                                                                            │
    │                                                                    │                                                                            │
    │ class A { x : int; constructor() {this.x=1;} }                     │ Type error at 2:24-2:34: Type bool is not included in type int             │
    │ class B < A { overrides x : bool; constructor() {this.x = true;} } │                                                                            │
    │                                                                    │                                                                            │
    │ class A { x : int; constructor() {this.x = 1;}}                    │ Type error at 2:24-2:33: Override field y not found in any super class     │
    │ class B < A { overrides y : int; constructor() {this.y = false;}}  │                                                                            │
    │                                                                    │                                                                            │
    │ fun f(x : int): int {x} fun g(): int { f(10, 10) }                 │ Type error at 1:40-1:49: Expected 1 arguments but found 2                  │
    └────────────────────────────────────────────────────────────────────┴────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
