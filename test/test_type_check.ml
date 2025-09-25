open! Core

let run_expr_check program =
  let ast =
    program
    |> May.For_testing.expr_parse_string
    |> Result.map_error ~f:May.Comp_error.to_string
    |> Result.ok_or_failwith
  in
  let check =
    May.Check.For_testing.with_function_check
      (May.Check.empty ~mode:May.Mode.Without)
      (May.Check.For_testing.Function_check.create ~return_type:May.Type.Unit ())
  in
  May.Check.infer_expr check ~term:ast, check
;;

let%expect_test "type checking expressions/statements" =
  let programs =
    [ "-1 / 2 + 3 * 4 - 5 / (6 + 7)"
    ; "3 < 4 and 5 <= 6 or !(1 == 2) and 5 != 7"
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
    let result, check = run_expr_check program in
    let type_ =
      result
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
    │ 3 < 4 and 5 <= 6 or !(1 == 2) and 5 != 7                │ bool      │
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
    let result, _ = run_expr_check program in
    let error = result |> Result.error |> Option.value_exn |> May.Comp_error.to_string in
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

let run_program_check program check =
  May.Resolved_ident.Global.Id.For_testing.reset_counter ();
  May.Type.Class_id.For_testing.reset_counter ();
  let%bind.Result ast = May.For_testing.parse_string program in
  let%bind.Result decls =
    May.Check.check_decls
      check
      ~decls:ast
      ~load_file:(Utils.load_file ~mappings:[])
      ~starting_file:Utils.starting_file
  in
  Ok decls
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
    ; {|
      module A {
        const x = 10;

        fun getX() : int { x }
      }
      |}
    ]
  in
  let test program =
    let program = Utils.remove_indentation program in
    let check = May.Check.empty ~mode:May.Mode.Without in
    let result = run_program_check program check in
    let error = result |> Result.map_error ~f:May.Comp_error.to_string |> Result.error in
    let check_string = May.Check.For_testing.to_string check in
    [%sexp { program : string; check_string : string; error : string option }]
  in
  programs |> List.map ~f:test |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────────────────────────────┬───────┐
    │ program                                                 │ check_string                                                                  │ error │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ const x = 10;                                           │ Globals:                                                                      │       │
    │                                                         │ x: int                                                                        │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun double(x : int): int { x * 2 }                      │ Globals:                                                                      │       │
    │ fun quadruple(x : int): int {double(double(x))}         │ quadruple: (int) -> int                                                       │       │
    │                                                         │ double: (int) -> int                                                          │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ module Sub { const x = 1; } const y = 2;                │ Globals:                                                                      │       │
    │                                                         │ y: int                                                                        │       │
    │                                                         │ Sub.x: int                                                                    │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): unit { return; }                             │ Globals:                                                                      │       │
    │                                                         │ yay: () -> unit                                                               │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): int { return 5; 1 }                          │ Globals:                                                                      │       │
    │                                                         │ yay: () -> int                                                                │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ fun yay(): int { let x = { return 5; } ; 1 }            │ Globals:                                                                      │       │
    │                                                         │ yay: () -> int                                                                │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ module A {                                              │ Globals:                                                                      │       │
    │     const x = 10;                                       │ A.B.y: char                                                                   │       │
    │     module B {                                          │ x: () -> char                                                                 │       │
    │       const y = 'y';                                    │ A.x: int                                                                      │       │
    │     }                                                   │                                                                               │       │
    │ }                                                       │ Classes:                                                                      │       │
    │ fun x(): char {                                         │                                                                               │       │
    │     A.B.y                                               │                                                                               │       │
    │ }                                                       │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class A { constructor() {} }                            │ Globals:                                                                      │       │
    │                                                         │                                                                               │       │
    │                                                         │ Classes:                                                                      │       │
    │                                                         │ A: ((id 0) (fields ()) (constructor ((args ()))) (methods ()) (super ())      │       │
    │                                                         │  (implements ()))                                                             │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class Counter {                                         │ Globals:                                                                      │       │
    │   id : int;                                             │                                                                               │       │
    │   mut value : int;                                      │ Classes:                                                                      │       │
    │                                                         │ Counter: ((id 0)                                                              │       │
    │   constructor(id : int) {                               │  (fields                                                                      │       │
    │     this.id = id;                                       │   ((id                                                                        │       │
    │     this.value = 0;                                     │     ((ty (Numeric Int)) (mut false) (overrides false) (evolves false)         │       │
    │   }                                                     │      (visibility Private)))                                                   │       │
    │ }                                                       │    (value                                                                     │       │
    │                                                         │     ((ty (Numeric Int)) (mut true) (overrides false) (evolves false)          │       │
    │                                                         │      (visibility Private)))))                                                 │       │
    │                                                         │  (constructor ((args ((Numeric Int))))) (methods ()) (super ())               │       │
    │                                                         │  (implements ()))                                                             │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class Super { constructor () {}}                        │ Globals:                                                                      │       │
    │ class Sub < Super {                                     │                                                                               │       │
    │   constructor () { super() }                            │ Classes:                                                                      │       │
    │ }                                                       │ Sub: ((id 1) (fields ()) (constructor ((args ()))) (methods ()) (super (0))   │       │
    │ class A {                                               │  (implements ()))                                                             │       │
    │   id : int;                                             │ B: ((id 3)                                                                    │       │
    │   mut value : int;                                      │  (fields                                                                      │       │
    │   other : Super;                                        │   ((other                                                                     │       │
    │                                                         │     ((ty (Object (Shared (Class 1)))) (mut false) (overrides true)            │       │
    │   constructor() {                                       │      (evolves false) (visibility Private)))                                   │       │
    │     this.id = 10;                                       │    (y                                                                         │       │
    │     this.value = 0;                                     │     ((ty (Numeric Int)) (mut false) (overrides false) (evolves false)         │       │
    │     this.other = new Super();                           │      (visibility Private)))))                                                 │       │
    │   }                                                     │  (constructor ((args ())))                                                    │       │
    │   fun something(x : int) : []int { [x] }                │  (methods                                                                     │       │
    │ }                                                       │   ((something                                                                 │       │
    │                                                         │     ((visibility Public)                                                      │       │
    │ class B < A {                                           │      (function_                                                               │       │
    │   y : int;                                              │       ((args ((Numeric Int))) (ret (Array (mut true) (elt (Numeric Int))))))  │       │
    │   overrides other : Sub;                                │      (overrides true) (receiver_evolves false)))))                            │       │
    │                                                         │  (super (2)) (implements ()))                                                 │       │
    │   constructor () {                                      │ A: ((id 2)                                                                    │       │
    │     super();                                            │  (fields                                                                      │       │
    │     this.y = -1;                                        │   ((id                                                                        │       │
    │     this.other = new Sub();                             │     ((ty (Numeric Int)) (mut false) (overrides false) (evolves false)         │       │
    │   }                                                     │      (visibility Private)))                                                   │       │
    │                                                         │    (other                                                                     │       │
    │   overrides                                             │     ((ty (Object (Shared (Class 0)))) (mut false) (overrides false)           │       │
    │   fun something(x : int) : []mut int {                  │      (evolves false) (visibility Private)))                                   │       │
    │     [x, this.y]                                         │    (value                                                                     │       │
    │   }                                                     │     ((ty (Numeric Int)) (mut true) (overrides false) (evolves false)          │       │
    │ }                                                       │      (visibility Private)))))                                                 │       │
    │                                                         │  (constructor ((args ())))                                                    │       │
    │                                                         │  (methods                                                                     │       │
    │                                                         │   ((something                                                                 │       │
    │                                                         │     ((visibility Public)                                                      │       │
    │                                                         │      (function_                                                               │       │
    │                                                         │       ((args ((Numeric Int))) (ret (Array (mut false) (elt (Numeric Int)))))) │       │
    │                                                         │      (overrides false) (receiver_evolves false)))))                           │       │
    │                                                         │  (super ()) (implements ()))                                                  │       │
    │                                                         │ Super: ((id 0) (fields ()) (constructor ((args ()))) (methods ()) (super ())  │       │
    │                                                         │  (implements ()))                                                             │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class A {                                               │ Globals:                                                                      │       │
    │   constructor () {}                                     │ f: (A) -> []int                                                               │       │
    │   fun something(x : int) : []int { [x] }                │                                                                               │       │
    │ }                                                       │ Classes:                                                                      │       │
    │                                                         │ A: ((id 0) (fields ()) (constructor ((args ())))                              │       │
    │ fun f(v : A): []int {                                   │  (methods                                                                     │       │
    │   v:something(10)                                       │   ((something                                                                 │       │
    │ }                                                       │     ((visibility Public)                                                      │       │
    │                                                         │      (function_                                                               │       │
    │                                                         │       ((args ((Numeric Int))) (ret (Array (mut false) (elt (Numeric Int)))))) │       │
    │                                                         │      (overrides false) (receiver_evolves false)))))                           │       │
    │                                                         │  (super ()) (implements ()))                                                  │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ class LinkedList {                                      │ Globals:                                                                      │       │
    │   mut v : int;                                          │                                                                               │       │
    │   mut next : ?LinkedList;                               │ Classes:                                                                      │       │
    │                                                         │ LinkedList: ((id 0)                                                           │       │
    │   constructor(v : int) {                                │  (fields                                                                      │       │
    │     this.v = v;                                         │   ((next                                                                      │       │
    │     this.next = null;                                   │     ((ty (Option (Object (Shared (Class 0))))) (mut true) (overrides false)   │       │
    │   }                                                     │      (evolves false) (visibility Private)))                                   │       │
    │                                                         │    (v                                                                         │       │
    │   fun setNext(next : ?LinkedList) : unit {              │     ((ty (Numeric Int)) (mut true) (overrides false) (evolves false)          │       │
    │     this.next = next;                                   │      (visibility Private)))))                                                 │       │
    │   }                                                     │  (constructor ((args ((Numeric Int)))))                                       │       │
    │                                                         │  (methods                                                                     │       │
    │   fun cons(v : int) : LinkedList {                      │   ((cons                                                                      │       │
    │     let new_head = new LinkedList(v);                   │     ((visibility Public)                                                      │       │
    │     new_head:setNext(this);                             │      (function_ ((args ((Numeric Int))) (ret (Object (Shared (Class 0))))))   │       │
    │     new_head                                            │      (overrides false) (receiver_evolves false)))                             │       │
    │   }                                                     │    (setNext                                                                   │       │
    │                                                         │     ((visibility Public)                                                      │       │
    │   fun sum() : int {                                     │      (function_ ((args ((Option (Object (Shared (Class 0)))))) (ret Unit)))   │       │
    │     this.v + (if? n = this.next { n:sum() } else { 0 }) │      (overrides false) (receiver_evolves false)))                             │       │
    │   }                                                     │    (sum                                                                       │       │
    │ }                                                       │     ((visibility Public) (function_ ((args ()) (ret (Numeric Int))))          │       │
    │                                                         │      (overrides false) (receiver_evolves false)))))                           │       │
    │                                                         │  (super ()) (implements ()))                                                  │       │
    │                                                         │                                                                               │       │
    ├─────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────────┼───────┤
    │ module A {                                              │ Globals:                                                                      │       │
    │   const x = 10;                                         │ A.getX: () -> int                                                             │       │
    │                                                         │ A.x: int                                                                      │       │
    │   fun getX() : int { x }                                │                                                                               │       │
    │ }                                                       │ Classes:                                                                      │       │
    │                                                         │                                                                               │       │
    └─────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────────────────────────────┴───────┘
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
    let check = May.Check.empty ~mode:Without in
    let result = run_program_check program check in
    let error = result |> Result.error |> Option.value_exn |> May.Comp_error.to_string in
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

let%expect_test "test ownership types" =
  let programs =
    [ {|
    class A { constructor() {} }

    fun main() : unit { let a: !A = new A(); () }
    |}
    ; {|
    class A { constructor() {} }
    fun main() : unit { 
      let a: !A = new A(); 
      let b: !A = a;
      let c: !A = a;
      () 
      }
    |}
    ; {|
    class A { constructor() {} }
    fun main() : unit { 
      let a: !A = new A(); 
      let b: A = a;
      let c: A = a;
      () 
      }
    |}
    ; {|
    class A { constructor() {} }
    class B < A { constructor() evolves A {} }
    fun evolveA(a: !A) : !B { a evolves B() }
    fun useA(a: A): unit {}
    fun main(): unit {
      let a = new A();
      let b : !B = evolveA(a);
      useA(a);
      ()
    }
    |}
    ; {|
    class A { constructor() {} }
    class B < A { constructor() evolves A {} }
    fun evolveA(a: !A) : !B { a evolves B() }
    fun main(): unit {
      let a = new A();
      let b = evolveA(a);
      let b2 = evolveA(a);
      ()
    }
    |}
    ; {|
    class A { constructor() {} }
    class B < A { constructor() evolves A {} }
    class C < B { constructor() evolves B {} }
    fun evolveA(a: !A) : !C { a evolves B() evolves C() }
    fun main(): unit {
      let a = new A();
      let c = evolveA(a);
      ()
    }
    |}
    ; {|
    class A { public id: int; constructor(id: int) { this.id = id; } }
    fun main(): unit {
      let a_array: []mut !A = [ new A(1) ];
      let a: !A = new A(2);
      a >=< a_array[0];
      ()
    }
    |}
    ; {|
    class A { public id: int; constructor(id: int) { this.id = id; } }
    class B < A { constructor() evolves A {} }
    fun main(): unit {
      let a_array: []mut !A = [ new A(1) ];
      let b = a_array[0] evolves B();
      ()
    }
    |}
    ; {|
    class A { public id: int; constructor(id: int) { this.id = id; } }
    fun main(): unit {
      let a_array: []mut !A = [ new A(1) ];
      let a: A = new A(2);
      a >=< a_array[0];
      ()
    }
    |}
    ; {|
    class A { constructor() {} }
    class B < A { constructor() evolves A {} }
    fun evolveA(a: !A) : !B { a evolves B() }
    fun main(): unit {
      let a = new A();
      if true {
        let b = evolveA(a); ()
      } else {
        let b2 = evolveA(a); ()
      }
      ()
    }
    |}
    ; {|
    class A { constructor() {} }
    class B < A { constructor() evolves A {} }
    fun evolveA(a: !A) : !B { a evolves B() }
    fun main(): unit {
      let a = new A();
      if true {
        let b = evolveA(a); ()
      }
      ()
    }
    |}
    ; {|
    class A { 
      constructor () {} 
      evolves fun makeB() : !B { this evolves B() }
    }
    class B < A { constructor() evolves A {} }
    |}
    ; {|
    class A { 
      constructor () {} 
      evolves fun makeB() : !B { this evolves B() }
    }
    class B < A { constructor() evolves A {} }
    fun main() : unit {
      let a = new A();
      let b = a:makeB();
      let b2 = a evolves B();
      ()
    }
    |}
    ; {|
    class A { 
      constructor () {} 
      evolves fun makeB() : !B { this evolves B() }
    }
    class B < A { constructor() evolves A {} }
    fun main() : unit {
      let a = new A();
      let b = a:makeB();
      let c = b:makeB();
      ()
    }
    |}
    ; {|
    class A { 
      constructor () {} 
      evolves fun makeB() : !B { this evolves B() }
    }
    class B < A { constructor() evolves A {} }
    fun main() : unit {
      let a = new A();
      let b = a:makeB();
      ()
    }
    |}
    ]
  in
  let test program =
    let program = Utils.remove_indentation program in
    let check = May.Check.empty ~mode:May.Mode.With_ownership in
    let result = run_program_check program check in
    let error = result |> Result.map_error ~f:May.Comp_error.to_string |> Result.error in
    [%sexp { program : string; error : string option }]
  in
  programs |> List.map ~f:test |> Expectable.print ~separate_rows:true;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────────────────────────┐
    │ program                                                            │ error                                                                                    │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │                                                                                          │
    │                                                                    │                                                                                          │
    │ fun main() : unit { let a: !A = new A(); () }                      │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │ Type error at 5:15-5:16: Type A is not included in type !A                               │
    │ fun main() : unit {                                                │                                                                                          │
    │   let a: !A = new A();                                             │                                                                                          │
    │   let b: !A = a;                                                   │                                                                                          │
    │   let c: !A = a;                                                   │                                                                                          │
    │   ()                                                               │                                                                                          │
    │   }                                                                │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │                                                                                          │
    │ fun main() : unit {                                                │                                                                                          │
    │   let a: !A = new A();                                             │                                                                                          │
    │   let b: A = a;                                                    │                                                                                          │
    │   let c: A = a;                                                    │                                                                                          │
    │   ()                                                               │                                                                                          │
    │   }                                                                │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun evolveA(a: !A) : !B { a evolves B() }                          │                                                                                          │
    │ fun useA(a: A): unit {}                                            │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let b : !B = evolveA(a);                                         │                                                                                          │
    │   useA(a);                                                         │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │ Type error at 7:20-7:21: Type A is not included in type !A                               │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun evolveA(a: !A) : !B { a evolves B() }                          │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let b = evolveA(a);                                              │                                                                                          │
    │   let b2 = evolveA(a);                                             │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ class C < B { constructor() evolves B {} }                         │                                                                                          │
    │ fun evolveA(a: !A) : !C { a evolves B() evolves C() }              │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let c = evolveA(a);                                              │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { public id: int; constructor(id: int) { this.id = id; } } │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a_array: []mut !A = [ new A(1) ];                            │                                                                                          │
    │   let a: !A = new A(2);                                            │                                                                                          │
    │   a >=< a_array[0];                                                │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { public id: int; constructor(id: int) { this.id = id; } } │ Type error at 5:11-5:21: Type A is not included in type !A                               │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a_array: []mut !A = [ new A(1) ];                            │                                                                                          │
    │   let b = a_array[0] evolves B();                                  │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { public id: int; constructor(id: int) { this.id = id; } } │ Type error at 5:3-5:20: Cannot exchange types A and !A                                   │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a_array: []mut !A = [ new A(1) ];                            │                                                                                          │
    │   let a: A = new A(2);                                             │                                                                                          │
    │   a >=< a_array[0];                                                │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun evolveA(a: !A) : !B { a evolves B() }                          │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   if true {                                                        │                                                                                          │
    │     let b = evolveA(a); ()                                         │                                                                                          │
    │   } else {                                                         │                                                                                          │
    │     let b2 = evolveA(a); ()                                        │                                                                                          │
    │   }                                                                │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A { constructor() {} }                                       │ Type error at 6:3-8:4: Local variable a ends with type A on one branch, but type !A on   │
    │ class B < A { constructor() evolves A {} }                         │ another                                                                                  │
    │ fun evolveA(a: !A) : !B { a evolves B() }                          │                                                                                          │
    │ fun main(): unit {                                                 │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   if true {                                                        │                                                                                          │
    │     let b = evolveA(a); ()                                         │                                                                                          │
    │   }                                                                │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A {                                                          │                                                                                          │
    │   constructor () {}                                                │                                                                                          │
    │   evolves fun makeB() : !B { this evolves B() }                    │                                                                                          │
    │ }                                                                  │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A {                                                          │ Type error at 9:12-9:13: Type A is not included in type !A                               │
    │   constructor () {}                                                │                                                                                          │
    │   evolves fun makeB() : !B { this evolves B() }                    │                                                                                          │
    │ }                                                                  │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun main() : unit {                                                │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let b = a:makeB();                                               │                                                                                          │
    │   let b2 = a evolves B();                                          │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A {                                                          │ Type error at 9:11-9:20: Method makeB not present on type B                              │
    │   constructor () {}                                                │                                                                                          │
    │   evolves fun makeB() : !B { this evolves B() }                    │                                                                                          │
    │ }                                                                  │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun main() : unit {                                                │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let b = a:makeB();                                               │                                                                                          │
    │   let c = b:makeB();                                               │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    ├────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────────────────────────┤
    │ class A {                                                          │                                                                                          │
    │   constructor () {}                                                │                                                                                          │
    │   evolves fun makeB() : !B { this evolves B() }                    │                                                                                          │
    │ }                                                                  │                                                                                          │
    │ class B < A { constructor() evolves A {} }                         │                                                                                          │
    │ fun main() : unit {                                                │                                                                                          │
    │   let a = new A();                                                 │                                                                                          │
    │   let b = a:makeB();                                               │                                                                                          │
    │   ()                                                               │                                                                                          │
    │ }                                                                  │                                                                                          │
    │                                                                    │                                                                                          │
    └────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
