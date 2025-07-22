
%{
open! Core
     (* TODO: a < b <= c < d syntax with single evaluation of a, b, c, d *)
%}

%token <Ast.Ident.t> IDENT
%token <Ast.Ident.t> CAP_IDENT
%token <string> STRING
%token <string> INT
%token <char> CHAR_LIT
%token <string> CHAR_INT

%token CLASS
%token CONST
%token ELSE
%token FALSE
%token FUN
%token IF
%token IFO
%token LET
%token MODULE
%token RETURN
%token TRUE
%token WHILE
%token MUT
%token OVERRIDES
%token PUBLIC
%token PRIVATE
%token CONSTRUCTOR
%token EVOLVES
%token THIS
%token SUPER
%token NEW
%token ORELSE
%token NULL
%token EXTERN

%token PIPE_PIPE
%token AMP_AMP
%token BANG
%token COMMA
%token COLON
// %token COLON_EQ
%token DOT
%token DOT_DOT
%token GE
%token GT
%token LE
%token LT
%token EQ
%token NEQ
%token EQQ
%token MINUS
%token PLUS
%token SEMI
%token SLASH
%token STAR
%token DOT_INTER
%token INTER
%token XCHG

%token O_CURLY
%token C_CURLY
%token O_PAREN
%token C_PAREN
%token O_SQUARE
%token C_SQUARE

%token EOF

// (* Lowest prec *)
%left ORELSE
%left PIPE_PIPE
%left AMP_AMP 
%left GT GE LT LE EQQ NEQ
%left PLUS MINUS
%left STAR SLASH
%nonassoc UNARY_MINUS
// (* Highest prec *)

%start <Ast.Expr.t> expr_prog
%start <Ast.Decl.t list> decl_prog
%%

expr_prog: e = expr EOF { e }
decl_prog: decls = list(decl) EOF { decls }

ty: ident = IDENT { Ast.Type.create ~ty:(Ident ident) ~loc:$loc }
  | path = cap_ident_path { Ast.Type.create ~ty:(Path path) ~loc:$loc }
  | O_SQUARE C_SQUARE mut = boption(MUT) ty = ty 
    { Ast.Type.create 
        ~ty:(Array {mut; elt = ty}) 
        ~loc:$loc }
  | INTER ty = ty {Ast.Type.create ~ty:(Option ty) ~loc:$loc}
  | BANG path = cap_ident_path { Ast.Type.create ~ty:(Owned path) ~loc:$loc }

expr :
  | non_stmt_expr { $1 }
  | if_then_else { $1 }
  | block { $1 }

non_stmt_expr :
  | subscriptable { $1 }
  | lhs = non_stmt_expr ORELSE    rhs = non_stmt_expr { Ast.Expr.create ~expr:(Or_else {lhs ; or_else = rhs}) ~loc:$loc}
  | lhs = non_stmt_expr AMP_AMP   rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = And}) ~loc:$loc}
  | lhs = non_stmt_expr PIPE_PIPE rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Or})  ~loc:$loc}
  | lhs = non_stmt_expr GT        rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Gt})  ~loc:$loc}
  | lhs = non_stmt_expr GE        rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Ge})  ~loc:$loc}
  | lhs = non_stmt_expr LT        rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Lt})  ~loc:$loc}
  | lhs = non_stmt_expr LE        rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Le})  ~loc:$loc}
  | lhs = non_stmt_expr EQQ       rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Eq})  ~loc:$loc}
  | lhs = non_stmt_expr NEQ       rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Ne})  ~loc:$loc}
  | lhs = non_stmt_expr PLUS      rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Add}) ~loc:$loc}
  | lhs = non_stmt_expr STAR      rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Mul}) ~loc:$loc}
  | lhs = non_stmt_expr MINUS     rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Sub}) ~loc:$loc}
  | lhs = non_stmt_expr SLASH     rhs = non_stmt_expr { Ast.Expr.create ~expr:(Bin_op {lhs ; rhs ; op = Div}) ~loc:$loc}
  | MINUS rhs = non_stmt_expr %prec UNARY_MINUS       { Ast.Expr.create ~expr:(Un_op  {rhs ; op = Neg}) ~loc:$loc}
  | BANG  rhs = non_stmt_expr %prec UNARY_MINUS       { Ast.Expr.create ~expr:(Un_op  {rhs ; op = Not}) ~loc:$loc}

subscriptable:
  | i = INT  
    { Ast.Expr.create ~expr:(Lit_int i) ~loc:$loc}
  | s = STRING 
    { Ast.Expr.create ~expr:(Lit_string s) ~loc:$loc}
  | c = CHAR_LIT
    { Ast.Expr.create ~expr:(Lit_char c) ~loc:$loc}
  | c = CHAR_INT
    { Ast.Expr.create ~expr:(Lit_char (Char.of_int_exn (Int.of_string c))) ~loc:$loc}
  | TRUE 
    { Ast.Expr.create ~expr:(Lit_bool true) ~loc:$loc}
  | FALSE 
    { Ast.Expr.create ~expr:(Lit_bool false) ~loc:$loc}
  | THIS
    { Ast.Expr.create ~expr:This ~loc:$loc}
  | SUPER
    { Ast.Expr.create ~expr:Super ~loc:$loc}
  | NULL
    { Ast.Expr.create ~expr:Null ~loc:$loc}
  | v = ident_path
    { Ast.Expr.create ~expr:(Ident v) ~loc:$loc}
  | O_PAREN C_PAREN 
    { Ast.Expr.create ~expr:Unit ~loc:$loc}
  | O_PAREN e = expr C_PAREN 
    { e }
  | expr = subscriptable DOT field = IDENT 
    { Ast.Expr.create ~expr:(Field_subscript {expr; field}) ~loc:$loc }
  | expr = subscriptable O_SQUARE index = expr C_SQUARE 
    { Ast.Expr.create ~expr:(Array_subscript {expr; index}) ~loc:$loc }
  | expr = subscriptable O_SQUARE from = expr DOT_DOT to_ = expr C_SQUARE 
    { Ast.Expr.create ~expr:(Array_subrange {expr; from; to_}) ~loc:$loc }
  | expr = subscriptable O_PAREN arguments = separated_list(COMMA, expr)  C_PAREN 
    { Ast.Expr.create ~expr:(Function_call {expr; arguments}) ~loc:$loc }
  | expr = subscriptable COLON method_ = IDENT O_PAREN arguments = separated_list(COMMA, expr)  C_PAREN 
    { Ast.Expr.create ~expr:(Method_call {expr; method_; arguments}) ~loc:$loc }
  | O_SQUARE elts = separated_list(COMMA, expr) C_SQUARE 
    { Ast.Expr.create ~expr:(Lit_array elts) ~loc:$loc}
  | NEW class_ = cap_ident_path O_PAREN arguments = separated_list(COMMA, expr)  C_PAREN
    { Ast.Expr.create ~expr:(New {class_; arguments}) ~loc:$loc}
  | NEW O_SQUARE size = expr C_SQUARE ty = ty O_PAREN init = expr C_PAREN
    { Ast.Expr.create ~expr:(New_array { size; ty; init}) ~loc:$loc}
  | expr = subscriptable EVOLVES class_ = cap_ident_path O_PAREN arguments = separated_list(COMMA, expr) C_PAREN
    { Ast.Expr.create ~expr:(Evolves {expr; class_; arguments}) ~loc:$loc}
  | lhs = subscriptable DOT_INTER 
    {Ast.Expr.create ~expr:(De_null { lhs }) ~loc:$loc}

block: 
  | O_CURLY stmts = stmt_list C_CURLY 
    { Ast.Expr.create ~expr:(Block (stmts)) ~loc:$loc }

non_expr_stmt: 
  | LET ident = IDENT annot = option(type_annotation) EQ expr = expr SEMI
    { Ast.Expr.create ~expr:(Let {ident; expr; annot}) ~loc:$loc }
  | lhs = expr EQ rhs = expr SEMI 
    { Ast.Expr.create ~expr:(Assign {lhs; rhs}) ~loc:$loc }
  | WHILE cond = expr block = block 
    { Ast.Expr.create ~expr:(While {cond; block}) ~loc:$loc }
  | RETURN expr = expr SEMI 
    { Ast.Expr.create ~expr:(Return expr) ~loc:$loc }
  | RETURN SEMI 
    { Ast.Expr.create 
        ~expr:(Return (Ast.Expr.create ~expr:Unit ~loc:$loc)) 
        ~loc:$loc }
  | if_then { $1 }
  | expr1 = expr XCHG expr2 = expr SEMI
    { Ast.Expr.create ~expr:(Exchange {expr1; expr2}) ~loc:$loc }

stmt_list:
  | e = non_stmt_expr SEMI tl = stmt_list { e :: tl }
  | e = non_stmt_expr { [e] }
  | e = non_expr_stmt tl = stmt_list { e :: tl }
  | e = block tl = stmt_list { e :: tl } 
  | e = if_then_else tl = stmt_list { e :: tl }
  | { [] }


if_then_else : 
  | IF cond = expr if_then = block ELSE if_else = if_then_else_if_else
    { Ast.Expr.create 
      ~expr:(If {cond; if_then; if_else = if_else}) 
      ~loc:$loc }
  | IFO 
    var = IDENT 
    annot = option(type_annotation) 
    EQ 
    expr = expr 
    if_value = block 
    ELSE 
    if_null = if_then_else_if_else
    { Ast.Expr.create 
      ~expr:(If_option {expr; var; if_value; if_null; annot }) 
      ~loc:$loc}

if_then :
  | IF cond = expr if_then = block if_else = if_then_if_else
    { Ast.Expr.create 
      ~expr:(If {cond; if_then; if_else}) 
      ~loc:$loc}
  | IFO 
    var = IDENT 
    annot = option(type_annotation) 
    EQ 
    expr = expr 
    if_value = block 
    if_null = if_then_if_else
    { Ast.Expr.create 
      ~expr:(If_option {expr; var; if_value; if_null; annot }) 
      ~loc:$loc}

if_then_if_else: 
  | { None } 
  | ELSE if_then = if_then {Some if_then}

if_then_else_if_else: 
  | block { Some $1 } 
  | if_then_else { Some $1 }

func:
  | FUN 
    name = IDENT 
    O_PAREN 
    args = list_of_typed_idents 
    C_PAREN COLON 
    ret_type = ty 
    body = block 
      { Ast.Decl.Function.create ~name ~args ~body ~ret_type }

extern_func:
  | EXTERN FUN 
    name = IDENT 
    O_PAREN 
    arg_tys = separated_list(COMMA, ty) 
    C_PAREN COLON 
    ret_type = ty 
    EQ 
    external_name = STRING SEMI
    { Ast.Decl.create 
      ~loc:$loc 
      ~decl:(Extern_function {name; arg_tys; ret_type; external_name }) }

decl: 
  | func { Ast.Decl.create ~decl:(Function $1) ~loc:$loc }
  | extern_func { $1 }
  | CLASS 
    name = CAP_IDENT 
    super_type = option(preceded(LT, cap_ident_path))
    O_CURLY 
    class_elts = list(class_elt)
    C_CURLY 
      { let constructors, fields, methods = 
          List.partition3_map class_elts ~f:Fn.id 
        in
        Ast.Decl.create 
        ~decl:(Class {name; super_type; fields; constructors; methods }) 
        ~loc:$loc}
  | CONST
    ident = IDENT
    annot = option(type_annotation)
    EQ
    value = expr
    SEMI
      { Ast.Decl.create 
        ~decl:(Constant {ident; value; annot}) 
        ~loc:$loc }
  | MODULE
    name = CAP_IDENT
    O_CURLY
    decls = list(decl)
    C_CURLY
    { Ast.Decl.create 
      ~decl:(Module {name; decls}) 
      ~loc:$loc }

class_elt:
  | class_constructor { $1 }
  | visibility = option(visibility)
    overrides = boption(OVERRIDES)
    field_or_method = endrule(class_field { $1 } | class_method { $1 })
    { field_or_method ?visibility ~overrides () }
      
type_annotation: COLON ty = ty {ty}

list_of_typed_idents: l = separated_list(COMMA, typed_ident) {l}

typed_ident: ident = IDENT COLON ty = ty {ident, ty}

class_constructor:
  | CONSTRUCTOR
    O_PAREN
    args = list_of_typed_idents
    C_PAREN
    evolves = option(preceded(EVOLVES, cap_ident_path))
    body = block
    { `Fst (Ast.Node.create 
              ~loc:$loc 
              ~data:Ast.Decl.Class.Constructor.{args; evolves; body} ) }

class_field: 
  | mut = boption(MUT) 
    name = IDENT
    COLON
    ty = ty
    SEMI
    { fun ?(visibility = Ast.Decl.Visibility.Private) ~overrides () ->
        `Snd (Ast.Node.create 
               ~data:Ast.Decl.Class.Field.{visibility; overrides; mut; name; ty} 
               ~loc:$loc) }

class_method:
  | function_ = func
    { fun ?(visibility = Ast.Decl.Visibility.Public) ~overrides () ->
        `Trd (Ast.Node.create 
                ~data:Ast.Decl.Class.Method.{ visibility; function_; overrides } 
                ~loc:$loc) }

visibility: 
  | PUBLIC { Ast.Decl.Visibility.Public }
  | PRIVATE { Ast.Decl.Visibility.Private }

ident_path: ident_path_rev 
  { Ast.Path.of_list (Nonempty_list.of_list_exn $1) }

ident_path_rev:
  | cap_ident = CAP_IDENT DOT remaining_path = ident_path_rev 
    { cap_ident :: remaining_path }
  | ident = IDENT 
    { [ ident ] }

cap_ident_path: cap_ident_path_rev 
  { Ast.Path.of_list (Nonempty_list.of_list_exn $1) }

cap_ident_path_rev:
  | cap_ident = CAP_IDENT DOT remaining_path = cap_ident_path_rev 
    { cap_ident :: remaining_path }
  | ident = CAP_IDENT 
    { [ ident ] }