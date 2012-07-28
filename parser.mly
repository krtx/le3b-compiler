%{
  open Printf
  open Syntax
%}

%token <string * Lexing.position * Lexing.position> IDENTIFIER
%token <int> CONSTANT
%token COMMA SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE
%token EQ
%token EQUAL NOTEQUAL

%token PLUS MINUS
%token MULTIPLY DIVIDE

%token LOR LAND
%token GREATER SMALLER
%token GREATEREQ SMALLEREQ
%token INT IF ELSE WHILE RETURN
%token LOCAL

%token EOF

%start program
%type <Syntax.program> program

%%

  program:
    external_declaration_list EOF { ExtDecList($1) }
  ;

  external_declaration_list:
    external_declaration { [$1] }
  | external_declaration_list external_declaration { $1 @ [$2] }
  ;

  external_declaration:
    declaration         { ExtDec($1) }
  | function_definition { FuncDec($1) }
  ;

  declaration:
    INT declarator_list SEMICOLON { VarDec($2) }
  ;

  declarator_list:
    declarator { [$1] }
  | declarator_list COMMA declarator { $1 @ [$3] }
  ;
  
  declarator:
    IDENTIFIER {
      match $1 with (x, y, z) ->
        {name = x; lex_start = y; lex_end = z;
         level = -1; offset = -1;narg = -1;
         defined = false}
    }
  ;
  
  function_definition:
    INT declarator LPAREN parameter_type_list RPAREN compound_statement
    {
      FuncDef($2, $4, $6)
    }
  ;

  parameter_type_list:
    parameter_type_list COMMA parameter_declaration { $1 @ [$3] }
  | parameter_declaration { [$1] }
  | { [] }
  ;

  parameter_declaration:
    INT declarator { ParamDec($2) }
  ;

  statement:
    SEMICOLON                                            { NilSt }
  | expression SEMICOLON                                 { Expr($1) }
  | compound_statement                                   { CompSt($1) }
  | IF LPAREN expression RPAREN statement                { If($3, $5) }
  | IF LPAREN expression RPAREN statement ELSE statement { IfElse($3, $5, $7) }
  | WHILE LPAREN expression RPAREN statement             { While($3, $5) }
  | RETURN expression SEMICOLON                          { Return($2) }
  | RETURN SEMICOLON                                     { ReturnNothing }
  | LOCAL function_definition                            { LocFuncDec($2) }
  ;

  compound_statement:
    LBRACE declaration_list statement_list RBRACE
    {
      DecSt($2, $3)
    }
  ;

  declaration_list:
    declaration_list declaration { $1 @ [$2] }
  | { [] }
  ;

  statement_list:
    statement_list statement { $1 @ [$2] }
  | { [] }
  ;

  expression:
    assign_expr_list { AsgnExpList($1) }
  ;

  assign_expr_list:
    assign_expr_list COMMA assign_expr { $1 @ [$3] }
  | assign_expr { [$1] }
  ;

  assign_expr:
    logical_OR_expr { $1 }
  | IDENTIFIER EQ assign_expr {
      match $1 with (x, y, z) ->
        Asgn({
          name = x; lex_start = y; lex_end = z;
          level = -1; offset = -1; narg = -1;
          defined = false
        }, $3)
    }
  ;

  logical_OR_expr:
    logical_AND_expr { $1 }
  | logical_OR_expr LOR logical_AND_expr { LogOr($1, $3) }
  ;

  logical_AND_expr:
    equality_expr { $1 }
  | logical_AND_expr LAND equality_expr { LogAnd($1, $3) }
  ;

  equality_expr:
    relational_expr { $1 }
  | equality_expr EQUAL relational_expr { Eq($1, $3) }
  | equality_expr NOTEQUAL relational_expr { Neq($1, $3) }
  ;

  relational_expr:
    add_expr { $1 }
  | relational_expr GREATER add_expr { Greater($1, $3) }
  | relational_expr SMALLER add_expr { Smaller($1, $3) }
  | relational_expr GREATEREQ add_expr { Greq($1, $3) }
  | relational_expr SMALLEREQ add_expr { Smeq($1, $3) }
  ;

  add_expr:
    mult_expr { $1 }
  | add_expr PLUS mult_expr { Plus($1, $3) }
  | add_expr MINUS mult_expr { Minus($1, $3) }
  ;

  mult_expr:
    unary_expr { $1 }
  | mult_expr MULTIPLY unary_expr { Mul($1, $3) }
  | mult_expr DIVIDE unary_expr { Div($1, $3) }
  ;

  unary_expr:
    postfix_expr { $1 }
  | MINUS unary_expr { Neg($2) }
  ;

  postfix_expr:
    primary_expr { $1 }
  | IDENTIFIER LPAREN argument_expression_list RPAREN {
      match $1 with (x, y, z) ->
        FunCall({
          name = x; lex_start = y; lex_end = z;
          level = -1; offset = -1; narg = -1;
          defined = false
        }, $3)
    }
  ;

  primary_expr:
    IDENTIFIER {
      match $1 with (x, y, z) -> 
        Var({
          name = x; lex_start = y; lex_end = z;
          level = -1; offset = -1; narg = -1;
          defined = false
        })
    }
  | CONSTANT { Const(Constant($1)) }
  | LPAREN expression RPAREN { $2 }
  ;

  argument_expression_list:
    assign_expr_list { $1 }
  | {[]}
  ;

