
type identifier = {
  name : string;
  lex_start : Lexing.position;
  lex_end : Lexing.position;
  mutable level : int;
  mutable offset : int;
  mutable narg : int;
  mutable defined : bool
}
    
and constant = Constant of int 
    
and program =
    ExtDecList of external_declaration list 
        
and external_declaration =
    ExtDec  of declaration
  | FuncDec of function_definition 
      
and declaration =
    VarDec of identifier list 
        
and function_definition =
    FuncDef of identifier * parameter_declaration list * compound_statement
        
and parameter_declaration =
    ParamDec of identifier
        
and statement =
    NilSt
  | Expr   of expression
  | CompSt of compound_statement
  | If     of expression * statement
  | IfElse of expression * statement * statement
  | While  of expression * statement
  | Return of expression
  | ReturnNothing
  | LocFuncDec of function_definition
  (* | LocFuncDec of identifier * parameter_declaration list * compound_statement *)

and compound_statement =
    DecSt of declaration list * statement list 
        
and expression =
    AsgnExpList of expression list
  | Asgn        of identifier * expression  (* 代入 *)
  | LogOr       of expression * expression   
  | LogAnd      of expression * expression  
  | Eq          of expression * expression      
  | Neq         of expression * expression     
  | Greater     of expression * expression 
  | Smaller     of expression * expression 
  | Greq        of expression * expression    
  | Smeq        of expression * expression    
  | Plus        of expression * expression    
  | Minus       of expression * expression
  | Mul         of expression * expression
  | Div         of expression * expression
  | Neg         of expression
  | FunCall     of identifier * expression list  (* 関数呼び出し *)
  | Var         of identifier                    (* 変数の参照 *)
  | Const       of constant
  | Paren       of expression 

let empty_identifier = {
  name = "";
  lex_start = {
    Lexing.pos_fname = ""; Lexing.pos_lnum = 0;
    Lexing.pos_bol = 0; Lexing.pos_cnum = 0;
  };
  lex_end = {
    Lexing.pos_fname = ""; Lexing.pos_lnum = 0;
    Lexing.pos_bol = 0; Lexing.pos_cnum = 0;
  };
  level = 0;
  offset = 0;
  narg = 0;
  defined = false;
}
