open Syntax
open Format

let pp_identifier ppf = function id ->
  if id.offset = -1 then
    fprintf ppf "%s:%d:%d" id.name id.level id.narg
  else
    fprintf ppf "%s:%d:%d" id.name id.level id.offset
let pp_constant ppf = function Constant c -> fprintf ppf "%d" c

let rec pp_some_list pp ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a@ %a" pp x (pp_some_list pp) xs

let rec pp_program ppf = function
  | ExtDecList lst -> pp_some_list pp_external_declaration ppf lst

and pp_external_declaration ppf = function
  | ExtDec (dec:declaration) -> fprintf ppf "@[<2>(%a)@]" pp_declaration dec
  | FuncDec (def:function_definition) -> fprintf ppf "@[<2>(DEFUN %a)@]" pp_funcdef def

and pp_declaration ppf = function
  | VarDec lst -> fprintf ppf "@[<2>(INT@ %a)@]" (pp_some_list pp_identifier) lst

and pp_funcdef ppf = function
  | FuncDef (id, prmlst, cmpst) ->
    fprintf ppf "(INT %a)@ (%a)@ (%a)"
      pp_identifier id (pp_some_list pp_parameter_declaration) prmlst pp_compound_statement cmpst

and pp_parameter_declaration ppf = function
  | ParamDec x -> fprintf ppf "(INT %a)" pp_identifier x

and pp_compound_statement ppf = function
  | DecSt (dec, st) ->
    fprintf ppf "@[(%a)@ %a@]"
      (pp_some_list pp_declaration) dec
      (pp_some_list pp_statement) st

and pp_statement ppf = function
  | NilSt -> ()
  | Expr expr -> fprintf ppf "@[(%a)@]" pp_expression expr
  | CompSt cmpst -> fprintf ppf "@[(%a)@]" pp_compound_statement cmpst
  | If (expr, st) -> fprintf ppf "@[<2>(IF@ %a@ %a)@]" pp_expression expr pp_statement st
  | IfElse (expr, st1, st2) ->
    fprintf ppf "@[<2>(IF@ %a@ %a@ %a)@]"
      pp_expression expr pp_statement st1 pp_statement st2
  | While (expr, st) -> fprintf ppf "@[<2>(WHILE@ %a@ %a)@]" pp_expression expr pp_statement st
  | Return expr -> fprintf ppf "@[(RETURN@ %a)@]" pp_expression expr
  | ReturnNothing -> fprintf ppf "(RETURN)"
  | LocFuncDec f -> pp_funcdef ppf f
    
and pp_expression ppf = function
  | AsgnExpList lst  -> pp_some_list pp_expression ppf lst
  | Asgn (id, expr)  -> fprintf ppf "@[(ASGN@ %a@ %a)@]" pp_identifier id pp_expression expr
  | LogOr (e1, e2)   -> fprintf ppf "@[(||@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | LogAnd (e1, e2)  -> fprintf ppf "@[(&&@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Eq (e1, e2)      -> fprintf ppf "@[(==@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Neq (e1, e2)     -> fprintf ppf "@[(!=@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Greater (e1, e2) -> fprintf ppf "@[(<@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Smaller (e1, e2) -> fprintf ppf "@[(>@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Greq (e1, e2)    -> fprintf ppf "@[(<=@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Smeq (e1, e2)    -> fprintf ppf "@[(>=@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Plus (e1, e2)    -> fprintf ppf "@[(+@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Minus (e1, e2)   -> fprintf ppf "@[(-@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Mul (e1, e2)     -> fprintf ppf "@[(*@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Div (e1, e2)     -> fprintf ppf "@[(/@ %a@ %a)@]" pp_expression e1 pp_expression e2
  | Neg e            -> fprintf ppf "@[(-@ %a)@]" pp_expression e
  | FunCall (id, lst) ->
    fprintf ppf "@[(%a@ %a)@]" pp_identifier id (pp_some_list pp_expression) lst
  | Var id     -> fprintf ppf "%a" pp_identifier id
  | Const c    -> fprintf ppf "%a" pp_constant c
  | Paren expr -> fprintf ppf "@[(%a)@]" pp_expression expr

let (pp_tree:(Syntax.program -> unit)) = pp_program std_formatter
