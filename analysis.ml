
open Syntax
open Management

let rec go_program frm = function ExtDecList lst ->
  List.iter
    (fun el -> match el with
        ExtDec (VarDec idlist) -> 
              List.iter (fun id -> declare_variable frm id) idlist
      | FuncDec func -> go_function_definition frm func) lst

and go_function_definition frm = function FuncDef (id, prmlst, cmpst) ->
  define_function frm id (List.length prmlst);
  let prm_frm = {
    paramsec = true;
    tbl = Hashtbl.create frame_size;
    lev = 1;
    var_locate = 0;
    parent = Some frm
  }
  in
  List.iter (function ParamDec id -> declare_parameter prm_frm id) prmlst;
  go_compound_statement
    {tbl = Hashtbl.create frame_size;
     lev = 2; paramsec = false;
     var_locate = -4;
     parent = Some prm_frm} 2 cmpst


and go_compound_statement frm lev = function DecSt (declst, stlst) ->
  List.iter (function VarDec idlist ->
    List.iter (fun id -> declare_variable frm id) idlist) declst;
  List.iter (fun st -> go_statement frm lev st) stlst;

and go_statement frm lev = function
  | Expr expr -> go_expression frm expr
  | CompSt cmpst ->
    go_compound_statement
      {tbl = Hashtbl.create frame_size;
       lev = lev + 1; paramsec = false;
       var_locate = frm.var_locate - (Hashtbl.length frm.tbl) * 4;
       parent = Some frm}
      (lev + 1) cmpst
  | If (expr, st) ->
    go_expression frm expr; go_statement frm lev st
  | IfElse (expr, st1, st2) ->
    go_expression frm expr;
    go_statement frm lev st1;
    go_statement frm lev st2
  | While (expr, st) ->
    go_expression frm expr; go_statement frm lev st
  | Return expr -> go_expression frm expr;
  | LocFuncDec func -> go_function_definition frm func
  | ReturnNothing | NilSt -> ()

and go_expression frm = function
  | AsgnExpList lst -> List.iter (fun expr -> go_expression frm expr) lst
  | Asgn (id, expr) -> refer_variable frm id; go_expression frm expr
  | LogOr (e1, e2) | LogAnd (e1, e2) | Eq (e1, e2) | Neq (e1, e2) |
      Greater (e1, e2) | Smaller (e1, e2) | Greq (e1, e2) | Smeq (e1, e2) |
          Plus (e1, e2) | Minus (e1, e2) | Mul (e1, e2) | Div (e1, e2) ->
    go_expression frm e1; go_expression frm e2
  | Neg expr -> go_expression frm expr
  | FunCall (id, exprlst) ->
    call_function frm id (List.length exprlst);
    List.iter (fun expr -> go_expression frm expr) exprlst
  | Var id -> refer_variable frm id
  | Paren expr -> go_expression frm expr
  | Const _ -> ()

let check tree =
  go_program (root ()) tree;
  if !semnerrs >= 1 then
    (Printf.fprintf stderr "error\n"; false)
  else true
