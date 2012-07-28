
open Printf
open Syntax
open Management

type code = {
  label : string option;
  inst : string;
  op1 : string option;
  op2 : string option;
  comment : string option;
}

let empty_code = {label = None; inst = ""; op1 = None; op2 = None; comment = None}

let gen_label num =
  (fun name ->
    let l = sprintf "L%d%s" !num name
    in incr num; l)

let print_code lst =
  List.iter (fun code ->
    (match code.label with
        Some s -> printf "%s:" s | None -> ());
    printf "\t";
    printf "%s" code.inst;
    (match code.op1 with
        Some s -> printf "\t%s" s | None -> ());
    (match code.op2 with
        Some s -> printf ", %s" s | None -> ());
    (match code.comment with
        Some s -> printf "\t;%s" s | None -> ());
    printf "\n") lst

(* 変数の位置を返す。グローバル変数とローカル変数で違う *)
let loc frm id =
  let s = search frm id in
  match Hashtbl.find s.tbl id.name with
      VarInfo vi ->
        if s.lev = 0 then sprintf "[%s]" id.name
        else sprintf "[ebp%+d]" vi.Var.offset
    | _ -> ""

let rec repeat elem = function
  | 0 -> []
  | n -> elem :: (repeat elem (n - 1))

(* 局所関数用の変数の位置 *)
let lec_loc frm id =
  let rec pos_search frm dist =
    if Hashtbl.mem frm.tbl id.name then (frm, dist)
    else match frm.parent with
        None -> raise Not_found
      | Some par -> pos_search par (dist + (if frm.paramsec then 1 else 0))
  in
  try
    let (s, dist) = pos_search frm 0 in
    match Hashtbl.find s.tbl id.name with
        VarInfo vi ->
        (* スコープが同じかグローバル変数 *)
          if dist = 0 or s.lev = 0 then
            [{empty_code with inst = "mov dword";
              op1 = Some "eax"; op2 = Some (loc frm id)}]
          else if dist = 1 then
            [{empty_code with inst = "mov dword";
              op1 = Some "eax"; op2 = Some "[ebp+8]"};
             {empty_code with inst = "mov dword";
               op1 = Some "eax"; op2 = Some (sprintf "[eax%+d]" vi.Var.offset)}]
          else
            [{empty_code with inst = "mov dword";
              op1 = Some "eax"; op2 = Some "[ebp+8]"}] @
              (repeat 
                 {empty_code with inst = "mov dword";
                   op1 = Some "eax"; op2 = Some "[eax+8]"} (dist - 1)) @
              [{empty_code with inst = "mov dword";
                op1 = Some "eax"; op2 = Some (sprintf "[eax%+d]" vi.Var.offset)}]
      | FuncInfo fi ->
      (* グローバル関数 *)
        if s.lev = 0 then
          [{empty_code with inst = "call"; op1 = Some fi.Func.name}]
        else (
          if s.lev = frm.lev then
            [{empty_code with inst = "push"; op1 = Some "ebp"}]
          else if dist = 0 then
            [{empty_code with inst = "push dword"; op1 = Some "[ebp+8]"}]
          else
            [{empty_code with inst = "mov";
              op1 = Some "eax"; op2 = Some "[ebp+8]"}] @
              (repeat
                 {empty_code with inst = "mov";
                   op1 = Some "eax"; op2 = Some "[eax+8]"} dist) @
              [{empty_code with inst = "push"; op1 = Some "[ebp+8]"}])
          @ [{empty_code with inst = "call"; op1 = Some fi.Func.ulabel}]
  with Not_found ->
    [{empty_code with inst = "call"; op1 = Some id.name}]

(* あるテーブル中の変数の個数 *)
let var_num tbl =
  (Hashtbl.fold
     (fun k d r -> match d with
         VarInfo _ -> r + 1
       | _ -> r)
     tbl 0)

let rec generate tree global_functions = 
  print_code
    (* 未定義関数をEXTERNする *)
    ((List.map
        (fun f -> {empty_code with inst = "EXTERN"; op1 = Some f})
        (fst global_functions)) @
        (List.map
           (fun f -> {empty_code with inst = "GLOBAL"; op1 = Some f})
           (snd global_functions)) @
        (gen_program (gen_label (ref 0)) (root ()) tree))

and gen_program label_generator frm = function ExtDecList lst ->
  List.concat
    (List.map
       (function
         | ExtDec dec -> gen_ext_declaration frm dec
         | FuncDec dec -> gen_function_definition label_generator frm dec)
       lst)
    
and gen_ext_declaration frm = function VarDec lst ->
  List.map
    (fun id ->
      add_variable frm id 0;
      {empty_code with inst = "COMMON"; op1 = Some (id.name ^ " 4")})
    lst

and gen_function_definition label_generator frm =
  function FuncDef (id, prmlst, cmpst) ->
    let prm_frm = {
      paramsec = true;
      tbl = Hashtbl.create frame_size;
      lev = frm.lev + 1;
      var_locate = 0;
      parent = Some frm}
    in
    List.iter
      (function ParamDec id ->
        add_variable prm_frm id
          ((var_num prm_frm.tbl) * 4 + 8 + (if frm.lev = 0 then 0 else 4)))
      prmlst;
    let func_label = label_generator (sprintf "%s" id.name)
    and return_label = label_generator (sprintf "ret_%s" id.name)
    in
    add_function frm id (List.length prmlst) true ~ulabel:func_label;
    match gen_compound_statement
      label_generator return_label
      {paramsec = false;
       tbl = Hashtbl.create frame_size;
       lev = prm_frm.lev + 1; var_locate = 0;
       parent = Some prm_frm} cmpst
    with (x, body) ->
      [{empty_code with
        label = Some (if frm.lev = 0 then id.name else func_label);
        inst = "push"; op1 = Some "ebp"};
       {empty_code with inst = "mov";
         op1 = Some "ebp"; op2 = Some "esp"};
       {empty_code with inst = "sub";
         op1 = Some "esp"; op2 = Some (sprintf "%d" x)}]
      @ body @
        [{empty_code with label = Some return_label;
          inst = "mov"; op1 = Some "esp"; op2 = Some "ebp"};
         {empty_code with inst = "pop"; op1 = Some "ebp"};
         {empty_code with inst = "ret";}]
      
and gen_compound_statement label_generator return_label frm =
  function DecSt (declst, stlst) ->
    List.iter (function VarDec idlist ->
      List.iter
        (fun id -> add_variable frm id
          (frm.var_locate - ((var_num frm.tbl) + 1) * 4)) idlist) declst;
    List.fold_left
      (fun x y -> (max (fst x) (fst y), (snd x) @ (snd y)))
      (-frm.var_locate + (var_num frm.tbl) * 4, [])
      (List.map
         (fun st -> gen_statement label_generator return_label frm st)
          stlst)

and gen_statement label_generator return_label frm st =
  match st with
    | NilSt -> (0, [])
    | Expr expr -> gen_expression label_generator frm expr
    | CompSt cmpst ->
      gen_compound_statement
        label_generator
        return_label
        {tbl = Hashtbl.create frame_size;
         lev = frm.lev + 1; paramsec = false;
         var_locate = frm.var_locate - (var_num frm.tbl) * 4;
         parent = Some frm}
        cmpst
    | If (expr, st) ->
      let ecode = gen_expression label_generator frm expr
      and scode = gen_statement label_generator return_label frm st
      and label = label_generator "If"
      in (max (fst ecode) (fst scode),
          (snd ecode) @
            [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"};
             {empty_code with inst = "je"; op1 = Some label;}] @
            (snd scode) @ [{empty_code with label = Some label}])
    | IfElse (expr, st1, st2) ->
      let ecode = gen_expression label_generator frm expr
      and s1 = gen_statement label_generator return_label frm st1
      and s2 = gen_statement label_generator return_label frm st2
      and l1 = label_generator "IfElse1" and l2 = label_generator "IfElse2"
      in (max (fst ecode) (max (fst s1) (fst s2)),
          (snd ecode) @
            [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"};
             {empty_code with inst = "je"; op1 = Some l1;}] @
            (snd s1) @
            [{empty_code with inst = "jmp"; op1 = Some l2};
             {empty_code with label = Some l1}] @
            (snd s2) @ [{empty_code with label = Some l2}])
    | While (expr, st) ->
      let ecode = gen_expression label_generator frm expr
      and scode = gen_statement label_generator return_label frm st
      and l1 = label_generator "While1" and l2 = label_generator "While2"
      in
      (max (fst ecode) (fst scode),
       [{empty_code with label = Some l1}] @ (snd ecode) @
         [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"};
          {empty_code with inst = "je"; op1 = Some l2;}] @ (snd scode) @
         [{empty_code with inst = "jmp"; op1 = Some l1};
          {empty_code with label = Some l2}])
    | Return expr ->
      let ecode = gen_expression label_generator frm expr
      in (fst ecode, (snd ecode) @
        [{empty_code with inst = "jmp"; op1 = Some return_label}])
    | ReturnNothing ->
      (0, [{empty_code with inst = "jmp"; op1 = Some return_label}])
    | LocFuncDec func ->
      let over_locfunc = label_generator "over" in
      (0, 
       [{empty_code with inst = "jmp"; op1 = Some over_locfunc}] @
         (gen_function_definition label_generator frm func) @
         [{empty_code with label = Some over_locfunc}])
  
(* ほんとはマクロを使いたいんだなあ *)
and binary_operation label_generator frm e1 e2 last_code =
  let tmpvar_offs = frm.var_locate - ((var_num frm.tbl) + 1) * 4
  and tmpvar_name = label_generator "*TMP*"
  in
  add_variable
    frm
    {empty_identifier with name = tmpvar_name; offset = tmpvar_offs}
    tmpvar_offs;
  let c2 = (gen_expression label_generator frm e2)
  and c1 = (gen_expression label_generator frm e1)
  in let code = (snd c2) @
       [{empty_code with
         inst = "mov";
         op1 = Some (sprintf "[ebp%+d]" tmpvar_offs);
         op2 = Some "eax"}] @
       (snd c1) @ (last_code (sprintf "[ebp%+d]" tmpvar_offs))
     in
     Hashtbl.remove frm.tbl tmpvar_name;
     (max (fst c2) (fst c1), code)

and basic_operation label_generator frm op e1 e2 =
  binary_operation label_generator frm e1 e2 (fun tmpoffs ->
      [{empty_code with inst = op; op1 = Some "eax"; op2 = Some tmpoffs;}])

and compare_operation label_generator frm op e1 e2 =
  binary_operation label_generator frm e1 e2 (fun tmpoffs ->
    [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some tmpoffs};
     {empty_code with inst = op; op1 = Some "al"};
     {empty_code with inst = "movzx"; op1 = Some "eax"; op2 = Some "al"}])

and logand_operation label_generator frm e1 e2 =
  let tmpvar_offs = frm.var_locate - ((var_num frm.tbl) + 1) * 4
  and tmpvar_name = label_generator "*TMP*"
  in
  add_variable
    frm {empty_identifier with name = tmpvar_name; offset = tmpvar_offs} tmpvar_offs;
  let c1 = (gen_expression label_generator frm e1)
  and c2 = (gen_expression label_generator frm e2)
  and label = label_generator "LogAnd"
  in let code =
       [{empty_code with inst = "mov dword";
         op1 = Some (sprintf "[ebp%+d]" tmpvar_offs); op2 = Some "0"}] @
         (snd c1) @
         [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"}] @
         [{empty_code with inst = "je"; op1 = Some label;}] @
         (snd c2) @
         [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"}] @
         [{empty_code with inst = "je"; op1 = Some label;}] @
         [{empty_code with inst = "mov dword";
           op1 = Some (sprintf "[ebp%+d]" tmpvar_offs); op2 = Some "1"}] @
         [{empty_code with label = Some label; inst = "mov";
           op1 = Some "eax"; op2 = Some (sprintf "[ebp%+d]" tmpvar_offs)}]
     in
     Hashtbl.remove frm.tbl tmpvar_name;
     (max (fst c2) (fst c1), code)

and logor_operation label_generator frm e1 e2 =
  let tmpvar_offs = frm.var_locate - ((var_num frm.tbl) + 1) * 4
  and tmpvar_name = label_generator "*TMP*"
  in
  add_variable
    frm {empty_identifier with name = tmpvar_name; offset = tmpvar_offs} tmpvar_offs;
  let c1 = (gen_expression label_generator frm e1)
  and c2 = (gen_expression label_generator frm e2)
  and label = label_generator "LogOr"
  in let code =
       [{empty_code with inst = "mov dword";
         op1 = Some (sprintf "[ebp%+d]" tmpvar_offs); op2 = Some "1"}] @
         (snd c1) @
         [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"}] @
         [{empty_code with inst = "jne"; op1 = Some label;}] @
         (snd c2) @
         [{empty_code with inst = "cmp"; op1 = Some "eax"; op2 = Some "0"}] @
         [{empty_code with inst = "jne"; op1 = Some label;}] @
         [{empty_code with inst = "mov dword";
           op1 = Some (sprintf "[ebp%+d]" tmpvar_offs); op2 = Some "0"}] @
         [{empty_code with label = Some label; inst = "mov";
           op1 = Some "eax"; op2 = Some (sprintf "[ebp%+d]" tmpvar_offs)}]
     in
     Hashtbl.remove frm.tbl tmpvar_name;
     (max (fst c2) (fst c1), code)

and separete lst = function
  | 0 -> ([] , lst)
  | n -> let (x, y) = separete (List.tl lst) (n - 1)
         in ((List.hd lst) :: x, y)

and last lst = separete lst ((List.length lst) - 1)

and asgn_operation label_generator frm id expr =
  let tmpvar_offs = frm.var_locate - ((var_num frm.tbl) + 1) * 4
  and tmpvar_name = label_generator "*TMP*"
  in
  add_variable frm
    {empty_identifier with name = tmpvar_name; offset = tmpvar_offs} tmpvar_offs;
  let (x, y) = (gen_expression label_generator frm expr)
  and (vcode, [vpos]) = last (lec_loc frm id) 
  in
  let code =
    y @
      [{empty_code with inst = "mov";
        op1 = Some (sprintf "[ebp%+d]" tmpvar_offs); op2 = Some "eax"}] @
      vcode @
      [{empty_code with inst = "mov";
        op1 = Some "ecx"; op2 = Some (sprintf "[ebp%+d]" tmpvar_offs)};
       {empty_code with inst = "mov";
         op1 = vpos.op2; op2 = Some "ecx";
         comment = Some (sprintf "asign to %s" id.name)
       }]
  in
  Hashtbl.remove frm.tbl tmpvar_name; (x, code)
                                                  

and gen_expression label_generator frm = function
  | AsgnExpList lst ->
    List.fold_left
      (fun x y -> (max (fst x) (fst y), (snd x) @ (snd y)))
      (-frm.var_locate + (var_num frm.tbl) * 4, [])
      (List.map (fun expr -> gen_expression label_generator frm expr) lst)
  | Asgn (id, expr) ->
    asgn_operation label_generator frm id expr
    (* (match (gen_expression label_generator frm expr) with (x, y) -> *)
    (*   (x, y @ *)
    (*     [{empty_code with inst="mov";op1=Some (loc frm id);op2=Some "eax"}])) *)
  | Plus (e1, e2) -> basic_operation label_generator frm "add" e1 e2
  | Mul (e1, e2) ->  basic_operation label_generator frm "imul" e1 e2
  | Minus (e1, e2) -> basic_operation label_generator frm "sub" e1 e2
  | Div (e1, e2) ->
    binary_operation label_generator frm e1 e2 (fun tmpoffs ->
      [{empty_code with inst = "cdq"};
       {empty_code with inst = "idiv dword"; op1 = Some tmpoffs}])
  | Eq (e1, e2) -> compare_operation label_generator frm "sete" e1 e2
  | Neq (e1, e2) -> compare_operation label_generator frm "setne" e1 e2
  | Greater (e1, e2) -> compare_operation label_generator frm "setg" e1 e2
  | Smaller (e1, e2) -> compare_operation label_generator frm "setl" e1 e2
  | Greq (e1, e2) -> compare_operation label_generator frm "setge" e1 e2
  | Smeq (e1, e2) -> compare_operation label_generator frm "setle" e1 e2
  | Neg expr ->
    (match (gen_expression label_generator frm expr) with (x, y) ->
      (x, y @ [{empty_code with inst = "imul"; op1 = Some "eax"; op2 = Some "-1"}]))
  | Paren expr -> gen_expression label_generator frm expr
  | LogOr (e1, e2) -> logor_operation label_generator frm e1 e2
  | LogAnd (e1, e2) -> logand_operation label_generator frm e1 e2
  | FunCall (id, lst) ->
    (* 引数をスタックに積む *)
    (match List.fold_left
        (fun x y ->
          (max (fst x) (fst y),
           (snd y) @ [{empty_code with inst = "push"; op1 = Some "eax"}] @ (snd x)))
        (-frm.var_locate + (var_num frm.tbl) * 4, [])
        (List.map (fun expr -> gen_expression label_generator frm expr) lst)
     with (x, y) ->
       (x, y @
         (lec_loc frm id) @
         [{empty_code with inst = "add"; op1 = Some "esp";
            op2 = Some (sprintf "%d" ((List.length lst) * 4))}]))
  | Const x -> (match x with Constant c ->
    (0,[{empty_code with inst = "mov";
      op1 = Some "eax"; op2 = Some (sprintf "%d" c)}]))
  | Var id ->
    (-frm.var_locate + (var_num frm.tbl) * 4,lec_loc frm id)
