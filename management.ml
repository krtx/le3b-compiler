(* 変数管理 *)

open Printf
open Syntax

exception Redeclared of string

module Var = struct
  type info = {name : string; mutable offset : int}
end

module Func = struct
  type info = {name : string; narg : int; mutable defined : bool; ulabel : string}
end

type id_info = VarInfo of Var.info | FuncInfo of Func.info

let frame_size = 100
type frame = {
  paramsec : bool;
  tbl : (string, id_info) Hashtbl.t;
  lev : int;
  var_locate : int;
  parent : frame option
}

let root () = {
  paramsec = false;
  tbl = Hashtbl.create 100;
  lev = 0;
  var_locate = 0;
  parent = None
}

let semnerrs = ref 0

let error fmt = incr semnerrs; fprintf stderr ("error: " ^^ fmt ^^ "\n")
let warn fmt = fprintf stderr ("warning: " ^^ fmt ^^ "\n")

let add_variable frm id offs =
  Hashtbl.add frm.tbl id.name (VarInfo {Var.name = id.name; Var.offset = offs});
  id.level <- frm.lev;
  id.offset <- offs

let add_function ?(ulabel="") frm id narg defined =
  Hashtbl.add frm.tbl id.name
    (FuncInfo {
      Func.name = id.name; Func.narg = narg;
      Func.defined = defined; Func.ulabel = ulabel;
    });
  id.level <- frm.lev;
  id.narg <- narg;
  id.defined <- defined

(* id が存在するフレームを返す。なかったらNot_found *)
let rec search frm id =
  if Hashtbl.mem frm.tbl id.name then frm
  else match frm.parent with
      None -> raise Not_found
    | Some frm -> search frm id
      
(* 
   同じレベルに同じ名前のオブジェクトがあればエラー
   パラメータ宣言に同じ名前の変数があれば警告
*)
let declare_variable frm id =
  (try
    let s = search frm id in
    if s.lev = frm.lev then
      match Hashtbl.find s.tbl id.name with
          VarInfo _ ->
          error "Line %d, redeclaration of variable `%s'"
            id.lex_start.Lexing.pos_lnum id.name;
      | FuncInfo _ ->
          error "Line %d, `%s' redeclared as different kind of symbol"
            id.lex_start.Lexing.pos_lnum id.name;
    (* 同じ名前のパラメータがあるかどうか *)
    else if s.lev = 1 then
      warn "Line %d, declaration of `%s' shadows a parameter"
        id.lex_start.Lexing.pos_lnum id.name
  with
      Not_found -> ());
  (* エラーリカバリも兼ねる *)
  add_variable frm id
    (if frm.lev >= 2 then frm.var_locate - (Hashtbl.length frm.tbl) * 4 else -1)

(* rootに同じ名前の定義済み関数があるか定義済みではないが引数の数が違う関数があればエラー *)
let define_function frm id narg =
  (try let s = search frm id in
       if s.lev = frm.lev then
         match Hashtbl.find s.tbl id.name with
             VarInfo _ ->
               error "Line %d, `%s' redeclared as different kind of symbol"
                 id.lex_start.Lexing.pos_lnum id.name;
           | FuncInfo fi ->
             if fi.Func.defined = false then
               (* 定義済みでない関数の引数の数が違っていた *)
               (if fi.Func.narg != narg then 
                   error "Line %d, wrong number of arguments `%s'"
                     id.lex_start.Lexing.pos_lnum id.name
                else
                   fi.Func.defined <- true)
             else
               (* 定義済みの関数があった *)
               error "Line %d, redefinition of function `%s'" 
                 id.lex_start.Lexing.pos_lnum id.name
   with
       Not_found -> ());
  add_function frm id narg true

(* 関数と同じ *)
let declare_parameter frm id =
  try
    match Hashtbl.find frm.tbl id.name with
        VarInfo _ -> error "Line %d, redeclaration of parameter `%s'" id.lex_start.Lexing.pos_lnum id.name
      | FuncInfo _ -> error "okasiiyo"
  with
      Not_found ->
        add_variable frm id ((Hashtbl.length frm.tbl) * 4 + 8)

(* 自分と同じかより上のレベルに同じ名前の変数が見つからなければエラー *)
let refer_variable _frm id =
  let rec climb frm =
    try
      match Hashtbl.find frm.tbl id.name with
          VarInfo _ -> id.level <- frm.lev (* 見つかった *)
        | FuncInfo _ ->
            error "Line %d, function `%s' is used as variable"
              id.lex_start.Lexing.pos_lnum id.name
    with
        Not_found ->
          match frm.parent with
              None ->
                error "Line %d, `%s' undeclared variable"
                  id.lex_start.Lexing.pos_lnum id.name;
                (* エラーリカバリ *)
                let offs =
                  if _frm.lev >= 2
                  then _frm.var_locate - (Hashtbl.length _frm.tbl) * 4 else -1
                in
                add_variable frm id offs
            | Some frm -> climb frm
  in (climb _frm)

let call_function frm id narg =
  let rec climb frm =
    try
      match Hashtbl.find frm.tbl id.name with
          VarInfo _ -> ()
        | FuncInfo fi ->
          if fi.Func.narg != narg then (* 引数の数が違ってた *)
            error "Line %d, wrong number of arguments `%s'"
              id.lex_start.Lexing.pos_lnum id.name
          else (
            id.level <- frm.lev;
            id.narg <- narg;
            id.defined <- true
          )
    with
        Not_found ->
          match frm.parent with
              (* 未定義の関数を呼び出した *)
              None -> (warn "Line %d, `%s' undeclared function"
                         id.lex_start.Lexing.pos_lnum id.name;
                       add_function frm id narg false)
            | Some frm -> climb frm
  in (climb frm)

