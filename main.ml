open Management

let _ =
  Format.set_margin 40;
  let lexbuf = Lexing.from_channel stdin in
  let tree = (Parser.program Lexer.token lexbuf) in
  (* Print.pp_tree tree; *)
  let root = root () in
  Analysis.go_program root tree;
  if !semnerrs = 0 then
    Generate.generate tree
      (Hashtbl.fold
         (fun k d r -> match d with
             FuncInfo fi ->
               if fi.Func.defined = false
               then (fi.Func.name :: (fst r), snd r) (* undef_functions *)
               else (fst r, fi.Func.name :: (snd r)) (* defed_functions *)
           | _ -> r)
         root.tbl ([], []))
