{
  open Parser
  
  exception End_of_file
}

let space = [' ' '\t' '\r']
let nl = '\013' '\010' | '\010'
let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_']
let ident_num = ['a'-'z' 'A'-'Z' '_' '0'-'9']

rule token = parse
  | space+                   { token lexbuf }
  | nl                       { Lexing.new_line lexbuf; token lexbuf } 
  | digit+ as num            { CONSTANT (int_of_string num) }
  | ','      { COMMA }
  | ';'      { SEMICOLON }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULTIPLY }
  | '/'      { DIVIDE }
  | '='      { EQ }
  | "=="     { EQUAL }
  | "!="     { NOTEQUAL }
  | "||"     { LOR }
  | "&&"     { LAND }
  | '<'      { SMALLER }
  | '>'      { GREATER }
  | "<="     { SMALLEREQ }
  | ">="     { GREATEREQ }
  | "int"    { INT }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "return" { RETURN }
  | "local"  { LOCAL }
  | ident ident_num* as word { IDENTIFIER (word, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) }
  | eof                      { EOF }
  | _ { failwith
          (Printf.sprintf
             "unknown token %s near characters %d-%d"
             (Lexing.lexeme lexbuf)
             (Lexing.lexeme_start lexbuf)
             (Lexing.lexeme_end lexbuf)) }
