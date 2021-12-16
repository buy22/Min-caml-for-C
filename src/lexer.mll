{
  open Lexing
  open Parser

  exception SyntaxError of string

  (* TODO: add skip comment *)
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      };;

  let keyword_tabel =
    [("int", INT_KW);
     ("char", CHAR_KW);
     ("return", RETURN_KW);
     ("if", IF_KW);
     ("else", ELSE_KW);
     ("for", FOR_KW);
     ("while", WHILE_KW)];;

  let rec find_token s kw_lst =
    match kw_lst with
    | [] -> Format.eprintf "%s\n" s;IDENT s
    | (a,b)::xs -> if s = a then (Format.eprintf "%s\n" a;b) else find_token s xs;;
}

let digit = ['0'-'9']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | eof { Format.eprintf "eof\n"; EOF }
  | white { Format.eprintf "white space\n"; token lexbuf }
  | newline  { next_line lexbuf; token lexbuf }
  | '{' { BRACE_OPEN }
  | '}' { Format.eprintf "hhhh\n"; BRACE_CLOSE }
  | '(' { PAREN_OPEN }
  | ')' { PAREN_CLOSE }
  | ',' { COMMA }
  | ';' { Format.eprintf "semicolon\n"; SEMICOLON }
  | ':' { COLON }
  | '!' { BANG }
  | '~' { COMPLEMENT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '^' { XOR }
  | '<' { LT }
  | "<=" { LE }
  | '>' { GT }
  | ">=" { GE }
  | '=' { EQ }
  | "==" { DOUBLE_EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "<<" { SHIFT_LEFT }
  | ">>" { SHIFT_RIGHT }
  | digit+ as integer { Format.eprintf "integer\n"; INT(int_of_string integer) }
  | id as s { (find_token s keyword_tabel) }
  | _ as c { raise (SyntaxError ("Unknown char: " ^ (Char.escaped c))) }
