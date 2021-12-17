{
  open Lexing
  open Parser

  exception SyntaxError of string

  let keyword_tabel =
    [("int", INT_KW);
     ("char", CHAR_KW);
     ("string", STRING_KW);
     ("return", RETURN_KW);
     ("if", IF_KW);
     ("else", ELSE_KW);
     ("for", FOR_KW);
     ("while", WHILE_KW)];;

  let rec find_token s kw_lst =
    match kw_lst with
    | [] -> Format.eprintf "%s\n" s; IDENT s
    | (a,b)::xs -> if s = a then (Format.eprintf "%s\n" b; b) else find_token s xs;;
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z' '_']
let identifier = letter (letter | digit)*

rule token = parse
  | eof { Format.eprintf "eof\n"; EOF }
  | whitespace { Format.eprintf "white space\n"; token lexbuf }
  | '{' { BRACE_OPEN }
  | '}' { BRACE_CLOSE }
  | '(' { Format.eprintf "(\n"; PAREN_OPEN }
  | ')' { PAREN_CLOSE }
  | ',' { COMMA }
  | ';' { Format.eprintf ";\n"; SEMICOLON }
  | ':' { COLON }
  | '!' { BANG }
  | '~' { COMPLEMENT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '^' { XOR }
  | '<' { Format.eprintf "<\n"; LT }
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
  | identifier as s { (find_token s keyword_tabel) }
  | "\"" as s { string "" lexbuf}
  | _ as c { raise (SyntaxError ("Unknown char: " ^ (Char.escaped c))) }

and string str = parse
  "\""  {STRING str}
  | _ as s  { string (str ^ (String.make 1 s)) lexbuf }
