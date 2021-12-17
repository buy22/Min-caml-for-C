{
  open Lexing
  open Parser

  exception SyntaxError of string
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z' '_']
let identifier = letter (letter | digit)*

rule token = parse
  | eof { Format.eprintf "eof\n"; EOF }
  | whitespace { Format.eprintf "whitespace\n"; token lexbuf }
  | "int" { INT_KW }
  | "char" { CHAR_KW }
  | "string" { STRING_KW }
  | "return" { RETURN_KW }
  | "if" { IF_KW }
  | "else"  { ELSE_KW }
  | "for"   { FOR_KW }
  | "while" { WHILE_KW }
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
  | digit+ as integer { Format.eprintf "integer\n"; INT(int_of_string integer) }
  | identifier as s { IDENT s }
  | "\"" as s { string "" lexbuf}
  | _ as c { raise (SyntaxError ("Unknown char: " ^ (Char.escaped c))) }

and string str = parse
  "\""  {STRING str}
  | _ as s  { string (str ^ (String.make 1 s)) lexbuf }
