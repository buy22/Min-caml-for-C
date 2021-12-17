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
  | "int" { Format.eprintf "int\n"; INT_KW }
  | "char" { Format.eprintf "char\n"; CHAR_KW }
  | "string" { Format.eprintf "string\n"; STRING_KW }
  | "return" { Format.eprintf "return\n"; RETURN_KW }
  | "if" { Format.eprintf "if\n"; IF_KW }
  | "else"  { Format.eprintf "else\n"; ELSE_KW }
  | "for"   { Format.eprintf "for\n"; FOR_KW }
  | "while" { Format.eprintf "while\n"; WHILE_KW }
  | '{' { Format.eprintf "{\n"; BRACE_OPEN }
  | '}' { Format.eprintf "}\n"; BRACE_CLOSE }
  | '(' { Format.eprintf "(\n"; PAREN_OPEN }
  | ')' { Format.eprintf ")\n"; PAREN_CLOSE }
  | ',' { Format.eprintf ",\n"; COMMA }
  | ';' { Format.eprintf ";\n"; SEMICOLON }
  | ':' { Format.eprintf ":\n"; COLON }
  | '!' { Format.eprintf "!\n"; BANG }
  | '~' { Format.eprintf "~\n"; COMPLEMENT }
  | '+' { Format.eprintf "+\n"; PLUS }
  | '-' { Format.eprintf "-\n"; MINUS }
  | '*' { Format.eprintf "*\n"; MULT }
  | '/' { Format.eprintf "/\n"; DIV }
  | '%' { Format.eprintf "mod\n"; MOD }
  | '^' { Format.eprintf "^\n"; XOR }
  | '<' { Format.eprintf "<\n"; LT }
  | "<=" { Format.eprintf "<=\n"; LE }
  | '>' { Format.eprintf ">\n"; GT }
  | ">=" { Format.eprintf ">=\n"; GE }
  | '=' { Format.eprintf "=\n"; EQ }
  | "==" { Format.eprintf "==\n"; DOUBLE_EQ }
  | "!=" { Format.eprintf "!=\n"; NEQ }
  | "&&" { Format.eprintf "&&\n"; AND }
  | "||" { Format.eprintf "||\n"; OR }
  | digit+ as integer { Format.eprintf "%s\n" integer; INT(int_of_string integer) }
  | identifier as s { Format.eprintf "%s\n" s; IDENT s }
  | "\"" as s { string "" lexbuf}
  | _ as c { raise (SyntaxError ("Unknown String to Lexer")) }

and string str = parse
  "\""  {STRING str}
  | _ as s  { string (str ^ (String.make 1 s)) lexbuf }
