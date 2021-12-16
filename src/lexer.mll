{
  open Core
  open Lexing
  open Parser

  exception SyntaxError of string

  (* TODO: add skip comment *)
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0' - '9']
let letter = ['a' - 'z' 'A' - 'Z' '_']
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | whitespace { token lexbuf }
  | newline  { next_line lexbuf; token lexbuf }
  | "void"  { VOID_KW }
  | "int"   { INT_KW }
  | "char"  { CHAR_KW }
  | "float" { FLOAT_KW }
  | "static"  { STATIC_KW }
  | "sizeof"  { SIZEOF_KW }
  | "return"  { RETURN_KW }
  | "if"    { IF_KW }
  | "else"  { ELSE_KW }
  | "for"   { FOR_KW }
  | "while" { WHILE_KW }
  | "break" { BREAK_KW }
  | "continue"  { CONTINUE_KW }
  | '{' { BRACE_OPEN }
  | '}' { BRACE_CLOSE }
  | '(' { PAREN_OPEN }
  | ')' { PAREN_CLOSE }
  | '[' { BRACKET_OPEN }
  | ']' { BRACKET_CLOSE }
  | ',' { COMMA }
  | '?' { QUESTION }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '!' { BANG }
  | '~' { COMPLEMENT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '%' { MOD }
  | '&' { BIT_AND }
  | '|' { BIT_OR }
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
  | "+=" { PLUS_EQ }
  | "-=" { MINUS_EQ }
  | "*=" { MULT_EQ }
  | "/=" { DIV_EQ }
  | "%=" { MOD_EQ }
  | "->" { ARROW }
  | digit+ as integer { INT(int_of_string integer) }
  | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)? as number { FLOAT(float_of_string number) }
  | letter (letter|digit)* as id { ID id }
  | _ as c { raise (SyntaxError ("Unknown char: " ^ (Char.escaped c))) }
  | eof { EOF }