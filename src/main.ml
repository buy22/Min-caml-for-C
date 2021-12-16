open Core
open Ast
open Gnerate

let ss =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let result = Parser.program Lexer.token lexbuf in
  gen_prog result Out_channel.stdout

let _ = ss;;