open Core
open Ast

let ss =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  Parser.program Lexer.token lexbuf