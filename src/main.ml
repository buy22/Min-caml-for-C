open Core
open Ast

let ss =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let result = Parser.program Lexer.token lexbuf in
  (* output ast *)
  result |> string_of_prog |> print_string;
     Out_channel.newline stdout

let _ = ss