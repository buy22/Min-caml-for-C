(*
This file makes use of source code in the file ast.ml in the project "socc" to
record and print out the structure of the parse tree.
Author: noti0na1
Publish date: June, 2018
Title of the program: socc
Filename: ast.ml
Type: source code
Web address: https://github.com/noti0na1/socc
*)

open Core
open Ast

let ss =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let result = Parser.program Lexer.token lexbuf in
  (* output ast *)
  result |> string_of_prog |> print_string;
     Out_channel.newline stdout

let _ = ss
