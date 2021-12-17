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

type const =
  | Int of int
  | Char of char
  | String of string
[@@deriving sexp]

type type_def =
  | IntType
  | CharType
  | StringType
[@@deriving sexp]

type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Lt
  | Le
  | Gt
  | Ge
  | Leq
  | Geq
  | Neq
  | Eq
  | And
  | Or
  | Xor
[@@deriving sexp]

type assign_op =
  | Equals
[@@deriving sexp]

type monop = Complement | Not
[@@deriving sexp]

type id = ID of string
[@@deriving sexp]

type exp =
  | Const of const
  | Var of id
  | MonOp of monop * exp
  | BinOp of binop * exp * exp
  | Assign of assign_op * id * exp
[@@deriving sexp]

type declaration =
  { var_type: type_def;
    var_name: id;
    init: exp option;
  }
[@@deriving sexp]

type block_item =
  | Statement of statement
  | Decl of declaration

and block = block_item list

and statement =
  | Block of block
  | If of {cond: exp; if_body: statement; else_body: statement option}
  | Exp of exp option
  | For of {init: exp option; cond: exp; post: exp option; body: block}
  | ForDecl of {init: declaration; cond: exp; post: exp option; body: block}
  | While of {cond: exp; body: statement}
  | ReturnVal of exp
  | Break
  | Continue
[@@deriving sexp]

type fun_param = Param of type_def * id
[@@deriving sexp]

type fun_declaration =
  { fun_type: type_def;
    name: id;
    params: fun_param list;
    body: block option;
  }
[@@deriving sexp]

type global =
  | Function of fun_declaration
  | GlobalVar of declaration
[@@deriving sexp]

type prog = Prog of global list
[@@deriving sexp]


let string_of_prog p = p |> sexp_of_prog |> Sexp.to_string_hum ~indent:4
