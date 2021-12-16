open Core

type const =
  | Int of int
  | Char of char
  | String of string
[@@deriving sexp]

type type_def =
  | IntType
  | CharType
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
  | Equals (* = *)
[@@deriving sexp]

type monop = Negate | Pos | Complement | Not
[@@deriving sexp]

type id = ID of string
[@@deriving sexp]

type exp =
  | Const of const
  | Var of id
  | MonOp of monop * exp
  | BinOp of binop * exp * exp
  | TernOp of exp * exp * exp
  | Assign of assign_op * id * exp
  | FunCall of id * exp list
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
  | For of {init: exp option; cond: exp; post: exp option; body: statement}
  | ForDecl of {init: declaration; cond: exp; post: exp option; body: statement}
  | While of {cond: exp; body: statement}
  | DoWhile of {body: statement; cond: exp}
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

type top_level =
  | Function of fun_declaration
  | GlobalVar of declaration
[@@deriving sexp]

type prog = Prog of top_level list
[@@deriving sexp]


let string_of_prog p = p |> sexp_of_prog |> Sexp.to_string_hum ~indent:4