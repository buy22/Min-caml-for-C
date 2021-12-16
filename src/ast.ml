type const =
  | Int of int
  | Char of char
  | String of string

type type_def =
  | IntType
  | CharType

type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | Leq
  | Geq
  | Neq
  | Eq
  | And
  | Or
  | Xor

type assign_op =
  | Equals (* = *)

type monoop = Negate | Pos | Complement | Not

type id = ID of string

type exp =
  | Const of const
  | Var of id
  | Monoop of monoop * exp
  | BinOp of binop * exp * exp
  | TernOp of exp * exp * exp
  | Assign of assign_op * id * exp
  | FunCall of id * exp list

type declaration =
  { var_type: type_def;
    var_name: id;
    init: exp option;
  }

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

type fun_param = Param of type_def * id

type fun_declaration =
  { fun_type: type_def;
    name: id;
    storage_class: storage_class;
    params: fun_param list;
    body: block option;
  }

type top_level =
  | Function of fun_declaration
  | GlobalVar of declaration

type prog = Prog of top_level list
