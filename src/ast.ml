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

type assign_op =
  | Equals (* = *)

type monop = Negate | Pos | Complement | Not

type id = ID of string

type exp =
  | Const of const
  | Var of id
  | MonOp of monop * exp
  | BinOp of binop * exp * exp
  | TernOp of exp * exp * exp
  | Assign of assign_op * id * exp
  | FunCall of id * exp list

type declaration =
  { var_type: type_def;
    var_name: id;
    init: exp option;
  }

and block = statement list

and statement =
  | Block of block
  | If of {cond: exp; if_body: statement; else_body: statement option}
  | Exp of exp option
  | For of {init: exp option; cond: exp; post: exp option; body: statement}
  | ForDecl of {init: declaration; cond: exp; post: exp option; body: statement}
  | While of {cond: exp; body: statement}
  | DoWhile of {body: statement; cond: exp}
  | ReturnVal of exp
  | Decl of declaration
  | Break
  | Continue

type fun_param = Param of type_def * id

type fun_declaration =
  { fun_type: type_def;
    name: id;
    params: fun_param list;
    body: block option;
  }

type top_level =
  | Function of fun_declaration
  | GlobalVar of declaration

type prog = Prog of top_level list