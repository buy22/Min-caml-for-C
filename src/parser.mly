%{
  open Ast
%}

%token <int> INT
%token <char> CHAR
%token <string> IDENT
%token BRACE_OPEN BRACE_CLOSE PAREN_OPEN PAREN_CLOSE BRACKET_OPEN BRACKET_CLOSE
%token COMMA QUESTION SEMICOLON COLON
%token VOID_KW INT_KW CHAR_KW LONG_KW UNSIGNED_KW FLOAT_KW DOUBLE_KW
%token STRUCT_KW CONST_KW STATIC_KW SIZEOF_KW RETURN_KW GOTO_KW
%token IF_KW ELSE_KW SWITCH_KW FOR_KW DO_KW WHILE_KW BREAK_KW CONTINUE_KW
%token BANG COMPLEMENT
%token PLUS MINUS NEG_MINUS MULT DIV MOD
%token PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ
%token BIT_AND_EQ BIT_OR_EQ XOR_EQ SHIFT_LEFT_EQ SHIFT_RIGHT_EQ
%token EQ DOUBLE_EQ NEQ LT LE GT GE AND OR
%token BIT_AND BIT_OR XOR SHIFT_LEFT SHIFT_RIGHT
%token ARROW ADDROF DEREF
%token EOF

%left COMMA
%right EQ PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ BIT_AND_EQ BIT_OR_EQ XOR_EQ SHIFT_LEFT_EQ SHIFT_RIGHT_EQ
%right QUESTION COLON
%left OR
%left AND
%left BIT_OR
%left XOR
%left BIT_AND
%left DOUBLE_EQ NEQ
%left LE LT GE GT
%left SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc NEG_MINUS ADDROF DEREF

/* %type <Ast.prog> program */
/* %type <fun_declaration> fun_declaration */
/* %type <exp> exp */

%start program
%type <Ast.prog> program

%%

type_def:
  | INT_KW { IntType }
  | CHAR_KW { CharType }

program:
  | fun_declaration  { Prog(Function($1)::[]) }
  | EOF { Prog [] }

fun_declaration:
  | type_def IDENT PAREN_OPEN parameters PAREN_CLOSE body_block
  { { fun_type = $1; name = ID $2; params = $4; body = Some($6) } }

/* no parameters, one paramter, more than one parameters */
parameters:
  | { [] }
  | type_def IDENT  { [Param($1, ID $2)] }
  | type_def IDENT COMMA parameters  { (Param($1, ID $2)) :: $4 }

body_block:
  | BRACE_OPEN statements BRACE_CLOSE {$2}

statements:
  | {[]}
  | statement statements  {$1::$2}

statement:
  | declaration SEMICOLON {Decl $1}
  | for_statement SEMICOLON {$1}
  | return_statement SEMICOLON  {$1}

declaration:
  /* | mult_declaration SEMICOLON  {$1} */
  | type_def IDENT SEMICOLON {{var_type=$1; var_name=ID $2; init=None}}
  | type_def IDENT EQ expression SEMICOLON {{var_type=$1; var_name=ID $2; init=Some $4}}

/* mult_declaration: */

for_statement:
  | FOR_KW PAREN_OPEN declaration SEMICOLON expression SEMICOLON expression PAREN_CLOSE body_block {Statement(ForDecl{init=$3; cond=$5; post=Some $7; body=Block $9})}
  | FOR_KW PAREN_OPEN expression SEMICOLON expression SEMICOLON expression PAREN_CLOSE body_block {Statement(For{init=Some $3; cond=$5; post=Some $7; body=Block $9})}

return_statement:
  | RETURN_KW expression SEMICOLON {Statement(ReturnVal $2)}

expression:
  | IDENT  {Var(ID $1)}
  | INT {Const(Int $1)}
  | unop expression {Monoop($1,$2)}
  | expression binop expression {BinOp($2,$1,$3)}

unop:
  | COMPLEMENT  {Complement}
  | BANG  {Not}

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | MULT { Mult }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | DOUBLE_EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
  /* | BIT_AND { BitAnd }
  | BIT_OR { BitOr } */
  | XOR { Xor }
  /* | SHIFT_LEFT { ShiftL }
  | SHIFT_RIGHT { ShiftR } */

assign_op:
  | EQ { Equals }
  /* | PLUS_EQ { AddEq }
  | MINUS_EQ { SubEq }
  | MULT_EQ { MultEq }
  | DIV_EQ { DivEq }
  | MOD_EQ { ModEq }
  | BIT_AND_EQ { BitAndEq }
  | BIT_OR_EQ { BitOrEq }
  | XOR_EQ { XorEq }
  | SHIFT_LEFT_EQ { ShiftLEq }
  | SHIFT_RIGHT_EQ { ShiftREq } */
%%
