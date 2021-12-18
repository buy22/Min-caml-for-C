%{
  open Core
  open Ast
%}

/* token definition */
%token <int> INT
%token <char> CHAR
%token <string> IDENT STRING
%token BRACE_OPEN BRACE_CLOSE PAREN_OPEN PAREN_CLOSE
%token COMMA SEMICOLON COLON
%token RETURN_KW
%token INT_KW CHAR_KW IF_KW ELSE_KW STRING_KW
%token FOR_KW WHILE_KW
%token BANG COMPLEMENT
%token PLUS MINUS NEG_MINUS MULT DIV MOD
%token EQ DOUBLE_EQ NEQ LT LE GT GE AND OR XOR
%token EOF


%start program
%type <Ast.prog> program

/* precedences */
%left COMMA
%right EQ
%left OR
%left AND
%left XOR
%left DOUBLE_EQ NEQ
%left LE LT GE GT
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc NEG_MINUS

%%

type_def:
  | INT_KW { IntType }
  | CHAR_KW { CharType }
  | STRING_KW { StringType }

/* support multiple function definition in a program  */
program:
  | function_declaration program  { match $2 with Prog [] -> Prog($1::[]) | Prog fs -> Prog($1::fs) }
  | EOF { Prog [] }

function_declaration:
  | type_def IDENT PAREN_OPEN parameters PAREN_CLOSE BRACE_OPEN body_block BRACE_CLOSE
  { Function { fun_type = $1; name = ID $2; params = $4; body = Some($7) } }

/* no parameters, one paramter, more than one parameters */
parameters:
  | { [] }
  | type_def IDENT  { [Param($1, ID $2)] }
  | type_def IDENT COMMA parameters  { (Param($1, ID $2)) :: $4 }

body_block:
  | statements {$1}

statements:
  | {[]}
  | statement statements  {$1::$2}

/* the semicolon are inside each specific statement */
/* return statement to put on top since it is the base condition */
statement:
  | return_statement  {Statement $1}
  | declaration {Decl $1}
  | for_statement {Statement $1}
  | while_statement {Statement $1}

declaration:
  | type_def IDENT SEMICOLON {{var_type=$1; var_name=ID $2; init=None}}
  | type_def IDENT assign_op expression SEMICOLON {{var_type=$1; var_name=ID $2; init=Some $4}}

assignment:
  | IDENT assign_op expression  {Assign ($2, ID $1, $3)}

/* for statements with different initail conditions */
for_statement:
  | FOR_KW PAREN_OPEN declaration expression SEMICOLON expression PAREN_CLOSE BRACE_OPEN body_block BRACE_CLOSE {ForDecl{init=$3; cond=$4; post=Some $6; body= $9}}
  | FOR_KW PAREN_OPEN expression SEMICOLON expression SEMICOLON expression PAREN_CLOSE BRACE_OPEN body_block BRACE_CLOSE {For{init=Some $3; cond=$5; post=Some $7; body= $10}}

while_statement:
  | WHILE_KW PAREN_OPEN expression PAREN_CLOSE BRACE_OPEN body_block BRACE_CLOSE {While{cond=$3; body=$6}}

return_statement:
  | RETURN_KW expression SEMICOLON {ReturnVal $2}

expression:
  | assignment {$1}
  | monop bin_expression {MonOp($1,$2)}
  | bin_expression {$1}

bin_expression:
  | bin_expression binop atom_expression {BinOp($2,$1,$3)}
  | atom_expression {$1}

atom_expression:
  | variable  {Var $1}
  | const {Const $1}
  | PAREN_OPEN expression PAREN_CLOSE { $2 }

variable:
  | IDENT {ID $1}

const:
  | INT {Int $1}
  | CHAR  {Char $1}
  | STRING  {String $1}

monop:
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
  | XOR { Xor }

/* only support one assign operation here */
assign_op:
  | EQ { Equals }
