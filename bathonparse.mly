/* Ocamlyacc parser for Bathon */

%{
open Ast
%}

%token COLON SEMI PERIOD UNDERSCORE COMMAND LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET ASSIGN MATCH WITH LAMBDA
%token BAND BOR BXOR BLS BRS
%token NEG NOT BNOT
%token PLUS MINUS TIMES DIVIDE MOD FDIVIDE EXP
%token EQ NEQ GT LT GEQ LEQ AND OR IN
%token IF ELIF ELSE FOR WHILE TRY EXCEPT ASSERT RAISING CONTINUE BREAK PASS 
%token INT BOOL STR LIST TUPLE RANGE DICT SET NONE
/* return, COMMA token */
%token DEF RETURN COMMA CLASS
%token <int> LITERAL
%token <float> FLITERAL
%token <string> SLITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right COMMAND
%right ASSIGN MATCH WITH LAMBDA IN
%right NEG NOT BNOT
%left OR
%left AND
%left EQ NEQ
%left GT LT GTE LTE
%left PLUS MINUS
%left TIMES DIVIDE FDIVIDE
%left MOD EXP
%left BAND BOR BXOR BLS BRS

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | stmt_list {([], $1)}
 | fdecl_list decls {([$1, $2])}

/* int x */
vdecl:
  ID COLON typ { ($1, $3) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | STR   { Str }

/* fdecl */
fdecl:
  DEF ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {
    {
      fname=$2;
      formals=$4
      body=$7
    }
  }

fdecl_list:
  /* nothing */ { [] }
  | fdecl fdecl_list  { $1::$2 }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr                                    { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELIF LPAREN expr RPAREN stmt ELSE stmt    { IfElif($3, $5, $8, $10, $11) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN ID IN expr RPAREN stmt       { For($3, $5, $7)}
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr                             { Return $2      }

expr:
    LITERAL          { IntLit($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | FLITERAL         { FloatLit($1)           }
  | SLITERAL         { StrLit($1)             }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mul,   $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr FDIVIDE expr { Binop($1, FDiv, $3)   }
  | expr MOD    expr { Binop($1, Modulo, $3)  }
  | expr Exp    expr { Binop($1, Exp,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LEQ    expr { Binop($1, Leq, $3)     }
  | expr GEQ    expr { Binop($1, Geq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr BAND   expr { Binop($1, Band,  $3)   }
  | expr BOR    expr { Binop($1, Bor,   $3)   }
  | expr BXOR   expr { Binop($1, Bxor,  $3)   }
  | expr BLS    expr { Binop($1, Ls,    $3)   }
  | expr BRS    expr { Binop($1, Rs,    $3)   }
  | MINUS expr %prec NEG  { Unop(Neg, $2)          }
  | NOT expr         { Unop(Not, $2)          }
  | BNOT expr        { Unop(Bnot, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | COMMAND expr COMMAND      { Cmd ($1)}

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
