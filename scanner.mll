{ open Parser 
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule tokenize = parse
 [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "#"        { comment lexbuf }
| "True"     { BLITERAL (true) }
| "False"    { BLITERAL (false) }
| "None"     { NONE }
| "int"      { INT }
| "float"    { FLOAT }
| "bool"     { BOOL }
| "str"      { STR }
| "list"     { LIST }
| "tuple"    { TUPLE }
| "range"    { RANGE }
| "dict"     { DCIT }
| "set"      { SET }
(* Arithmetic Operators *)
| '%'        { MOD }
| '>'        { GT }
| '<'        { LT }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| "//"       { FDIVIDE } 
| "**"       { EXPO }
| '='        { ASSIGN }
(* Comparison Operators *)
| "=="       { EQ }
| "=<"       { LTE }
| "<"        { LT }
| ">="       { GTE }
| ">"        { GT }
| "!="       { NE }
( * Logical Operators)
| "not" 	   { NOT }
| "and"      { AND }
| "or"       { OR }
( * Bitwise Operation *)
| '&'        { BAND }
| '|'        { BOR }
| '^'        { BXOR }
| "<<"       { BLTS }
| ">>"       { BGTS }
| '~'        { BNOT}
( * Membership Operator *)
| "in"       { IN }
(* Delimiter *)
| ':'        { COLON }
| '.'        { PERIOD }
| ','        { COMMA }
| ';'        { SEMC }
| '_'        { UNDERSCORE }
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE }
| '['	       { LBRACKET }
| ']'	       { RBRACKET }
(* Keywords *)
| "try"      { TRY }
| "except"   { EXCEPT }
| "assert"   { ASSERT}
| "raising"  { RAISING }
| "continue" { CONTINUE }
| "break"    { BREAK } 
| "match"    { MATCH }
| "lambda"   { LAMBDA }
| "with"     { WITH }
| "if"       { IF }
| "elif"     { ELIF}
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "pass"     { PASS }
| "def"      { DEF }
| "return"   { RETURN }
(* Class *)
| "class"    { CLASS } 
(* Command *)
| '`'        {command lexbuf}

| string_literal as lxm { SLITERAL(remove_quotes lxm) }
| digit+ as lxm { ILITERAL(int_of_string lxm) }
| flt as lxm { FLITERAL(lxm) }
| letter['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { ID(lxm) }
| eof     { EOF }
| _ as char { raise (Failure("illegal character" ^ Char.escaped char)) } 

and comment = parse
| '\n' { tokenize lexbuf }
| _    { comment lexbuf }

and command = parse 
| '`' { tokenize lexbuf }
| _   { command lexbuf}
