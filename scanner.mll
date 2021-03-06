(* Ocamllex scanner for Bathon *)

{ open Bathonparse
  let remove_quotes str =
  match String.length str with
  | 0 | 1 | 2 -> "" 
  | len -> String.sub str 1 (len - 2)
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let float = (digit+) '.' (digit+)
let string_literal = ('"'[' '-'~']*'"')


rule tokenize = parse
 [' ' '\t' '\r'] { tokenize lexbuf }
| '\n'+      { EOL }
| "#"        { comment lexbuf }
| "True"     { BLIT (true) }
| "False"    { BLIT (false) }
| "None"     { NONE }
| "int"      { INT }  (* type keywords may be unnecessary, types inferenced by values instead of keywords *)
| "float"    { FLOAT } 
| "bool"     { BOOL }
| "str"      { STR }
| "list"     { LIST }
| "tuple"    { TUPLE }
| "range"    { RANGE }
| "dict"     { DICT }
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
| "**"       { EXP }
| '='        { ASSIGN }
(* Comparison Operators *)
| "=="       { EQ }
| "<="       { LEQ }
| "<"        { LT } (* dup *)
| ">="       { GEQ }
| ">"        { GT } (* dup *)
| "!="       { NEQ }
(* Logical Operators *)
| "not" 	   { NOT }
| "and"      { AND }
| "or"       { OR }
(* Bitwise Operation *)
| '&'        { BAND }
| '|'        { BOR }
| '^'        { BXOR }
| "<<"       { BLS } (* LS: left shift *)
| ">>"       { BRS } (* RS: right shift *)
| '~'        { BNOT}
(* Membership Operator *)
| "in"       { IN }
(* Delimiter *)
| ':'        { COLON }
| '.'        { PERIOD }
| ','        { COMMA }
| ';'        { SEMI }
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
| string_literal as lxm { SLITERAL(remove_quotes lxm) }
| digit+ as lxm { ILITERAL(int_of_string lxm) }
| float as lxm  { FLITERAL(float_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { ID(lxm) }
| '`' [' '-'~']* '`' as lxm { COMMAND(remove_quotes lxm) }
| eof     { EOF }
| _ as char { raise (Failure("illegal character" ^ Char.escaped char)) } 

and comment = parse
| '\n' { tokenize lexbuf }
| _    { comment lexbuf }