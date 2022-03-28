(* Abstract Syntax Tree and Print Functions*)

type op = Modulo | Greater | Less | Add | Sub | Mul 
| Div | Fdiv | Exp | Equal | Leq | Geq | Neq | And | Or 
| Band | Bor | Bxor | Ls | Rs | Bnot 
(* Fdiv: floor division. 3 // 2 = 1 *)
(* Exp: exponent. 2 ** 3 = 8 *)
(* And: logical and. True And False = False *)
(* Or: logical or. *)
(* Band: bitwise and. 3 & 2 = 2 *)
(* Bor: bitwise or. *)
(* Bxor: bitwise xor. *)
(* Ls: bitwise left shift. 1 << 2 = 4 *)
(* Rs: bitwise right shift. *) 
(* Bnot: negate. ~0 = -1 *)

type typ = Int | Bool | Str | Float

type expr = 
    Assign of string * expr
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | FloatLit of float 
  | Id of string
  | Binop of expr * op * expr 
  | Call of string of expr list
  | Cmd of string (* variable support '$x' to be implemented *)

type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt (* if expr {stmt} else {stmt} *)
  | While of expr * stmt (* while expr {stmt} *)
  | For of expr * expr * stmt (* for id in id {stmt} *)
  | Return of expr

type bind = string * typ (* In function definition, def foo(var : type) *)

type func_def = {
    fname: string;
    formals: bind list;
    body: stmt list;
} (* def fname(formals) {body} *)

type program = func_def list * stmt list

(* print functions *)

let string_of_op = function
    Modulo -> "%"
  | Greater -> ">"
  | Less -> "<" 
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*" 
  | Div -> "/"
  | Fdiv -> "//"
  | Exp -> "**"
  | Equal -> "=="
  | Leq -> "<="
  | Geq -> ">="
  | Neq -> "!="
  | And -> "and" 
  | Or -> "or"
  | Band -> "&" 
  | Bor -> "|"
  | Bxor -> "^" 
  | Ls -> "<<" 
  | Rs -> ">>" 
  | Bnot -> "~"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"

let string_of_expr = function
    Assign(v, e) -> v ^ " = " string_of_expr e
  | IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> s
  | FloatLit(f) -> string_of_float f
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call(f, el) -> 
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Cmd(c) -> c 

let string_of_stmt = function     
    Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if " ^ string_of_expr e ^ "\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while " ^ string_of_expr e ^ " " ^ string_of_stmt s
  | For(e1, e2, s) -> 
    "for " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ " " ^ string_of_stmt s
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"

let string_of_fdecl fdecl = 
  "def " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map fst fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (funcs, stmts) =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)

