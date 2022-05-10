(* Abstract Syntax Tree and Print Functions*)

type op = Modulo | Greater | Less | Add | Sub | Mul 
| Div | Fdiv | Exp | Equal | Leq | Geq | Neq | And | Or 
| Band | Bor | Bxor | Ls | Rs 
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

type uop = Neg | Not | Bnot 
(* Neg: negative -. -2 *)
(* Not: logical not. not True = False *)
(* Bnot: negate. ~0 = -1 *)


type typ = Int | Bool | Str | Float
         | List of typ

type expr = 
    Assign of string * expr
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | FloatLit of float 
  | Id of string
  | Binop of expr * op * expr 
  | Unop of uop * expr
  | Call of string * expr list
  | Cmd of string (* variable support '$x' to be implemented *)
  | Access of string * expr
  | AccessAssign of string * expr * expr

type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt (* if expr {stmt} else {stmt} *)
  | While of expr * stmt (* while expr {stmt} *)
  | For of expr * expr * stmt (* for id in id {stmt} *)
  | Return of expr
  | Empty

type bind = string * typ (* In function definition, def foo(var : type) *)

type func_def = {
    rtyp: typ;
    fname: string;
    formals: bind list;
    body: stmt list;
} (* def fname:rtyp(formals) {body} *)

type program = bind list * func_def list * stmt list

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

let string_of_uop = function
    Neg -> "-"
  | Not -> "not"
  | Bnot -> "~"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Str -> "string"
  | List(t) -> string_of_typ t ^ "[]"

let rec string_of_expr = function
    Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(s) -> s
  | FloatLit(f) -> string_of_float f
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(u, e) -> string_of_uop u ^ string_of_expr e
  | Call(f, el) -> 
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Cmd(c) -> "Command:" ^ c
  | Access(e, i) -> e ^ "[" ^ string_of_expr i ^ "]"
  | AccessAssign(v, i, e) -> v ^ "[" ^ string_of_expr i ^ "]" ^ " = " ^ string_of_expr e

let rec string_of_stmt = function     
    Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ "\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | For(e1, e2, s) -> 
    "for " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ " " ^ string_of_stmt s
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | Empty -> ""

let string_of_vdecl (id, t) = id ^ " : " ^ string_of_typ t ^ "\n"

let string_of_fdecl fdecl = 
  "def " ^ fdecl.fname ^ " : " ^ string_of_typ fdecl.rtyp ^ "(" ^ String.concat ", " (List.map fst fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs, stmts) =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)