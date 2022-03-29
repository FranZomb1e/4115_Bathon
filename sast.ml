(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx = 
    SAssign of string * sexpr
  | SIntLit of int
  | SBoolLit of bool
  | SStrLit of string
  | SFloatLit of float
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SCall of string * sexpr list
  | SCmd of string (* TBD *)

type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sstmt
  | Return of sexpr

type sfunc_def = {
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = sfunc_def list * sstmt list

(* print functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SIntLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBootLit(false) -> "false"
      | SStrLit(s) -> s
      | SFloatLit(f) -> string_of_float f
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " string_of_sexpr e2
      | SUnop(u, e) -> string_of_uop u ^ string_of_sexpr e
      | SCall(f, el) ->
        f ^ "(" ^ String.concat "," (List.map string_of_sexpr el) ^ ")"    
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ "\n"
  | SIf(e, s1, s2) -> "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SFor(e1, e2, s) -> "for (" ^ string_of_sexpr e1 ^ " in " ^ string_of_sexpr e2 ^ ")\n" ^
                      string_of_sstmt s
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "\n"

let string_of_sfdecl fedcl =
  "def " ^ fedcl.sfname ^ "(" ^ String.concat "," (List.map fst fedcl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fedcl.sbody) ^ 
  "}\n"

let string_of_sprogram (funcs, stmts) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sstmt stmts)