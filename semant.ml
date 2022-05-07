(* Semantic checking for the Bathon compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each function, then check each statement *)

let check (globals, funcs, stmts) = 

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (string * typ) list) =
    let rec dups = function
        [] -> ()
      | ((n1,_) :: (n2,_) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (a,_) (b,_) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies
     current version with print function where output is only "Hello World" *)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [("x", Int)];
      locals = [];
      body = []
    } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function name " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname
    in match fd with
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in 

  (* Collect all function names into one symbol table *)
  let main_func = {
      rtyp = Int;
      fname = "__main__";
      formals = [];
      locals = [];
      body = stmts
    }
  in

  let functions = main_func :: funcs
  in

  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_func func = 
    (* Make sure no formals are void or duplicates *)
    check_binds "formal" func.formals;
    
    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (name, ty) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier sTable s =
      try StringMap.find s sTable
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Assign may update symbol table *)
    let check_assign sT var rvaluet ex =
      if StringMap.mem var sT 
      then 
        let vty = type_of_identifier sT var in
        if vty = rvaluet
        then
          sT
        else
        let err = "illegal assignment " ^ string_of_typ vty ^ " = " ^
                  string_of_typ rvaluet ^ " in " ^ string_of_expr ex
        in
        raise (Failure err)
      else
        StringMap.add var rvaluet sT
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr sT expr =
      match expr with
      | IntLit l -> (sT, (Int, SIntLit l))
      | BoolLit l -> (sT, (Bool, SBoolLit l))
      | StrLit s -> (sT, (Str, SStrLit s))
      | FloatLit f -> (sT, (Float, SFloatLit f))
      | Id var -> (sT, (type_of_identifier sT var, SId var))
      | Assign(var, e) as ex ->
        let (sT1, (rt, e')) = check_expr sT e in
        let sT2 = check_assign sT1 var rt ex in
        (sT2, (rt, SAssign(var, (rt, e')))) 
      | Binop(e1, op, e2) as e ->
        let (sT1, (t1, e1')) = check_expr sT e1 in
        let (sT2, (t2, e2')) = check_expr sT1 e2 in
        let err = "illegal binary operator " ^ 
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* version 0.1 -> All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Mul | Div | Fdiv | Exp when t1 = Int -> Int 
            | Add | Sub | Mul | Div | Fdiv | Exp when t1 = Float -> Float
            | Band | Bor | Bxor | Ls | Rs when t1 = Int -> Int
            | Equal | Neq | Leq | Geq | Greater | Less -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (sT2, (t, SBinop((t1, e1'), op, (t2, e2'))))
        else raise (Failure err)
      | Unop(op, e) as ue ->
        let (sT1, (t, e')) = check_expr sT e in
        let err = "illegal unary operator " ^
                  string_of_typ t ^ " " ^ string_of_uop op ^ " in " ^
                  string_of_expr ue
        in let ot = match op with
            Neg when t = Float -> Float
          | Neg | Bnot when t = Int -> Int
          | Not when t = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (sT1, (ot, SUnop(op, (t, e'))))
      | Call(fname, args) as call ->
        (* Return a function from our symbol table *)
        let fd = find_func fname in 
        let param_length = List.length fd.formals in 
        if List.length args != param_length then 
          raise (Failure ("expecting " ^ string_of_int param_length ^ 
                          " arguments in " ^ string_of_expr call))
        else 
          (* Raise an exception if the given rvalue type cannot be assigned to
          the given lvalue type *)
          let check_assign_formals lvaluet rvaluet err =
            if lvaluet = rvaluet then lvaluet else raise (Failure err)
          in

          let rec check_call sT formals args = 
          match formals with
          | [] -> (sT, [])
          | (_, ft) :: bl -> 
            match args with
            | [] -> (sT, [])
            | e :: el -> 
              let (sT1 ,(et, e')) = check_expr sT e in
              let err = "illegal argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              let (sT_star, xs) = check_call sT1 bl el
              in (sT_star, (check_assign_formals ft et err, e') :: xs)
          in

          let (sT1, args') = check_call sT fd.formals args
          in (sT1, (fd.rtyp, SCall(fname, args')))
      | Cmd(content) -> (sT, (Str, SCmd(content)))
    in

    let check_bool_expr sT e = 
      let (sT1, (t, e')) = check_expr sT e in 
      match t with
      | Bool -> (sT1, (t, e'))
      | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list sT sl =
      match sl with
      | [] -> (sT, [])
      | Block sl :: sl' -> check_stmt_list sT (sl @ sl') (* FIXME: Flatten blocks *)
      | s :: sl -> 
        let (sT1, head) = check_stmt sT s in
        let (sT_star, rest) = check_stmt_list sT1 sl in
        (sT_star, head :: rest)
        (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt sT stmt =
      match stmt with
      (* A block is correct if each statement is correct and nothing
      follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
        let (sT1, sast) = check_stmt_list sT sl in
        (sT1, SBlock (sast))
      | Expr e -> 
        let (sT1, sast) = check_expr sT e in
          (sT1, SExpr (sast))
      | If(e, st1, st2) ->
        let (sT1, sast1) = check_bool_expr sT e in
        let (sT2, sast2) = check_stmt sT1 st1 in
        let (sT3, sast3) = check_stmt sT2 st2 in
        (sT3, SIf(sast1, sast2, sast3))
      | While(e, st) ->
        let (sT1, sast1) = check_bool_expr sT e in
        let (sT2, sast2) = check_stmt sT1 st in
        (sT2, SWhile(sast1, sast2))
      | For(e1, e2, st) -> 
        let (sT1, sast1) = check_expr sT e1 in
        let (sT2, sast2) = check_expr sT1 e2 in
        let (sT3, sast3) = check_stmt sT2 st in
        (sT3, SFor(sast1, sast2, sast3))
      | Return e ->
        let (sT1, (t, e')) = check_expr sT e in 
        if t = func.rtyp then (sT1, SReturn (t, e'))
        else raise (Failure("return gives " ^ string_of_typ t ^ " expected " ^
                            string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      | Empty -> (sT, SEmpty)
    in

    let check_res = check_stmt_list symbols func.body 
    in

    let slocals =
      let finalST = fst check_res
      in
      let rec remove_formals sT (binds : (string * typ) list) =
        match binds with
        | [] -> sT
        | (n, p) :: bl -> remove_formals (StringMap.remove n sT) bl
      in
      StringMap.bindings (remove_formals finalST (StringMap.bindings symbols))
    in
    {
      srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals = slocals;
      sbody = snd check_res;
    }
  in

  let sasts = List.map check_func functions
  in
  let main = List.hd sasts
  in
  (globals, sasts, main.sbody)
