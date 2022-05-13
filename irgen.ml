module L = Llvm
module A = Ast
open Sast

exception Error of string

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Bathon" in

  (* Get types from the context *)
  let i32_t      = L.i32_type     context
  and i8_t       = L.i8_type      context
  and i1_t       = L.i1_type      context 
  and double_t   = L.double_type  context
  and float_t    = L.float_type   context in 
  let str_t      = L.pointer_type i8_t in

  (* List type *)
  let define_struct_typ name lltypes =
    let struct_type = L.named_struct_type context name in
    L.struct_set_body struct_type lltypes false;
    L.pointer_type struct_type
  in

  let list_t = define_struct_typ "list" [| L.pointer_type (L.pointer_type i8_t); i32_t; i32_t; L.pointer_type i8_t |] in
  

  (* Return the LLVM type for a Bathon type *)
  let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.Float  -> float_t
    | A.Str -> str_t
    | A.List(_) -> list_t
  in

  (* let list_hashtb = Hashtbl.create 10 in *)

  let lvalue_of_typ t n =
    match t with
    | A.List(_) -> 
      (* Hashtbl.add list_hashtb n false; *)
      L.const_pointer_null (ltype_of_typ t)
      (* L.set_value_name "uninit" lvalue;
      lvalue *)
    | A.Int | A.Bool ->
      L.const_int (ltype_of_typ t) 0
    | A.Float ->
      L.const_float (ltype_of_typ t) 0.0
    | A.Str -> L.const_pointer_null (ltype_of_typ t)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (n, t) =
      let init = lvalue_of_typ t n
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
    
  
  (* BUILT-IN FUNCTION *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| str_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
  
  let exec_t : L.lltype =
    L.var_arg_function_type (str_t) [| str_t |] in
  let exec_func : L.llvalue =
    L.declare_function "exec" exec_t the_module in
  
  (* Create an empty list given a type *)
  let create_list_t : L.lltype =
    L.function_type list_t [| str_t |] in
  let create_list_func : L.llvalue =
    L.declare_function "create_list" create_list_t the_module in

  (* Length of List *)
  let list_len_t : L.lltype = 
    L.function_type (ltype_of_typ A.Int) [| list_t |] in
  let list_len_func : L.llvalue =
    L.declare_function "list_len" list_len_t the_module in
  
  (* Access List *)
  let access_params_t = [| list_t; (ltype_of_typ A.Int) |] in
  (* int[] access via index *)
  let access_int_t : L.lltype =
    L.function_type (ltype_of_typ A.Int) access_params_t in
  let access_int_func : L.llvalue =
    L.declare_function "access_int" access_int_t the_module in
  (* float[] access via index *)
  let access_float_t : L.lltype =
    L.function_type (ltype_of_typ A.Float) access_params_t in
  let access_float_func : L.llvalue =
    L.declare_function "access_float" access_float_t the_module in
  (* str[] access via index *)
  let access_str_t : L.lltype = 
    L.function_type (ltype_of_typ A.Str) access_params_t in
  let access_str_func : L.llvalue = 
    L.declare_function "access_str" access_str_t the_module in
  
  (* Append List *)
  (* append int[] *)
  let append_int_t : L.lltype =
    L.function_type (ltype_of_typ A.Int) [| list_t; (ltype_of_typ A.Int) |] in
  let append_int_func : L.llvalue =
    L.declare_function "append_int" append_int_t the_module in
  (* append float[] *)
  let append_float_t : L.lltype =
    L.function_type (ltype_of_typ A.Int) [| list_t; (ltype_of_typ A.Float) |] in
  let append_float_func : L.llvalue =
    L.declare_function "append_float" append_float_t the_module in
  (* append str[] *)
  let append_str_t : L.lltype =
    L.function_type (ltype_of_typ A.Int) [| list_t; (ltype_of_typ A.Str) |] in
  let append_str_func : L.llvalue =
    L.declare_function "append_str" append_str_t the_module in
  
  (* Assign List *)
  (* assign int[] via index *)
  let assign_int_t : L.lltype = 
    L.function_type (ltype_of_typ A.Int) [| list_t; (ltype_of_typ A.Int); (ltype_of_typ A.Int) |] in
  let assign_int_func : L.llvalue = 
    L.declare_function "assign_int" assign_int_t the_module in
  (* assign float[] via index *)
  let assign_float_t : L.lltype = 
    L.function_type (ltype_of_typ A.Float) [| list_t; (ltype_of_typ A.Int); (ltype_of_typ A.Float) |] in
  let assign_float_func : L.llvalue =
    L.declare_function "assign_float" assign_float_t the_module in
  (* assign str[] via index *)
  let assign_str_t : L.lltype =
    L.function_type (ltype_of_typ A.Str) [| list_t; (ltype_of_typ A.Int); (ltype_of_typ A.Str) |] in
  let assign_str_func : L.llvalue =
    L.declare_function "assign_str" assign_str_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in


    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (n, t) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (n, t) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let rec get_typ_string e_typ =
      match e_typ with
        A.Int -> "int"
      | A.Float -> "float"
      | A.Str -> "str"
      | A.List(t) -> get_typ_string t
      | _ -> raise (Error "get_typ_string the current type not supported")
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((e_t, e) : sexpr) = match e with
      | SCmd command -> 
        let command_ll = L.build_global_stringptr command "command" builder in
        L.build_call exec_func [| command_ll |] "exec" builder
      | SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit str -> L.build_global_stringptr str "tmp" builder
      | SFloatLit f -> L.const_float float_t f
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SUnop(u, e) ->
        let e' = build_expr builder e in
        (match u with
            A.Neg   -> L.build_neg
          | A.Not   -> L.build_not
          | A.Bnot  -> L.build_not
        ) e' "tmp" builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mul     -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Band    -> L.build_and
         | A.Bor     -> L.build_or
         | A.Bxor    -> L.build_xor
         | A.Ls      -> L.build_shl
         | A.Rs      -> L.build_ashr
         | A.Modulo  -> L.build_srem
         | _ -> raise(Error "Unreachable")
         
        ) e1' e2' "tmp" builder
      | SAccess (s, e) ->
        let idx = build_expr builder e in
        let li = L.build_load (lookup s) s builder in
        let access_func = 
          match e_t with
            A.Int -> access_int_func
          | A.Float -> access_float_func
          | A.Str -> access_str_func
          | _ -> raise(Error "access type not supported")
        in
        L.build_call access_func [| li; idx |] "access_typ" builder
      | SAccessAssign(s, e1, e2) ->
        let idx = build_expr builder e1 in
        let li = L.build_load (lookup s) s builder in
        let value = build_expr builder e2 in
        let assign_func =
          match e_t with
            A.Int -> assign_int_func
          | A.Float -> assign_float_func
          | A.Str -> assign_str_func
          | _ -> raise(Error "access assign type not supported")
        in
        L.build_call assign_func [| li; idx; value |] "assign_typ" builder
      | SCall ("print", [ ((t, v) as e) ]) ->
        let print_value = 
          match t with
            A.Float -> L.build_fpext (build_expr builder e) double_t "ext" builder
          | _ -> build_expr builder e
        in
        let format_str = 
          match t with
            A.Int -> int_format_str
          | A.Float -> float_format_str
          | A.Bool -> int_format_str
          | A.Str -> str_format_str
          | _ -> raise(Error "print type format not supported")
        in
        L.build_call printf_func [| format_str ; print_value |] "printf" builder
      | SCall ("append", e_list) -> 
        let (typ, sx) as li_e = List.hd e_list in
        let p_e = List.nth e_list 1 in
        if (get_typ_string typ) != (get_typ_string (fst p_e)) then
          raise(Error "list type is inconsistent with that of the append value")
        else
          let p = build_expr builder p_e in
          let li = build_expr builder li_e in
          let append_func = 
            match p_e with
                  (A.Int, _) -> append_int_func
                | (A.Float, _) -> append_float_func
                | (A.Str, _) -> append_str_func
                | _ -> raise(Error "append parameter type not supported")
          in
          L.build_call append_func [| li; p |] "" builder
      | SCall ("init", [ (t, sx) ]) ->
        let typ_string_ptr = 
          (match t with
          | A.List(_) -> 
            let typ_string = (get_typ_string t) in
            build_expr builder (A.Str, SStrLit(typ_string))
          | _ -> raise(Error "init function only supports list initialization")) in
        let li_lvalue = L.build_call create_list_func [| typ_string_ptr |] "create_list" builder in
        let p_lvalue = 
          (match sx with
          | SId(s) -> 
            lookup s
          | _ -> raise(Error "init unexpected error")) in
        L.build_store li_lvalue p_lvalue builder
      | SCall ("len", [ (t, _) as e ]) ->
        let li = 
          (match t with
            A.List(_) -> build_expr builder e
          | _ -> raise(Error "len function only supports list")) in
        L.build_call list_len_func [| li |] "len" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SEmpty -> builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      | _ -> raise(Error "Unreachable")

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module