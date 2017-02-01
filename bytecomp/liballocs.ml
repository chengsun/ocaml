[@@@ocaml.warning "-32-33-37-39-27"]

open Asttypes
open Primitive
open Format
open Types
open Lambda

module C = struct
  type ctype =
    | C_TODO
    | C_Boxed
    | C_Void
    | C_Int
    | C_UInt
    | C_Bool
    | C_Char
    | C_Struct of Ident.t * (ctype * Ident.t) list

  type expression =
    | C_InlineRevStatements of statement list (* putting a statement where an expression belongs; this needs to be extracted in a later pass *)
    | C_IntLiteral of int
    | C_PointerLiteral of int
    | C_Variable of Ident.t
    | C_ArrayIndex of expression * int
    | C_Cast of ctype * expression
    | C_BinaryOp of string * expression * expression
    | C_FunCall of Ident.t * expression list
    | C_Allocate of int (* number of words *)

  and statement =
    | C_Expression of expression
    | C_VarDeclare of ctype * expression * expression option
    | C_Assign of expression * expression
    | C_If of expression * statement list * statement list
    | C_Return of expression option

  type toplevel =
    | C_GlobalDefn of ctype * expression * expression option
    | C_FunDefn of ctype * expression * (ctype * Ident.t) list * statement list
    | C_StaticConstructor of int * statement list

  let rec map_toplevel f t =
    match t with
    | C_GlobalDefn _ -> t
    | C_FunDefn (ct,e,args,sl) -> C_FunDefn (ct,e,args,f sl)
    | C_StaticConstructor (id,sl) -> C_StaticConstructor (id,f sl)


  let ctype_to_string = function
    | C_TODO -> "/*TODO*/void*"
    | C_Boxed -> "void*"
    | C_Void -> "void"
    | C_Int -> "intptr_t"
    | C_UInt -> "unsigned"
    | C_Bool -> "bool"
    | C_Char -> "char"
    | C_Struct (id, _) -> "struct " ^ (Ident.unique_name id)


  let map_intersperse_concat f sep xs =
    let rec loop accum = function
      | [] -> accum
      | x::xs -> loop (accum ^ sep ^ (f x)) xs
    in
    match xs with
    | [] -> ""
    | x::xs -> loop (f x) xs


  let rec expression_to_string = function
    | C_InlineRevStatements sl -> Printf.sprintf "/*FIXME:inline*/{\n%s\n}" (rev_statements_to_string sl)
    | C_IntLiteral i -> string_of_int i
    | C_PointerLiteral i -> Printf.sprintf "((void*)%d)" i
    | C_Variable id -> Ident.unique_name id
    | C_ArrayIndex (e,i) -> Printf.sprintf "%s[%d]" (expression_to_string e) i
    | C_Cast (ty,e) -> Printf.sprintf "(%s)%s" (ctype_to_string ty) (expression_to_string e)
    | C_BinaryOp (op,x,y) -> "(" ^ (expression_to_string x) ^ op ^ (expression_to_string y) ^ ")"
    | C_FunCall (id,es) ->
        (Ident.unique_name id) ^ "(" ^ (map_intersperse_concat expression_to_string ", " es) ^ ")"
    | C_Allocate n -> Printf.sprintf "malloc(sizeof(intptr_t)*%d)" n

  and statement_to_string = function
    | C_Expression e -> (expression_to_string e) ^ ";"
    | C_VarDeclare (t,eid,e_option) ->
        (ctype_to_string t) ^ " " ^ (expression_to_string eid) ^
        (match e_option with
         | None -> ""
         | Some e -> " = " ^ (expression_to_string e)
        ) ^ ";"
    | C_Assign (eid,e) -> (expression_to_string eid) ^ " = " ^ (expression_to_string e) ^ ";"
    | C_If (e,ts,fs) ->
        "if (" ^ (expression_to_string e) ^ ") {\n" ^
        (map_intersperse_concat statement_to_string "\n" ts) ^ "\n} else {\n" ^
        (map_intersperse_concat statement_to_string "\n" fs) ^ "\n}"
    | C_Return (Some e) -> "return " ^ (expression_to_string e) ^ ";"
    | C_Return None -> "return;"

  and rev_statements_to_string sl = map_intersperse_concat statement_to_string "\n" (List.rev sl)

  let toplevel_to_string = function
    | C_GlobalDefn (t,id,e_option) ->
        (ctype_to_string t) ^ " " ^ (expression_to_string id) ^
        (match e_option with
         | None -> ""
         | Some e -> " = " ^ (expression_to_string e)
        ) ^ ";"
    | C_FunDefn (t,id,args,xs) ->
        (ctype_to_string t) ^ " " ^ (expression_to_string id) ^ "(" ^
        (map_intersperse_concat (fun (t,id) -> (ctype_to_string t) ^ " " ^ (Ident.unique_name id)) ", " args) ^ ") {\n" ^
        (rev_statements_to_string xs) ^ "\n}"
    | C_StaticConstructor (int_id, xs) ->
        "__attribute__((constructor)) void __static_constructor_" ^ (string_of_int int_id) ^ "() {\n" ^
        (rev_statements_to_string xs) ^ "\n}"
end

module Emitcode = struct
  let to_file oc _modulename _filename c_code =
      List.iter (fun t ->
          output_string oc (C.toplevel_to_string t);
          output_string oc "\n\n";
      ) c_code
end

open C

let formats f x =
  let b = Buffer.create 16 in
  let fo = formatter_of_buffer b in
  f fo x;
  pp_print_flush fo ();
  Buffer.contents b

let dumps_lambda lam =
  formats Printlambda.lambda lam

let dumps_env env =
  Env.fold_values (fun s p vd accum ->
      Printf.sprintf "%s\n%s : %s" accum s (formats Printtyp.type_expr vd.val_type)) None env ""

let rec structured_constant_to_expression = function
  | Const_base (Const_int n) -> C_IntLiteral n
  | Const_pointer n -> C_PointerLiteral n
  | Const_block (tag, scl) ->
      let id = Ident.create "__structured_constant" in
      let rec construct k statements = function
        | [] -> (C_Expression (C_Variable id)) :: statements
        | sc_head::scl ->
            let s = structured_constant_to_expression sc_head in
            construct (succ k) ((C_Assign (C_ArrayIndex (C_Variable id,k),s))::statements) scl
      in
      C_InlineRevStatements (construct 0 [
          C_VarDeclare (C_Boxed, C_Variable id,
                        Some (C_Allocate (List.length scl)))
        ] scl)
  | _ -> failwith "constant_to_expression: unknown constant type"

let rec lambda_to_expression env lam =
  match lam with
  | Levent (body, ev) ->
      lambda_to_expression (Envaux.env_from_summary ev.lev_env Subst.identity) body
  | Lprim (prim, largs) ->
      begin
        let bop op e1 e2 = C_BinaryOp (op, lambda_to_expression env e1, lambda_to_expression env e2) in
        let int_bop op e1 e2 = C_BinaryOp (op,
                                           C_Cast (C_Int, lambda_to_expression env e1),
                                           C_Cast (C_Int, lambda_to_expression env e2)) in
        match prim, largs with
        | Pmakeblock (tag, mut), contents ->
            let id = Ident.create "__makeblock" in
            let rec construct k statements = function (* TODO: this construct is almost identical to that in structured_constant_to_expression::Const_block *)
              | [] -> (C_Expression (C_Variable id)) :: statements
              | e_head::el ->
                  let s = lambda_to_expression env e_head in
                  construct (succ k) ((C_Assign (C_ArrayIndex (C_Variable id,k),s))::statements) el
            in
            C_InlineRevStatements (construct 0 [
                C_VarDeclare (C_Boxed, C_Variable id,
                              Some (C_Allocate (List.length contents)))
              ] contents)
        | Psequand, [e1;e2] -> bop "&&" e1 e2
        | Psequor, [e1;e2] -> bop "||" e1 e2
        | Paddint, [e1;e2] -> int_bop "+" e1 e2
        | Psubint, [e1;e2] -> int_bop "-" e1 e2
        | Pmulint, [e1;e2] -> int_bop "*" e1 e2
        | Pdivint, [e1;e2] -> int_bop "/" e1 e2
        | Pmodint, [e1;e2] -> int_bop "%" e1 e2
        | Pandint, [e1;e2] -> int_bop "&" e1 e2
        | Porint, [e1;e2] -> int_bop "|" e1 e2
        | Pxorint, [e1;e2] -> int_bop "^" e1 e2
        | Plslint, [e1;e2] -> int_bop "<<" e1 e2
        | Pintcomp(Ceq), [e1;e2] -> int_bop "==" e1 e2
        | Pintcomp(Cneq), [e1;e2] -> int_bop "!=" e1 e2
        | Pintcomp(Clt), [e1;e2] -> int_bop "<" e1 e2
        | Pintcomp(Cle), [e1;e2] -> int_bop "<=" e1 e2
        | Pintcomp(Cgt), [e1;e2] -> int_bop ">" e1 e2
        | Pintcomp(Cge), [e1;e2] -> int_bop ">=" e1 e2
        | _ -> failwith ("lambda_to_expression Lprim " ^ (Printlambda.name_of_primitive prim))
      end
  | Lconst sc -> structured_constant_to_expression sc (* TODO: share constants, and construct at compile time *)
  | Lvar id -> C_Variable id
  | Lapply { ap_func = Lvar id ; ap_args } ->
      C_FunCall (id, List.map (lambda_to_expression env) ap_args)
        (* problematic
  | Lifthenelse _ ->
      C_InlineRevStatements (lambda_to_rev_statements env lam)
        *)
  | _ -> failwith ("lambda_to_expression " ^ (dumps_lambda lam))

and lambda_to_rev_statements env lam =
  match lam with
  | Levent (body, ev) ->
      lambda_to_rev_statements (Envaux.env_from_summary ev.lev_env Subst.identity) body
  | Lvar _ | Lconst _ | Lprim _ | Lapply _ ->
      [C_Expression (lambda_to_expression env lam)]
  | Lsequence (l1, l2) ->
      (lambda_to_rev_statements env l2) @ (lambda_to_rev_statements env l1)
  | Lifthenelse (l,lt,lf) ->
      [C_If (lambda_to_expression env l,
             lambda_to_rev_statements env lt,
             lambda_to_rev_statements env lf)]
  | lam -> failwith ("lambda_to_rev_statements " ^ (dumps_lambda lam))

let id_staticconstructor = ref 0

let rec let_args_to_toplevels env id lam =
  match lam with
  | Levent (body, ev) ->
      let_args_to_toplevels (Envaux.env_from_summary ev.lev_env Subst.identity) id body
  | Lfunction { params ; body } ->
    Printf.printf "Got a fun LET %s\n%!" (expression_to_string id);
      let typedparams = List.map (fun id ->
        (*ENV
        Printf.printf "We have these in env: %s\n%!" (dumps_env env);
        Printf.printf "Looking up type of %s\n%!" (Ident.unique_name id);
          let (p, ty) = Env.lookup_value (Longident.Lident (Ident.name id)) env in
          C_Boxed ty.val_type, id
        *)
          C_Boxed, id
        ) params in
      let cbody_rev =
        match lambda_to_rev_statements env body with
        | [] -> failwith "let_args_to_toplevels: empty function?"
        | (C_Expression laste)::bodytl -> (C_Return (Some laste))::bodytl
        | cbody_rev ->
            (* TODO: check return type is void *)
            cbody_rev
      in
      [C_FunDefn (C_Boxed, id, typedparams, cbody_rev)]
  | _ ->
      let cbody_rev =
        match lambda_to_rev_statements env lam with
        | [] -> failwith "let_args_to_toplevels: empty let body?"
        | (C_Expression laste)::bodytl -> (C_Assign (id, laste))::bodytl
        | cbody_rev ->
            (* TODO: check return type is void *)
            cbody_rev
      in
      let ret =
        [ C_GlobalDefn (C_TODO, id, None)
        ; C_StaticConstructor (!id_staticconstructor, cbody_rev)]
      in
      id_staticconstructor := succ !id_staticconstructor; ret

let rec lambda_to_toplevels env lam =
  match lam with
  | Levent (body, ev) ->
      lambda_to_toplevels (Envaux.env_from_summary ev.lev_env Subst.identity) body
  | Llet (_strict, id, args, body) ->
    Printf.printf "Got a LET %s\n%!" (Ident.unique_name id);
      (let_args_to_toplevels env (C_Variable id) args) @ (lambda_to_toplevels env body)
  | Lprim (Pmakeblock(tag, Immutable), largs) ->
    Printf.printf "WARNING: ignoring an immutable makeblock at the toplevel: %s\n%!" (dumps_lambda lam);
    []
  | _ -> failwith ("lambda_to_toplevels " ^ (dumps_lambda lam))



let rec fixup_expression accum e = (* sticks inlined statements on the front of accum *)
  match e with
  | C_InlineRevStatements [] -> failwith "fixup_expression: invalid inlinerevstatements (empty)"
  | C_InlineRevStatements (C_Expression e::sl) ->
      let (accum', e') = fixup_expression accum e in
      (fixup_rev_statements accum' sl), e'
  | C_InlineRevStatements _ -> failwith "fixup_expression: invalid inlinerevstatements (last statement not a value expression)"
  | C_IntLiteral _ | C_PointerLiteral _ | C_Variable _
  | C_Allocate _ -> accum, e
  | C_ArrayIndex (e,i) -> let (accum', e') = fixup_expression accum e in accum', C_ArrayIndex (e',i)
  | C_Cast (ty,e) -> let (accum', e') = fixup_expression accum e in accum', C_Cast (ty,e')
  | C_BinaryOp (ty,e1,e2) ->
      let (accum', e1') = fixup_expression accum e1 in
      let (accum'', e2') = fixup_expression accum' e2 in
      accum'', C_BinaryOp (ty,e1',e2')
  | C_FunCall (id,es) ->
      let rec loop accum es' = function
        | [] -> accum, List.rev es'
        | e::es ->
            let (accum', e') = fixup_expression accum e in
            loop accum' (e'::es') es
      in
      let (accum', es') = loop accum [] es in
      accum', C_FunCall (id,es')

and fixup_rev_statements accum sl = (* sticks fixed up sl on the front of accum *)
  let rec loop accum = function
    | [] -> accum
    | s::statements ->
        let accum'' =
          match s with
          | C_Expression e -> let (accum', e') = fixup_expression accum e in (C_Expression e') :: accum'
          | C_VarDeclare (ty,eid,None) -> C_VarDeclare (ty,eid,None) :: accum
          | C_VarDeclare (ty,eid,Some e) -> let (accum', e') = fixup_expression accum e in C_VarDeclare (ty,eid,Some e') :: accum'
          | C_Assign (eid,e) -> let (accum', e') = fixup_expression accum e in C_Assign (eid,e') :: accum'
          | C_If (e,slt,slf) -> let (accum', e') = fixup_expression accum e in (C_If (e', loop [] slt, loop [] slf)) :: accum'
          | C_Return (None) -> C_Return (None) :: accum
          | C_Return (Some e) -> let (accum', e') = fixup_expression accum e in (C_Return (Some e')) :: accum'
        in
        loop accum'' statements
  in
  loop accum (List.rev sl)


let compile_implementation modulename lambda =
  Printf.printf "intercepting %s\n%!" modulename;
  let toplevels =
    match lambda with
    | Lprim (Psetglobal id, lams) when Ident.name id = modulename ->
      List.concat (List.map (lambda_to_toplevels Env.empty) lams)
    | lam -> failwith ("compile_implementation unexpected root: " ^ (dumps_lambda lam))
  in
  let fixed_toplevels =
    List.map (map_toplevel (fixup_rev_statements [])) toplevels
  in
  fixed_toplevels

