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
    | C_IntLiteral of int
    | C_PointerLiteral of int
    | C_Variable of Ident.t
    | C_Cast of ctype * expression
    | C_BinaryOp of string * expression * expression
    | C_FunCall of Ident.t * expression list
    | C_Allocate of int (* number of words *)

  type statement =
    | C_Expression of expression
    | C_VarDeclare of ctype * Ident.t * expression option
    | C_Assign of Ident.t * expression
    | C_If of expression * statement list * statement list option
    | C_Return of expression option

  type toplevel =
    | C_GlobalDefn of ctype * Ident.t * expression option
    | C_FunDefn of ctype * Ident.t * (ctype * Ident.t) list * statement list
    | C_StaticConstructor of int * statement list

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
    | C_IntLiteral i -> string_of_int i
    | C_PointerLiteral i -> Printf.sprintf "((void*)%d)" i
    | C_Variable id -> Ident.unique_name id
    | C_Cast (ty,e) -> Printf.sprintf "(%s)%s" (ctype_to_string ty) (expression_to_string e)
    | C_BinaryOp (op,x,y) -> "(" ^ (expression_to_string x) ^ op ^ (expression_to_string y) ^ ")"
    | C_FunCall (id,es) ->
        (Ident.unique_name id) ^ "(" ^ (map_intersperse_concat expression_to_string ", " es) ^ ")"
    | C_Allocate n -> Printf.sprintf "malloc(sizeof(intptr_t)*%d)" n

  let rec statement_to_string = function
    | C_Expression e -> (expression_to_string e) ^ ";"
    | C_VarDeclare (t,id,e_option) ->
        (ctype_to_string t) ^ " " ^ (Ident.unique_name id) ^
        (match e_option with
         | None -> ""
         | Some e -> " = " ^ (expression_to_string e)
        ) ^ ";"
    | C_Assign (id,e) -> (Ident.unique_name id) ^ " = " ^ (expression_to_string e) ^ ";"
    | C_If (e,ts,fs_option) ->
        "if (" ^ (expression_to_string e) ^ ") {\n" ^
        (map_intersperse_concat statement_to_string "\n" ts) ^ "\n}" ^
        (match fs_option with
         | None -> ""
         | Some fs -> " else {\n" ^
                      (map_intersperse_concat statement_to_string "\n" fs) ^ "\n}"
        )
    | C_Return (Some e) -> "return " ^ (expression_to_string e) ^ ";"
    | C_Return None -> "return;"

  let toplevel_to_string = function
    | C_GlobalDefn (t,id,e_option) ->
        (ctype_to_string t) ^ " " ^ (Ident.unique_name id) ^
        (match e_option with
         | None -> ""
         | Some e -> " = " ^ (expression_to_string e)
        ) ^ ";"
    | C_FunDefn (t,id,args,xs) ->
        (ctype_to_string t) ^ " " ^ (Ident.unique_name id) ^ "(" ^
        (map_intersperse_concat (fun (t,id) -> (ctype_to_string t) ^ " " ^ (Ident.unique_name id)) ", " args) ^ ") {\n" ^
        (map_intersperse_concat statement_to_string "\n" xs) ^ "\n}"
    | C_StaticConstructor (int_id, xs) ->
        "__attribute__((constructor)) void __static_constructor_" ^ (string_of_int int_id) ^ "() {\n" ^
        (map_intersperse_concat statement_to_string "\n" xs) ^ "\n}"
end

module Emitcode = struct
  let to_file oc _modulename _filename ~required_globals:_ c_code =
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

let rec lambda_to_expression = function
  | Levent (body, ev) ->
      lambda_to_expression (*Envaux.env_from_summary ev.lev_env Subst.identity*) body
  | Lprim (prim, largs, _loc) ->
      begin
        let bop op e1 e2 = C_BinaryOp (op, lambda_to_expression e1, lambda_to_expression e2) in
        let int_bop op e1 e2 = C_BinaryOp (op,
                                           C_Cast (C_Int, lambda_to_expression e1),
                                           C_Cast (C_Int, lambda_to_expression e2)) in
        match prim, largs with
        (*
        | Pmakeblock (tag, mut, Some ss), es ->
            C_Allocate (List.length ss) (* TODO: is this right? *)
           *)
        | Psequand, [e1;e2] -> bop "&&" e1 e2
        | Psequor, [e1;e2] -> bop "||" e1 e2
        | Paddint, [e1;e2] -> int_bop "+" e1 e2
        | Psubint, [e1;e2] -> int_bop "-" e1 e2
        | Pmulint, [e1;e2] -> int_bop "*" e1 e2
        | Pdivint Unsafe, [e1;e2] -> int_bop "/" e1 e2
        | Pmodint Unsafe, [e1;e2] -> int_bop "%" e1 e2
        | Pandint, [e1;e2] -> int_bop "&" e1 e2
        | Porint, [e1;e2] -> int_bop "|" e1 e2
        | Pxorint, [e1;e2] -> int_bop "^" e1 e2
        | Plslint, [e1;e2] -> int_bop "<<" e1 e2
        | Pintcomp(Ceq), [e1;e2] -> bop "==" e1 e2
        | Pintcomp(Cneq), [e1;e2] -> bop "!=" e1 e2
        | Pintcomp(Clt), [e1;e2] -> bop "<" e1 e2
        | Pintcomp(Cle), [e1;e2] -> bop "<=" e1 e2
        | Pintcomp(Cgt), [e1;e2] -> bop ">" e1 e2
        | Pintcomp(Cge), [e1;e2] -> bop ">=" e1 e2
        | _ -> failwith ("lambda_to_expression Lprim " ^ (Printlambda.name_of_primitive prim))
      end
  | Lconst (Const_base (Const_int n)) -> C_IntLiteral ((n lsl 1) lor 1)
  | Lconst (Const_pointer n) -> C_PointerLiteral n
  | Lvar id -> C_Variable id
  | Lapply { ap_func = Lvar id ; ap_args } ->
      C_FunCall (id, List.map lambda_to_expression ap_args)
  | lam -> failwith ("lambda_to_expression " ^ (dumps_lambda lam))

let rec lambda_to_rev_statements env lam =
  match lam with
  | Levent (body, ev) ->
      lambda_to_rev_statements (Envaux.env_from_summary ev.lev_env Subst.identity) body
  | Lvar _ | Lconst _ | Lprim _ | Lapply _ ->
      [C_Expression (lambda_to_expression lam)]
  | Lsequence (l1, l2) ->
      (lambda_to_rev_statements env l2) @ (lambda_to_rev_statements env l1)
  | lam -> failwith ("lambda_to_rev_statements " ^ (dumps_lambda lam))

let id_staticconstructor = ref 0

let rec let_args_to_toplevels env id lam =
  match lam with
  | Levent (body, ev) ->
      let_args_to_toplevels (Envaux.env_from_summary ev.lev_env Subst.identity) id body
  | Lvar _ | Lconst _ | Lprim _ ->
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
        ; C_StaticConstructor (!id_staticconstructor, List.rev cbody_rev)]
      in
      id_staticconstructor := succ !id_staticconstructor; ret
  | Lfunction { params ; body } ->
    Printf.printf "Got a fun LET %s\n%!" (Ident.unique_name id);
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
      [C_FunDefn (C_Boxed, id, typedparams, List.rev cbody_rev)]
  | lam -> failwith ("Llet " ^ (dumps_lambda lam))

let rec lambda_to_toplevels env lam =
  match lam with
  | Levent (body, ev) ->
      lambda_to_toplevels (Envaux.env_from_summary ev.lev_env Subst.identity) body
  | Llet (_strict, _kind, id, args, body) ->
    Printf.printf "Got a LET %s\n%!" (Ident.unique_name id);
      (let_args_to_toplevels env id args) @ (lambda_to_toplevels env body)
  | Lprim (Pmakeblock(tag, Immutable, shape), largs, _loc) ->
    Printf.printf "WARNING: ignoring an immutable makeblock at the toplevel: %s\n%!" (dumps_lambda lam);
    []
  | _ -> failwith ("lambda_to_toplevels " ^ (dumps_lambda lam))

let compile_implementation modulename lambda =
  Printf.printf "intercepting %s\n%!" modulename;
  match lambda with
  | Lprim (Psetglobal id, lams, loc) when Ident.name id = modulename ->
    List.concat (List.map (lambda_to_toplevels Env.empty) lams)
  | lam -> failwith ("compile_implementation unexpected root: " ^ (dumps_lambda lam))

