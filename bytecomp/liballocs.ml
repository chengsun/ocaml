[@@@ocaml.warning "-32-33-37-39-27-26"]

open Asttypes
open Primitive
open Format
open Types
open Lambda

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


module C = struct
  type ctype =
    | C_CommentedType of ctype * string
    | C_Pointer of ctype
    | C_Boxed (* the union boxed_pointer_t *)
    | C_Void
    | C_Int
    | C_Double
    | C_UInt
    | C_Bool
    | C_Char
    | C_Struct of Ident.t * (ctype * Ident.t) list
    | C_FunPointer of ctype * ctype list
    | C_VarArgs

  type expression =
    | C_Blob of (string * expression * string)
    | C_InlineRevStatements of statement list (* putting a statement where an expression belongs; this needs to be extracted in a later pass *)
    | C_InlineFunDefn of expression * (ctype * Ident.t) list * statement list
    | C_IntLiteral of int
    | C_PointerLiteral of int
    | C_StringLiteral of string
    | C_CharLiteral of char
    | C_GlobalVariable of Ident.t
    | C_Variable of Ident.t
    | C_ArrayIndex of expression * int option
    | C_InitialiserList of expression list
    | C_Cast of ctype * expression
    | C_BinaryOp of string * expression * expression
    | C_FunCall of expression * expression list
    | C_Allocate of ctype * int(*multiplier*) * (Types.type_expr, string) result (* for debugging only *)

  and statement =
    | C_Expression of expression
    | C_VarDeclare of ctype * expression * expression option
    | C_Assign of expression * expression
    | C_If of expression * statement list * statement list (* reversed! *)
    | C_Return of expression option

  type toplevel =
    | C_TopLevelComment of string
    | C_TypeDefn of ctype (* e.g. defining a struct *)
    | C_GlobalDefn of ctype * expression * expression option
    | C_ExternDecl of ctype * expression
    | C_FunDefn of ctype * expression * (ctype * Ident.t) list * statement list option

  (* runs [f] on any statement lists in the toplevel *)
  let rec map_toplevel f t =
    match t with
    | C_TopLevelComment _ -> t
    | C_TypeDefn _ -> t
    | C_GlobalDefn _ -> t
    | C_ExternDecl _ -> t
    | C_FunDefn (ct,e,args,Some sl) -> C_FunDefn (ct,e,args,Some (f sl))
    | C_FunDefn _ -> t


  (* the type library is responsible for translating type_exprs to C types,
   * and keeping track of all newly created C types (structs), and the mapping
   * from type_exprs to these types *)
  module TypeLibrary = struct
    (* the payload of data we want with each type_expr *)
    type t = ctype

    module TypeHash = Hashtbl.Make(Types.TypeOps)
    (* TODO: store separate list in dependency order *)
    let g_table: t TypeHash.t = TypeHash.create 16

    let rec tfield_to_list type_expr =
      match type_expr.desc with
      | Tfield (name, Fpresent, t, ts) -> (name, t) :: (tfield_to_list ts)
      | Tnil -> []
      | _ -> failwith "unexpected type_desc type in Tobject field list"

    let rec add_structlike_mapping type_expr name_hint ts =
      let struct_name = Ident.create name_hint in
      let ctype =
        C_Struct (struct_name, (List.map (fun (fieldname, fieldtype) ->
          ocaml_to_c_type fieldtype, Ident.create fieldname) ts))
      in
      TypeHash.add g_table type_expr ctype;
      ctype

    and add_mapping type_expr =
      match type_expr.desc with
      | Ttuple ts ->
          add_structlike_mapping type_expr "tuple" (List.mapi (fun i t -> (Printf.sprintf "_%d" (i+1)), t) ts)
      | Tobject (ts, r) when !r = None ->
          add_structlike_mapping type_expr "struct" (tfield_to_list ts)
      | _ -> failwith "unexpected type_expr to add mapping for"

    and ocaml_to_c_type type_expr =
      match type_expr.desc with
      | Tvar _ -> C_Boxed
      | Tarrow _ -> C_FunPointer (C_Void, []) (* TODO: could add more detail *)
      | Tconstr (path, [], _) when path = Predef.path_int -> C_Int
      | Tconstr (path, [], _) when path = Predef.path_string -> C_Pointer C_Char
      | Tconstr (path, [], _) when path = Predef.path_float -> C_Double
      (* TODO: add more *)
      | Tconstr _ -> C_CommentedType (C_Boxed, "unknown Tconstr")
      | Ttuple _
      | Tobject _ ->
          (try TypeHash.find g_table type_expr
           with Not_found -> add_mapping type_expr)
      | Tfield _ | Tnil -> failwith "unexpected Tfield/Tnil at top level"
      | Tlink _e
      | Tsubst _e -> failwith "unexpected Tlink/Tsubst" (* ocaml_to_c_type e probably? *)
      | Tvariant _ -> failwith "really need to handle Tvariant" (* TODO *)
      | Tunivar _ | Tpoly _ | Tpackage _ -> failwith "type which i have no idea"


    (* get all struct declarations so far *)
    (* TODO: read from separate list in dependency order *)
    let dump_all_types_for_definition () =
      TypeHash.fold (fun _k v acc -> v::acc) g_table []
  end



  let rec assign_last_value_of_statement id sl = (* for extracting C_InlineRevStatements *)
    let f e = C_Assign (C_Variable id, e) in
    match sl with
    | [] -> failwith "assign_last_value_of_statement: empty sl" (* TODO: unit? *)
    | s::sl ->
        begin
          match s with
          | C_Expression e -> f e :: sl
          | C_VarDeclare _ -> failwith "assign_last_value_of_statement: unexpected C_VarDeclare as last statement in sl"
          | C_Assign (varid,e) -> f varid :: C_Assign (varid,e) :: sl
          | C_If (e,slt, slf) -> C_If (e, assign_last_value_of_statement id slt, assign_last_value_of_statement id slf) :: sl
          | C_Return _ -> failwith "assign_last_value_of_statement: unexpected C_Return as last statement in sl"
        end


  let map_intersperse_concat f sep xs =
    let rec loop accum = function
      | [] -> accum
      | x::xs -> loop (accum ^ sep ^ (f x)) xs
    in
    match xs with
    | [] -> ""
    | x::xs -> loop (f x) xs

  let rec ctype_to_string = function
    | C_CommentedType (cty, comment) -> Printf.sprintf "%s/*%s*/" (ctype_to_string cty) comment
    | C_Pointer cty -> (ctype_to_string cty) ^ "*"
    | C_Boxed -> "boxed_pointer_t"
    | C_Void -> "void"
    | C_Int -> "intptr_t"
    | C_Double -> "double"
    | C_UInt -> "unsigned"
    | C_Bool -> "bool"
    | C_Char -> "char"
    | C_Struct (id, _) -> "struct " ^ (Ident.unique_name id)
    | C_FunPointer (tyret, tyargs) -> Printf.sprintf "%s(*)(%s)" (ctype_to_string tyret) (map_intersperse_concat ctype_to_string "," tyargs)
    | C_VarArgs -> "..."

  let rec ctype_size_nwords = function
    | C_CommentedType (cty, _) -> ctype_size_nwords cty
    | C_Pointer _ | C_FunPointer _ | C_Boxed | C_Int | C_Double | C_UInt -> 1
    | C_Struct (_, tys) -> List.fold_left (fun acc (t,_) -> acc + (ctype_size_nwords t)) 0 tys
    | C_Bool | C_Char -> failwith "bools/chars have sub-word size"
    | C_VarArgs | C_Void -> failwith "invalid ctype to query size for"

  let cstruct_defn_string id fields =
    let fieldstrings =
      map_intersperse_concat (fun (ctype, fieldid) ->
        Printf.sprintf "%s %s;" (ctype_to_string ctype) (Ident.unique_name fieldid)
      ) "\n" fields
    in
    Printf.sprintf "struct %s {\n%s\n};\n" (Ident.unique_name id) fieldstrings

  let ctype_defn_string = function
    | C_Struct (id, fields) -> cstruct_defn_string id fields
    | _ -> failwith "unexpected attempt to define non-struct ctype"

  let string_split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r

  let rec expression_to_string = function
    | C_Blob (s1, e, s2) -> Printf.sprintf "%s%s%s" s1 (expression_to_string e) s2
    | C_InlineRevStatements sl -> Printf.sprintf "/*FIXME:inline*/{\n%s\n}" (rev_statements_to_string sl)
    | C_InlineFunDefn (name, args, sl) -> Printf.sprintf "/*FIXME:inline fun %s (%s)*/{\n%s\n}"
                              (expression_to_string name)
                              (map_intersperse_concat (fun (t,id) -> (ctype_to_string t) ^ " " ^ (Ident.unique_name id)) ", " args)
                              (rev_statements_to_string sl)
    | C_IntLiteral i -> string_of_int i
    | C_PointerLiteral i -> Printf.sprintf "((void*)%d)" i
    | C_StringLiteral str -> Printf.sprintf "%S" str
    | C_CharLiteral ch -> Printf.sprintf "%C" ch
    | C_GlobalVariable id -> Ident.name id
    | C_Variable id -> Ident.unique_name id
    | C_ArrayIndex (e,Some i) -> Printf.sprintf "%s[%d]" (expression_to_string e) i
    | C_ArrayIndex (e,None) -> (expression_to_string e) ^ "[]"
    | C_InitialiserList es -> Printf.sprintf "{%s}" (map_intersperse_concat expression_to_string ", " es)
    | C_Cast (ty,e) -> Printf.sprintf "((%s)%s)" (ctype_to_string ty) (expression_to_string e)
    | C_BinaryOp (op,x,y) -> "(" ^ (expression_to_string x) ^ op ^ (expression_to_string y) ^ ")"
    | C_FunCall (e_id,es) ->
        (expression_to_string e_id) ^ "(" ^ (map_intersperse_concat expression_to_string ", " es) ^ ")"
    | C_Allocate (ctype, n, tyinfo) -> begin
      let comment =
        match tyinfo with
        | Ok type_expr -> formats Printtyp.type_expr type_expr
        | Error s -> Printf.sprintf "NO TYPE INFO: %s" s
      in
      Printf.sprintf "malloc(sizeof(%s)*%d /*%s*/)" (ctype_to_string ctype) n comment
    end

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
        (map_intersperse_concat statement_to_string "\n" (List.rev ts)) ^ "\n} else {\n" ^
        (map_intersperse_concat statement_to_string "\n" (List.rev fs)) ^ "\n}"
    | C_Return (Some e) -> "return " ^ (expression_to_string e) ^ ";"
    | C_Return None -> "return;"

  and rev_statements_to_string sl = map_intersperse_concat statement_to_string "\n" (List.rev sl)

  let toplevel_to_string = function
    | C_TopLevelComment str -> "/*\n" ^ str ^ "\n*/"
    | C_TypeDefn t -> ctype_defn_string t
    | C_GlobalDefn (t,id,e_option) ->
        (ctype_to_string t) ^ " " ^ (expression_to_string id) ^
        (match e_option with
         | None -> ""
         | Some e -> " = " ^ (expression_to_string e)
        ) ^ ";"
    | C_ExternDecl (t,id) ->
        Printf.sprintf "extern %s %s;" (ctype_to_string t) (expression_to_string id)
    | C_FunDefn (t,id,args,xs_option) ->
        (ctype_to_string t) ^ " " ^ (expression_to_string id) ^ "(" ^
        (map_intersperse_concat (fun (t,id) -> (ctype_to_string t) ^ " " ^ (Ident.unique_name id)) ", " args) ^ ")" ^
        ( match xs_option with
          | None -> ";"
          | Some xs -> "{\n" ^ (rev_statements_to_string xs) ^ "\n}"
        )
end

module Emitcode = struct
  let to_file oc _modulename _filename c_code =
    output_string oc "#include <stdlib.h>\n#include <stdint.h>\n";
    output_string oc "#include \"liballocs.h\"\n";
    output_string oc "\n";
    List.iter (fun t ->
        output_string oc (C.toplevel_to_string t);
        output_string oc "\n\n";
      ) c_code
end

open C

let compile_implementation modulename lambda =
  let do_allocation nwords (type_expr_result : _ result) =
    let ctype, n =
      match type_expr_result with
      | Ok type_expr ->
          let ctype = TypeLibrary.ocaml_to_c_type type_expr in
          let ctype_size = ctype_size_nwords ctype in
          assert (ctype_size = nwords);
          ctype, 1
      | Error _ -> C_Boxed, nwords
    in
    C_Allocate(ctype, n, type_expr_result)
  in

  let make_boxed_d e =
      C_Cast (C_Boxed,
        (C_InitialiserList [ (* TODO: rewrite -- this is disgusting *)
          C_Blob (".d = ", e, "")
         ]))
  in
  (* TODO: rewrite -- this is disgusting *)
  let get_boxed_d e = C_Blob ("", e, ".d") in
  let get_boxed_f e = C_Blob ("", e, ".f") in

  let module_initialiser_name module_id = Ident.create ((Ident.name module_id) ^ "__init") in

  let globals = ref [] in

  let rec let_args_to_vardecl env id lam =
    match lam with
    | Levent (body, ev) ->
        let_args_to_vardecl (Envaux.env_from_summary ev.lev_env Subst.identity) id body
    | Lfunction { params ; body } ->
        Printf.printf "Got a expression fun LET %s\n%!" (expression_to_string id);
        let typedparams = List.map (fun id ->
            (*ENV
              Printf.printf "We have these in env: %s\n%!" (dumps_env env);
              Printf.printf "Looking up type of %s\n%!" (Ident.unique_name id);
              let (p, ty) = Env.lookup_value (Longident.Lident (Ident.name id)) env in
              C_Boxed ty.val_type, id
            *)
            C_Boxed, id
          ) params in
        let cbody =
          C_Return (Some (C_InlineRevStatements (lambda_to_rev_statements env body)))
        in
        C_Expression (C_InlineFunDefn (id, typedparams, [cbody]))
    | _ ->
        C_VarDeclare (C_Boxed, id, Some (lambda_to_expression env lam))


  (* Translates a structured constant into an expression of ctype boxed_pointer_t* *)
  and structured_constant_to_expression env = function
    | Const_base (Const_int n) -> C_IntLiteral n
    | Const_base (Const_char ch) -> C_CharLiteral ch
    | Const_base (Const_string (s, None)) -> C_StringLiteral s (* FIXME: MUTABLE STRING, SHOULD NOT BE SHARED/just a literal *)
    | Const_pointer n -> C_PointerLiteral n
    | Const_immstring str -> C_StringLiteral str (* immediate/immutable string *)
    | Const_block (tag, scl) ->
      lambda_to_expression env (Lprim
        (Pmakeblock (tag, Asttypes.Immutable, Error "structured_constant_to_expression"),
        List.map (fun x -> Lconst x) scl))
      (* we'll recurse back into this function eventually *)
    | _ -> failwith "structured_constant_to_expression: unknown constant type"

  and lambda_to_expression env lam : C.expression =
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
          | Popaque, [Lprim (Pgetglobal id, [])] ->
              (* assume these are declarations of use of external modules; ensure initialisation *)
              globals := id :: !globals;
              C_InlineRevStatements (
                [ C_Expression (C_GlobalVariable id)
                ; C_Expression (C_FunCall (C_GlobalVariable (module_initialiser_name id), []))])
          | Popaque, [lam] -> C_InlineRevStatements (lambda_to_rev_statements env lam)
          | Pgetglobal id, [] -> C_GlobalVariable id
          | Pfield i, [lam] -> C_ArrayIndex (lambda_to_expression env lam, Some i)
          | Pmakeblock (tag, mut, tyinfo), contents ->
              let id = Ident.create "__makeblock" in
              let var = C_Variable id in
              let rec construct k statements = function
                | [] -> (C_Expression var) :: statements
                | e_head::el ->
                    let s = lambda_to_expression env e_head in
                    let elem = C_ArrayIndex (var, Some k) in
                    let assign = C_Assign (elem,s) in
                    construct (succ k) (assign::statements) el
              in
              C_InlineRevStatements (construct 0 [
                  C_VarDeclare (C_Pointer C_Boxed, C_Variable id,
                                Some (do_allocation (List.length contents) tyinfo))
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
    | Lconst sc -> structured_constant_to_expression env sc
    | Lvar id -> C_Variable id
    | Lapply { ap_func = e_id ; ap_args } ->
        (* FIXME: hardcoded for printf *)
        let vararg =
          match e_id with
          | Lprim (Pfield _, [Lprim (Pgetglobal id, [])]) when Ident.name id = "Printf" -> true
          | _ -> false
        in
        let ty =
          if vararg
          then C_FunPointer (C_Boxed, [C_Boxed; C_VarArgs])
          else C_FunPointer (C_Boxed, List.map (fun _ -> C_Boxed) ap_args)
        in
        C_FunCall (C_Cast (ty, get_boxed_f (lambda_to_expression env e_id)),
                   List.map (lambda_to_expression env) ap_args)
    | Lifthenelse _ ->
        C_InlineRevStatements (lambda_to_rev_statements env lam)
    | _ -> failwith ("lambda_to_expression " ^ (dumps_lambda lam))

  and lambda_to_rev_statements env lam =
    match lam with
    | Levent (body, ev) ->
        lambda_to_rev_statements (Envaux.env_from_summary ev.lev_env Subst.identity) body
    | Lvar _ | Lconst _ | Lprim _ | Lapply _ ->
        [C_Expression (lambda_to_expression env lam)]
    | Llet (_strict, id, args, body) ->
        (lambda_to_rev_statements env body) @
        [let_args_to_vardecl env (C_Variable id) args]
    | Lletrec ([id, args], body) ->
        (lambda_to_rev_statements env body) @
        [let_args_to_vardecl env (C_Variable id) args]
    | Lsequence (l1, l2) ->
        (lambda_to_rev_statements env l2) @ (lambda_to_rev_statements env l1)
    | Lifthenelse (l,lt,lf) ->
        [C_If (lambda_to_expression env l,
               lambda_to_rev_statements env lt,
               lambda_to_rev_statements env lf)]
    | lam -> failwith ("lambda_to_rev_statements " ^ (dumps_lambda lam))
  in

  let static_constructors = ref [] in

  let rec let_args_to_toplevels env id lam = (* FIXME: get rid of this soon! *)
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
        let cbody =
          C_Return (Some (C_InlineRevStatements (lambda_to_rev_statements env body)))
        in
        [C_FunDefn (C_Boxed, id, typedparams, Some [cbody])]
    | _ ->
        let cbody =
          C_Assign (id, C_InlineRevStatements (lambda_to_rev_statements env lam))
        in
        static_constructors := cbody :: !static_constructors;
        [ C_GlobalDefn (C_Boxed, id, None) ]
  in

  let rec lambda_to_toplevels module_id env lam =
    match lam with
    | Levent (body, ev) ->
        lambda_to_toplevels module_id (Envaux.env_from_summary ev.lev_env Subst.identity) body
    | Llet (_strict, id, args, body) ->
        Printf.printf "Got a LET %s\n%!" (Ident.unique_name id);
        (let_args_to_toplevels env (C_Variable id) args) @ (lambda_to_toplevels module_id env body)
    | Lletrec ([id, args], body) ->
        Printf.printf "Got a LETREC %s\n%!" (Ident.unique_name id);
        (let_args_to_toplevels env (C_Variable id) args) @ (lambda_to_toplevels module_id env body)
    | Lsequence (l1, l2) -> (* TODO: dedup this... *)
        let cbody_rev = lambda_to_rev_statements env l1 in
        static_constructors := cbody_rev @ !static_constructors;
        lambda_to_toplevels module_id env l2
    | Lprim (Pmakeblock(tag, Immutable, tyinfo), largs) ->
        (* TODO: why do we not use tyinfo here? *)
        (* immutable makeblock at the toplevel: add module export table init code *)
        let es = lambda_to_expression env lam in
        let export_var = C_GlobalVariable module_id in
        static_constructors := C_Assign (export_var, es) :: !static_constructors;
        [C_GlobalDefn (C_Pointer C_Boxed, export_var, None) (* .bss NULL *)]
    | _ -> failwith ("lambda_to_toplevels " ^ (dumps_lambda lam))
  in


  let deinlined_funs = ref [] in (* TODO: one day we want them to be attached near their users *)

  let rec fixup_expression accum e = (* sticks inlined statements on the front of accum *)
    match e with
    | C_InlineRevStatements [] -> failwith "fixup_expression: invalid inlinerevstatements (empty)"
    | C_InlineRevStatements (C_Expression e::sl) ->
        let (accum', e') = fixup_expression accum e in
        (fixup_rev_statements accum' sl), e'
    | C_InlineRevStatements sl ->
        let id = Ident.create "__deinlined" in
        let sl' = fixup_rev_statements ((C_VarDeclare (C_Boxed, C_Variable id, None))::accum) sl in
        let sl'' = assign_last_value_of_statement id sl' in
        sl'', C_Variable id
    | C_InlineFunDefn (name, args, sl) ->
        let sl' = fixup_rev_statements [] sl in
        deinlined_funs := (C_FunDefn (C_Boxed, name, args, Some sl')) :: !deinlined_funs;
        accum, name
    | C_IntLiteral _ | C_PointerLiteral _ | C_StringLiteral _ | C_CharLiteral _
    | C_Variable _ | C_GlobalVariable _ | C_Allocate _ -> accum, e
    | C_Blob (s1,e,s2) -> let (accum', e') = fixup_expression accum e in accum', C_Blob (s1,e',s2)
    | C_ArrayIndex (e,i) -> let (accum', e') = fixup_expression accum e in accum', C_ArrayIndex (e',i)
    | C_Cast (ty,e) -> let (accum', e') = fixup_expression accum e in accum', C_Cast (ty,e')
    | C_BinaryOp (ty,e1,e2) ->
        let (accum', e1') = fixup_expression accum e1 in
        let (accum'', e2') = fixup_expression accum' e2 in
        accum'', C_BinaryOp (ty,e1',e2')
    | C_InitialiserList es ->
        let rec loop accum es' = function
          | [] -> accum, List.rev es'
          | e::es ->
              let (accum', e') = fixup_expression accum e in
              loop accum' (e'::es') es
        in
        let (accum', es') = loop accum [] es in
        accum', C_InitialiserList es'
    | C_FunCall (id,es) -> (*TODO dedup *)
        let rec loop accum es' = function
          | [] -> accum, List.rev es'
          | e::es ->
              let (accum', e') = fixup_expression accum e in
              loop accum' (e'::es') es
        in
        let (accum', es') = loop accum [] es in
        accum', C_FunCall (id,es')

  and fixup_rev_statements accum sl = (* sticks fixed up sl on the front of accum *)
    let rec loop_start accum xs = loop accum (List.rev xs)
    and loop accum = function
      | [] -> accum
      | s::statements ->
          let accum'' =
            match s with
            | C_Expression e -> let (accum', e') = fixup_expression accum e in (C_Expression e') :: accum'
            | C_VarDeclare (ty,eid,None) -> C_VarDeclare (ty,eid,None) :: accum
            | C_VarDeclare (ty,eid,Some e) -> let (accum', e') = fixup_expression accum e in C_VarDeclare (ty,eid,Some e') :: accum'
            | C_Assign (eid,e) -> let (accum', e') = fixup_expression accum e in C_Assign (eid,e') :: accum'
            | C_If (e,slt,slf) -> let (accum', e') = fixup_expression accum e in (C_If (e', loop_start [] slt, loop_start [] slf)) :: accum'
            | C_Return (None) -> C_Return (None) :: accum
            | C_Return (Some e) -> let (accum', e') = fixup_expression accum e in (C_Return (Some e')) :: accum'
          in
          loop accum'' statements
    in
    loop_start accum sl
  in


  let toplevels =
    match lambda with
    | Lprim (Psetglobal id, [lam]) when Ident.name id = modulename ->
        lambda_to_toplevels id Env.empty lam
    | lam -> failwith ("compile_implementation unexpected root: " ^ (dumps_lambda lam))
  in
  let fixed_toplevels =
    List.map (map_toplevel (fixup_rev_statements [])) toplevels
  in

  let module_id = Ident.create modulename in
  let fixed_static_constructors =
    C_If (C_GlobalVariable module_id,
          [C_Return None],
          fixup_rev_statements [] !static_constructors)
  in
  let the_module_constructor =
    C_FunDefn (C_Void, C_GlobalVariable (module_initialiser_name module_id), [], Some [fixed_static_constructors])
  in

  let global_decls =
    List.concat (List.map (fun global_id ->
        [ C_FunDefn (C_Void, C_GlobalVariable (module_initialiser_name global_id), [], None)
        ; C_ExternDecl (C_Pointer C_Boxed, C_GlobalVariable global_id)
        ]
      ) !globals)
  in
  (* NB: should be evaluated last when we know all types *)
  let global_typedecls =
    List.map (fun ty -> C_TypeDefn ty) (TypeLibrary.dump_all_types_for_definition ())
  in
  [C_TopLevelComment "global typedecls:"] @ global_typedecls @
  [C_TopLevelComment "global decls:"] @ global_decls @
  [C_TopLevelComment "deinlined functions:"] @ !deinlined_funs @
  [C_TopLevelComment "fixed toplevels:"] @ fixed_toplevels @
  [C_TopLevelComment "the module constructor:" ; the_module_constructor]

;;
