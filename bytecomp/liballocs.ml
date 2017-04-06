[@@@ocaml.warning "-32-33-37-39-27-26"]

open Asttypes
open Primitive
open Format
open Types
open Lambda


let map_intersperse_concat f sep xs =
  let rec loop accum = function
    | [] -> accum
    | x::xs -> loop (accum ^ sep ^ (f x)) xs
  in
  match xs with
  | [] -> ""
  | x::xs -> loop (f x) xs

(* uses a formatter [f] to format [x], returned as a string *)
let formats f x =
  let b = Buffer.create 16 in
  let fo = formatter_of_buffer b in
  f fo x;
  pp_print_flush fo ();
  Buffer.contents b

let dumps_env env =
  Env.fold_values (fun s p vd accum ->
      Printf.sprintf "%s\n%s : %s" accum s (formats Printtyp.type_expr vd.val_type)) None env ""


module C = struct
  type ctype =
    | C_CommentedType of ctype * string
    | C_Pointer of ctype
    | C_Boxed (* the union ocaml_value_t *)
    | C_Void
    | C_Int
    | C_Double
    | C_UInt
    | C_Bool
    | C_Char
    | C_Struct of Ident.t * (ctype * string) list
    | C_FunPointer of ctype * ctype list
    | C_VarArgs
    | C_WhateverType (* used to type things like Lstaticraise, can unify with any type *)

  (* this isn't in emitcode because it's more fundamental and useful for debugging *)
  let rec ctype_to_string = function
    | C_CommentedType (cty, comment) -> Printf.sprintf "%s/*%s*/" (ctype_to_string cty) comment
    | C_Pointer cty -> (ctype_to_string cty) ^ "*"
    | C_Boxed -> "ocaml_value_t"
    | C_Void -> "void"
    | C_Int -> "intptr_t"
    | C_Double -> "double"
    | C_UInt -> "unsigned"
    | C_Bool -> "bool"
    | C_Char -> "char"
    | C_Struct (id, _) -> "struct " ^ (Ident.unique_name id)
    | C_FunPointer (tyret, tyargs) -> Printf.sprintf "%s(*)(%s)" (ctype_to_string tyret) (map_intersperse_concat ctype_to_string "," tyargs)
    | C_VarArgs -> "..."
    | C_WhateverType -> "/*UNDERSPECIFIED TYPE!*/"


  (* unifies two compatible ctypes, returns the most general of the two (which the other should be casted to)
   * returns a type which both types can guaranteed to be [cast]ed to validly
   * however not all types that both at and bt can be [cast]ed to are eligible for returning! (e.g. what's more general, void( * )() or int( * )()?)
   *)
  let unified_ctype ?hint at bt =
    if at = bt then at
    else if at = C_WhateverType then bt
    else if bt = C_WhateverType then at
    else if at = C_Boxed || bt = C_Boxed then C_Boxed
    else if at = C_Pointer C_Void then bt (* HACK: lambda likes to use Const_pointer 0 to represent nil, false, ... *)
    else if bt = C_Pointer C_Void then at
    else (
      failwith (Printf.sprintf "%scan't unify two different types: %s and %s\n"
        (match hint with None -> "" | Some x -> x ^ ": ")
        (ctype_to_string at)
        (ctype_to_string bt))
    )

  let rec canon_ctype = function
    | C_CommentedType (t,_) -> canon_ctype t
    | t -> t

  type expression =
    | C_Blob of (string * expression * string)
    | C_InlineRevStatements of ctype * statement list (* putting a statement where an expression belongs; this needs to be extracted in a later pass. ctype necessary annotation *)
    | C_InlineFunDefn of ctype(*return*) * expression(*name*) * (ctype * Ident.t) list * statement list
    | C_IntLiteral of Int64.t
    | C_FloatLiteral of string
    | C_PointerLiteral of int
    | C_StringLiteral of string
    | C_CharLiteral of char
    | C_GlobalVariable of string
    | C_Variable of Ident.t (* y *)
    | C_ArrayIndex of expression * expression option (* <...>[] or <...>[0] *)
    | C_Field of expression * string (* <...>.foo *)
    | C_InitialiserList of expression list (* { <...>, <...> } *)
    | C_Cast of ctype * expression
    | C_UnaryOp of string * expression
    | C_BinaryOp of string * expression * expression
    | C_FunCall of expression * expression list
    | C_Allocate of ctype * int(*multiplier*) * (Types.type_expr, string) result (* for debugging only *)

  and statement =
    | C_Expression of expression
    | C_VarDeclare of ctype * expression * expression option
    | C_Assign of expression * expression
    | C_If of expression * statement list * statement list (* reversed! *)
    | C_Return of expression option
    | C_LabelDecl of string
    | C_LabelGoto of string

  and texpression = ctype * expression

  (* just for debugging, nothing useful *)
  let statement_to_debug_string = function
    | C_Expression _ -> "C_Expression"
    | C_VarDeclare _ -> "C_VarDeclare"
    | C_Assign _ -> "C_Assign"
    | C_If _ -> "C_If"
    | C_Return _ -> "C_Return"
    | C_LabelDecl _ -> "C_LabelDecl"
    | C_LabelGoto _ -> "C_LabelGoto"

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

  let rec ctype_size_nwords = function
    | C_CommentedType (cty, _) -> ctype_size_nwords cty
    | C_Pointer _ | C_FunPointer _ | C_Boxed | C_Int | C_Double | C_UInt -> 1
    | C_Struct (_, tys) -> List.fold_left (fun acc (t,_) -> acc + (ctype_size_nwords t)) 0 tys
    | C_Bool | C_Char -> failwith "bools/chars have sub-word size"
    | C_VarArgs | C_Void | C_WhateverType ->
        failwith "invalid ctype to query size for"



  let rec cast target_ctype (source_ctype, source_cexpr) =
    let box_kind = function
      | C_Int | C_UInt | C_Bool | C_Char -> "i", C_Int
      | C_Double -> "d", C_Double
      | C_Pointer _ -> "p", C_Pointer C_Boxed
      | C_FunPointer _ -> "fp", C_FunPointer (C_Boxed, [])
      | _ ->
          failwith (Printf.sprintf "bad cast from %s to %s: invalid box kind" (ctype_to_string source_ctype) (ctype_to_string target_ctype))
    in
    let st = canon_ctype source_ctype in
    let tt = canon_ctype target_ctype in
    let st = if st = C_WhateverType then tt else st in
    let tt = if tt = C_WhateverType then st else tt in
    if st = tt then (
      source_cexpr
    ) else if st <> C_Boxed && tt <> C_Boxed then (
      (* we need to cast *)
      (* make sure the cast is sensible *)
      let skind = fst (box_kind st) in
      let tkind = fst (box_kind tt) in
      if (not (skind = tkind) &&
          not (skind = "p" && tkind = "i") (* e.g. pointer to bool *)
         ) then begin
        failwith (Printf.sprintf "bad cast from %s to %s: differing box kinds" (ctype_to_string source_ctype) (ctype_to_string target_ctype))
      end;
      C_Cast (tt, source_cexpr)
    ) else if st = C_Boxed then (
      (* we need to unbox according to target_ctype *)
      let box_field, box_canon_type = box_kind tt in
      cast tt (box_canon_type, C_Field (source_cexpr, box_field))
    ) else if tt = C_Boxed then (
      (* we need to box according to source_ctype *)
      let box_field, box_canon_type = box_kind st in
      C_Cast (tt,
        (* TODO: rewrite -- this is disgusting *)
        C_Blob ( Printf.sprintf "{ .%s = " box_field
               , cast box_canon_type (source_ctype, source_cexpr)
               , " }"))
    ) else failwith "unreachable"

  (* for extracting C_InlineRevStatements. assigns the final expression to the
   * variable [id] *)
  let rec assign_last_value_of_statement (tgt_type,id) (src_type,sl) =
    let f e = C_Assign (C_Variable id, cast tgt_type (src_type, e)) in
    match sl with
    | [] -> failwith "assign_last_value_of_statement: empty sl" (* TODO: unit? *)
    | s::sl ->
      begin
        match s with
        | C_Expression e -> f e :: sl
        | C_Assign (varid,e) -> f varid :: s::sl
        | C_If (e,slt, slf) ->
            C_If (e,
              assign_last_value_of_statement (tgt_type,id) (src_type,slt),
              assign_last_value_of_statement (tgt_type,id) (src_type,slf)) :: sl
        | C_Return _ | C_LabelGoto _ -> s :: sl (* the requested assignment doesn't matter, we're gone! *)
        | C_VarDeclare _ | C_LabelDecl _ ->
            failwith (Printf.sprintf "assign_last_value_of_statement: unexpected statement %s as last statement in sl" (statement_to_debug_string s))
      end


  (* a wrapper around assign_last_value_of_statement: casts type of sl *)
  let rec cast_revst target_ctype (source_ctype, source_sl) =
    let st = canon_ctype source_ctype in
    let tt = canon_ctype target_ctype in
    if st = tt then (
      source_sl
    ) else (
      let id = Ident.create "__typecasted" in
      [C_Expression (C_Variable id)] @
      assign_last_value_of_statement (target_ctype, id) (source_ctype, source_sl) @
      [C_VarDeclare (target_ctype, C_Variable id, None)]
    )

end

open C



module Emitcode = struct
  let cstruct_defn_string id fields =
    let fieldstrings =
      map_intersperse_concat (fun (ctype, fieldname) ->
        Printf.sprintf "%s %s;" (ctype_to_string ctype) fieldname
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
    | C_InlineRevStatements (_, sl) -> Printf.sprintf "/*FIXME:inline*/{\n%s\n}" (rev_statements_to_string sl)
    | C_InlineFunDefn (retty, name, args, sl) -> Printf.sprintf "/*FIXME:inline fun %s %s (%s)*/{\n%s\n}"
                              (ctype_to_string retty)
                              (expression_to_string name)
                              (map_intersperse_concat (fun (t,id) -> (ctype_to_string t) ^ " " ^ (Ident.unique_name id)) ", " args)
                              (rev_statements_to_string sl)
    | C_IntLiteral i -> Int64.to_string i
    | C_FloatLiteral f -> f
    | C_PointerLiteral i -> Printf.sprintf "((void*)%d)" i
    | C_StringLiteral str -> Printf.sprintf "%S" str
    | C_CharLiteral ch -> Printf.sprintf "%C" ch
    | C_GlobalVariable id -> id
    | C_Variable id -> Ident.unique_name id
    | C_Field (e,f) -> Printf.sprintf "%s.%s" (expression_to_string e) f
    | C_ArrayIndex (e,Some i) -> Printf.sprintf "%s[%s]" (expression_to_string e) (expression_to_string i)
    | C_ArrayIndex (e,None) -> (expression_to_string e) ^ "[]"
    | C_InitialiserList es -> Printf.sprintf "{%s}" (map_intersperse_concat expression_to_string ", " es)
    | C_Cast (ty,e) -> Printf.sprintf "((%s)%s)" (ctype_to_string ty) (expression_to_string e)
    | C_UnaryOp (op,x) -> "(" ^ op ^ (expression_to_string x) ^ ")"
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
    | C_LabelDecl s -> s ^ ":;"
    | C_LabelGoto s -> "goto " ^ s ^ ";"

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


  let to_file oc _modulename _filename c_code =
    output_string oc "#include <stdlib.h>\n#include <stdint.h>\n";
    output_string oc "#include \"liballocs.h\"\n";
    output_string oc "\n";
    List.iter (fun t ->
        output_string oc (toplevel_to_string t);
        output_string oc "\n\n";
      ) c_code
end


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
        ocaml_to_c_type fieldtype, fieldname) ts))
    in
    TypeHash.add g_table type_expr ctype;
    ctype

  and add_mapping type_expr =
    match type_expr.desc with
    | Ttuple ts ->
        add_structlike_mapping type_expr "tuple"
          (List.mapi (fun i t -> (Printf.sprintf "_%d" (i+1)), t) ts)
    | Tobject (ts, r) when !r = None ->
        add_structlike_mapping type_expr "struct"
          (tfield_to_list ts)
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

  let reset () =
    TypeHash.clear g_table
end


(* the VarLibrary keeps track of known ctypes for each variable *)
(* Globals only for now *)
module VarLibrary = struct
  module VarHash = Hashtbl.Make(struct
    include Ident
    let equal = same (* equal checks equality of name only, not stamp *)
  end)
  let g_table: ctype VarHash.t = VarHash.create 16

  let ctype id =
    Printf.printf "getting type of %s... " (Ident.unique_name id);
    let the_ctype =
      try VarHash.find g_table id
      with Not_found -> (
        Printf.printf "\n<<<<<<<<<<<<<<<<<<<<WARNING>>>>>>>>>>>>>>>>>>>>\ntype not found. substituting with ocaml_value_t\n<<<<<<<<<<<<<<<<<<<<WARNING>>>>>>>>>>>>>>>>>>>>\n ... ";
        C_Boxed
      )
    in
    Printf.printf "%s\n" (ctype_to_string the_ctype);
    the_ctype


  let set_ctype ctype id =
    Printf.printf "setting type of %s to %s\n" (Ident.unique_name id) (ctype_to_string ctype);
    if VarHash.mem g_table id then
      failwith "VarLibrary: trying to set ctype of variable that already exists!"
    else
      VarHash.add g_table id ctype

  let create ctype name =
    let id = Ident.create name in
    Printf.printf "creating %s with type %s\n" (Ident.unique_name id) (ctype_to_string ctype);
    set_ctype ctype id;
    id

  let reset () =
    VarHash.clear g_table;
    (* prepopulate with some inbuilt modules *)
    List.iter (fun (ty, id) -> set_ctype ty id)
    [ C_Pointer C_Boxed, Ident.create_persistent "Pervasives"
    ; C_Pointer C_Boxed, Ident.create_persistent "Printf"
    ; C_Pointer C_Boxed, Ident.create_persistent "List"
    ]
end



let compile_implementation modulename lambda =
  let () = TypeLibrary.reset () in
  let () = VarLibrary.reset () in

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

  let module_initialiser_name module_id = (Ident.name module_id) ^ "__init" in

  let global_decls = ref [] in

  (* translate a let* to a variable or function declaration *)
  let rec let_to_rev_statements env id lam =
    match lam with
    | Levent (body, ev) ->
        let_to_rev_statements (Envaux.env_from_summary ev.lev_env Subst.identity) id body
    | Lfunction { params ; body } ->
        (* NB: let_args_to_toplevels copies this for now, it will go eventually *)
        Printf.printf "Got a expression fun LET %s\n%!" (Ident.unique_name id);
        let typedparams = List.map (fun id ->
            (*ENV
              Printf.printf "We have these in env: %s\n%!" (dumps_env env);
              Printf.printf "Looking up type of %s\n%!" (Ident.unique_name id);
              let (p, ty) = Env.lookup_value (Longident.Lident (Ident.name id)) env in
              C_Boxed ty.val_type, id
            *)
            C_Boxed, id
          ) params in
        (* TODO: determine ret type of function*)
        let ret_type = C_Boxed in
        let typedparams_types = List.map fst typedparams in
        let ret_var_id = VarLibrary.create ret_type "_return_value" in
        (* be careful to assign the param types before lambda_to_trev_statements *)
        List.iter2 (fun ctype id -> VarLibrary.set_ctype ctype id) typedparams_types params;
        VarLibrary.set_ctype (C_FunPointer (ret_type, typedparams_types)) id;
        let rev_st =
          assign_last_value_of_statement (ret_type, ret_var_id) (lambda_to_trev_statements env body)
        in
        let cbody =
          [C_Return (Some (C_Variable ret_var_id))] @
          rev_st @
          [C_VarDeclare (ret_type, C_Variable ret_var_id, None)]
        in
        [C_Expression (C_InlineFunDefn (ret_type, C_Variable id, typedparams, cbody))]
    | _ ->
        let ctype, defn = lambda_to_texpression env lam in
        VarLibrary.set_ctype ctype id;
        [C_VarDeclare (ctype, C_Variable id, Some (defn))]


  (* Translates a structured constant into an expression and its ctype *)
  and structured_constant_to_texpression env = function
    | Const_base (Const_int n) -> C_Int, C_IntLiteral (Int64.of_int n)
    | Const_base (Const_int32 n) -> C_Int, C_IntLiteral (Int64.of_int32 n)
    | Const_base (Const_int64 n) -> C_Int, C_IntLiteral n
    | Const_base (Const_nativeint n) -> C_Int, C_IntLiteral (Int64.of_nativeint n)
    | Const_base (Const_float f) -> C_Double, C_FloatLiteral f
    | Const_base (Const_char ch) -> C_Int, C_Cast (C_Int, C_CharLiteral ch)(*not C_Char because needs to be word-sized *)
    | Const_base (Const_string (s, None)) -> C_Pointer C_Char, C_StringLiteral s(* FIXME: MUTABLE STRING, SHOULD NOT BE SHARED/just a literal *)
    | Const_pointer n -> C_Pointer C_Void, C_PointerLiteral n
    | Const_immstring str -> C_Pointer C_Char, C_StringLiteral str (* immediate/immutable string *)
    | Const_block (tag, scl) ->
      lambda_to_texpression env (Lprim
        (Pmakeblock (tag, Asttypes.Immutable, Error "structured_constant_to_texpression"),
        List.map (fun x -> Lconst x) scl))
      (* we'll recurse back into this function eventually *)
    | konst -> failwith (Printf.sprintf "structured_constant_to_texpression: unknown constant type %s" (formats Printlambda.structured_constant konst))

  and lambda_to_texpression env lam : C.texpression =
    match lam with
    | Levent (body, ev) ->
        lambda_to_texpression (Envaux.env_from_summary ev.lev_env Subst.identity) body
    | Lprim (prim, largs) ->
        begin
          let bop expected_type op e1 e2 =
            expected_type,
            C_BinaryOp (op,
                        cast expected_type (lambda_to_texpression env e1),
                        cast expected_type (lambda_to_texpression env e2))
          in
          let uop expected_type op e =
            expected_type,
            C_UnaryOp (op,
                       cast expected_type (lambda_to_texpression env e))
          in
          let bool_bop = bop C_Bool in
          let int_bop = bop C_Int in
          let int_uop = uop C_Int in
          match prim, largs with
          | Pidentity, [x] -> (* why does Pidentity occur? e.g. in Pervasives *)
              lambda_to_texpression env x
          | Pignore, [x] -> (* why does Pignore occur? e.g. in Pervasives *)
              C_WhateverType, C_GlobalVariable "NULL" (* this is the closest we'll get to () *)
          | Popaque, [Lprim (Pgetglobal id, [])] ->
              if List.mem id Predef.all_predef_exns then begin
                (* this is a predefined exception *)
                VarLibrary.set_ctype C_Boxed id;
                global_decls :=
                  C_ExternDecl (C_Boxed, C_GlobalVariable (Ident.name id)) ::
                  !global_decls;
                C_Boxed, C_GlobalVariable (Ident.name id)
              end else begin
                (* assume these are declarations of use of external modules; ensure initialisation *)
                global_decls :=
                  C_FunDefn (C_Void, C_GlobalVariable (module_initialiser_name id), [], None) ::
                  C_ExternDecl (C_Pointer C_Boxed, C_GlobalVariable (Ident.name id)) ::
                  !global_decls;
                C_Pointer C_Boxed, (* all modules are represented as arrays of ocaml_value_t *)
                C_InlineRevStatements (VarLibrary.ctype id,
                  [ C_Expression (C_GlobalVariable (Ident.name id))
                  ; C_Expression (C_FunCall (C_GlobalVariable (module_initialiser_name id), []))])
              end
          | Popaque, [lam] ->
              (* TODO: can't this be handled recursively? *)
              let ctype, rev_st = lambda_to_trev_statements env lam in
              ctype, C_InlineRevStatements (ctype, rev_st)
          | Pgetglobal id, [] ->
              VarLibrary.ctype id, C_GlobalVariable (Ident.name id)
          | Pfield i, [lam] ->
              (* TODO: make this use type-specific accessors if possible *)
              C_Boxed,
              C_ArrayIndex (cast (C_Pointer C_Boxed) (lambda_to_texpression env lam), Some (C_IntLiteral (Int64.of_int i)))
          | Psetfield (i, (Immediate | Pointer), Assignment), [lam; lval] ->
              (* note we assign immediates and pointers in the same way *)
              (* TODO: make this use type-specific accessors if possible *)
              C_Boxed,
              C_InlineRevStatements (C_Boxed, [
                C_Assign (
                  C_ArrayIndex (cast (C_Pointer C_Boxed) (lambda_to_texpression env lam), Some (C_IntLiteral (Int64.of_int i))),
                  cast C_Boxed (lambda_to_texpression env lval)
                )
              ])
          | Pmakeblock (tag, mut, tyinfo), contents ->
              let id = Ident.create "__makeblock" in
              let var = C_Variable id in
              let rec construct k statements = function
                | [] -> (C_Expression var) :: statements
                | e_head::el ->
                    let texpr = lambda_to_texpression env e_head in
                    let elem = C_ArrayIndex (var, Some (C_IntLiteral (Int64.of_int k))) in
                    let assign = C_Assign (elem, cast C_Boxed texpr) in
                    construct (succ k) (assign::statements) el
              in
              let block_type = C_Pointer C_Boxed in
              block_type,
              C_InlineRevStatements (block_type, construct 0 [
                  C_VarDeclare (block_type, C_Variable id,
                                Some (do_allocation (List.length contents) tyinfo))
                ] contents)
          | Psequand, [e1;e2] -> bool_bop "&&" e1 e2
          | Psequor, [e1;e2] -> bool_bop "||" e1 e2
          | Paddint, [e1;e2] -> int_bop "+" e1 e2
          | Psubint, [e1;e2] -> int_bop "-" e1 e2
          | Pmulint, [e1;e2] -> int_bop "*" e1 e2
          | Pdivint, [e1;e2] -> int_bop "/" e1 e2
          | Pmodint, [e1;e2] -> int_bop "%" e1 e2
          | Pandint, [e1;e2] -> int_bop "&" e1 e2
          | Porint, [e1;e2] -> int_bop "|" e1 e2
          | Pxorint, [e1;e2] -> int_bop "^" e1 e2
          | Plslint, [e1;e2] -> int_bop "<<" e1 e2
          | Plsrint, [e1;e2] -> bop C_UInt ">>" e1 e2
          | Pnegint, [e] -> int_uop "-" e
          | Pintcomp(Ceq), [e1;e2] -> int_bop "==" e1 e2
          | Pintcomp(Cneq), [e1;e2] -> int_bop "!=" e1 e2
          | Pintcomp(Clt), [e1;e2] -> int_bop "<" e1 e2
          | Pintcomp(Cle), [e1;e2] -> int_bop "<=" e1 e2
          | Pintcomp(Cgt), [e1;e2] -> int_bop ">" e1 e2
          | Pintcomp(Cge), [e1;e2] -> int_bop ">=" e1 e2
          | Pccall {prim_name; prim_arity}, lam ->
              assert (List.length lam = prim_arity);
              let args =
                List.map (fun x -> cast C_Boxed (lambda_to_texpression env x)) lam
              in
              C_Boxed, C_FunCall (C_GlobalVariable prim_name, args)
          | Praise(Raise_regular | Raise_reraise), [e] ->
              C_WhateverType, C_FunCall (C_GlobalVariable "ocaml_liballocs_raise_exn", [cast C_Boxed (lambda_to_texpression env e)])
          | Pstringlength, [e] ->
              C_Int, C_FunCall (C_GlobalVariable "strlen", [cast (C_Pointer C_Char) (lambda_to_texpression env e)])
          | Pstringrefs, [es; en] ->
              let exps = cast (C_Pointer C_Char) (lambda_to_texpression env es) in
              let expn = cast (C_Int) (lambda_to_texpression env en) in
              C_Char, C_ArrayIndex (exps, Some expn)
          | _ -> failwith ("lambda_to_expression Lprim " ^ (Printlambda.name_of_primitive prim))
        end
    | Lconst sc -> structured_constant_to_texpression env sc
    | Lvar id -> VarLibrary.ctype id, C_Variable id
    | Lapply { ap_func = e_id ; ap_args } ->
        let vararg =
          (* FIXME: hardcoded for printf *)
          match e_id with
          | Lprim (Pfield _, [Lprim (Pgetglobal id, [])]) when Ident.name id = "Printf" -> true
          | _ -> false
        in
        let ctype, expr = lambda_to_texpression env e_id in
        let apfunc_actual_ctype =
          if vararg
          then C_FunPointer (C_Boxed, [C_Boxed; C_VarArgs])
          else ctype
        in
        let ret_ctype, arg_ctypes =
          match apfunc_actual_ctype with
          | C_FunPointer (x, a) when vararg ->
              x, List.map (fun _ -> C_Boxed) ap_args
          | C_FunPointer (x, a) when not vararg ->
              x, a
          | C_Boxed ->
              (* calling a boxed function pointer -- have to guess... *)
              (* for instance (field 0 (global List!)) *)
              C_Boxed, List.map (fun _ -> C_Boxed) ap_args
          | _ -> failwith "Lapply on non-function expression?"
        in
        let cargs =
          List.map2 (fun ctype x -> cast ctype (lambda_to_texpression env x))
                      arg_ctypes ap_args
        in
        ret_ctype, C_FunCall (cast apfunc_actual_ctype (ctype, expr), cargs)
    | Lifthenelse _ ->
        let ctype, rev_st = lambda_to_trev_statements env lam in
        ctype, C_InlineRevStatements (ctype, rev_st)
    | _ -> failwith ("lambda_to_texpression " ^ (formats Printlambda.lambda lam))

  and lambda_to_trev_statements env lam =
    let unify_revsts ?hint (ctype_a, sl_a) (ctype_b, sl_b) =
      let ctype = unified_ctype ?hint ctype_a ctype_b in
      let sl_a = cast_revst ctype (ctype_a, sl_a) in
      let sl_b = cast_revst ctype (ctype_b, sl_b) in
      (ctype, sl_a, sl_b)
    in
    match lam with
    | Levent (body, ev) ->
        lambda_to_trev_statements (Envaux.env_from_summary ev.lev_env Subst.identity) body
    | Lvar _ | Lconst _ | Lprim _ | Lapply _ ->
        let ctype, cexpr = lambda_to_texpression env lam in
        ctype, [C_Expression cexpr]
    | Llet (_strict, id, args, body) ->
        (* NB: order of execution of the next two lines matter! *)
        let let_statements = let_to_rev_statements env id args in
        let ctype, rev_st = lambda_to_trev_statements env body in
        ctype, rev_st @ let_statements
    | Lletrec ([id, args], body) ->
        (* NB: order of execution of the next two lines matter! *)
        let let_statements = let_to_rev_statements env id args in
        let ctype, rev_st = lambda_to_trev_statements env body in
        ctype, rev_st @ let_statements
    | Lsequence (l1, l2) ->
        let _ctype1, rev_st1 = lambda_to_trev_statements env l1 in
        let ctype2, rev_st2 = lambda_to_trev_statements env l2 in
        ctype2, rev_st2 @ rev_st1
    | Lifthenelse (l,lt,lf) ->
        let cexpr_cond = cast C_Bool (lambda_to_texpression env l) in
        let trev_t = lambda_to_trev_statements env lt in
        let trev_f = lambda_to_trev_statements env lf in
        let (ctype, sl_t, sl_f) = unify_revsts ~hint:"ifthenelse" trev_t trev_f in
        ctype, [C_If (cexpr_cond, sl_t, sl_f)]
    | Ltrywith (lbody, param, lhandler) ->
        VarLibrary.set_ctype C_Boxed param;
        let trev_body = lambda_to_trev_statements env lbody in
        let trev_hand = lambda_to_trev_statements env lhandler in
        let (ctype, sl_body, sl_hand) = unify_revsts ~hint:"trywith" trev_body trev_hand in
        let sl_hand = sl_hand @ [
            C_VarDeclare (C_Boxed, C_Variable param, Some (C_FunCall (C_GlobalVariable "ocaml_liballocs_get_exn", [])))
          ]
        in
        ctype, [C_If (C_FunCall (C_GlobalVariable "ocaml_liballocs_push_exn_handler", []),
          sl_body, sl_hand)]
    | Lstaticcatch (lbody, (id, [](*vars, what do?*)), lhandler) ->
        let trev_body = lambda_to_trev_statements env lbody in
        let trev_hand = lambda_to_trev_statements env lhandler in
        let (ctype, sl_body, sl_hand) = unify_revsts ~hint:"staticcatch" trev_body trev_hand in
        ctype, [C_If (C_IntLiteral Int64.one, sl_body, sl_hand @ [
          C_LabelDecl (Printf.sprintf "label_staticcatch_%d" id)
        ])]
    | Lstaticraise (id, [](*vars, what do?*)) ->
        C_WhateverType,
        [C_LabelGoto (Printf.sprintf "label_staticcatch_%d" id)]

    | Lstringswitch (lam, cases, default) ->
        let default_ctype, default_sl =
          match default with
          | Some x -> lambda_to_trev_statements env x
          | None -> C_WhateverType, []
        in
        let switch_ctype = C_Pointer C_Char in
        let switch_sl = cast switch_ctype (lambda_to_texpression env lam) in
        let var = Ident.create "__stringswitch" in
        let trevs = List.map (fun (s,l) -> s, lambda_to_trev_statements env l) cases in
        let ctype = List.fold_left (fun a (_, (b, _)) -> unified_ctype a b) default_ctype trevs in
        let sl =
          List.fold_left (fun acc_sl (s, this_trev) ->
            let this_sl = cast_revst ctype this_trev in
            [C_If (
              C_BinaryOp ("==", C_IntLiteral Int64.zero, C_FunCall (C_GlobalVariable "strcmp", [C_Variable var; C_StringLiteral s])),
              this_sl,
              acc_sl
            )]
          ) default_sl trevs
        in
        ctype, sl @ [C_VarDeclare (switch_ctype, C_Variable var, Some switch_sl)]

    | lam -> failwith ("lambda_to_trev_statements " ^ (formats Printlambda.lambda lam))
  in

  let static_constructors = ref [] in

  let rec let_args_to_toplevels env id lam = (* FIXME: get rid of this soon! we can probably handle everything "inline", with toplevels only created at fixup *)
    match lam with
    | Levent (body, ev) ->
        let_args_to_toplevels (Envaux.env_from_summary ev.lev_env Subst.identity) id body
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
        (* TODO: determine ret type of function*)
        let ret_type = C_Boxed in
        let typedparams_types = List.map fst typedparams in
        let ret_var_id = VarLibrary.create ret_type "_return_value" in
        (* be careful to assign the param types before lambda_to_trev_statements *)
        List.iter2 (fun ctype id -> VarLibrary.set_ctype ctype id) typedparams_types params;
        VarLibrary.set_ctype (C_FunPointer (ret_type, typedparams_types)) id;
        let rev_st =
          assign_last_value_of_statement (ret_type, ret_var_id) (lambda_to_trev_statements env body)
        in
        let cbody =
          [C_Return (Some (C_Variable ret_var_id))] @
          rev_st @
          [C_VarDeclare (ret_type, C_Variable ret_var_id, None)]
        in
        [C_FunDefn (ret_type, C_Variable id, typedparams, Some cbody)]
    | _ ->
        let ctype, sl = lambda_to_trev_statements env lam in
        let cbody =
          C_Assign (C_Variable id, C_InlineRevStatements (ctype, sl))
        in
        static_constructors := cbody :: !static_constructors;
        VarLibrary.set_ctype ctype id;
        [ C_GlobalDefn (ctype, C_Variable id, None) ]
  in

  let rec lambda_to_toplevels module_id env lam =
    match lam with
    | Levent (body, ev) ->
        lambda_to_toplevels module_id (Envaux.env_from_summary ev.lev_env Subst.identity) body
    | Llet (_strict, id, args, body) ->
        Printf.printf "Got a LET %s\n%!" (Ident.unique_name id);
        (* evaluation order important for type propagation *)
        let a = let_args_to_toplevels env id args in
        let b = lambda_to_toplevels module_id env body in
        a @ b
    | Lletrec ([id, args], body) ->
        Printf.printf "Got a LETREC %s\n%!" (Ident.unique_name id);
        (* evaluation order important for type propagation *)
        let a = let_args_to_toplevels env id args in
        let b = lambda_to_toplevels module_id env body in
        a @ b
    | Lsequence (l1, l2) -> (* TODO: dedup this... *)
        let cbody_rev = snd (lambda_to_trev_statements env l1) in
        static_constructors := cbody_rev @ !static_constructors;
        lambda_to_toplevels module_id env l2
    | Lprim (Pmakeblock(tag, Immutable, tyinfo), largs) ->
        (* This is THE immutable makeblock at the toplevel. *)
        let ctype, es = lambda_to_texpression env lam in
        assert (ctype = C_Pointer C_Boxed);
        let export_var = C_GlobalVariable (Ident.name module_id) in (* no need to add export var to VarLibrary *)
        static_constructors := C_Assign (export_var, es) :: !static_constructors;
        [C_GlobalDefn (ctype, export_var, None) (* .bss initialised to NULL *)]
    | _ -> failwith ("lambda_to_toplevels " ^ (formats Printlambda.lambda lam))
  in


  let deinlined_funs = ref [] in (* TODO: one day we want them to be attached near their users *)

  let rec fixup_expression accum e = (* sticks inlined statements on the front of accum *)
    match e with
    | C_InlineRevStatements (_, []) -> failwith "fixup_expression: invalid inlinerevstatements (empty)"
    | C_InlineRevStatements (_ctype, C_Expression e::sl) ->
        let (accum', e') = fixup_expression accum e in
        (fixup_rev_statements accum' sl), e'
    | C_InlineRevStatements (ctype,sl) ->
        let id = Ident.create "__deinlined" in
        let sl' = fixup_rev_statements ((C_VarDeclare (ctype, C_Variable id, None))::accum) sl in
        let sl'' = assign_last_value_of_statement (ctype,id) (ctype,sl') in
        sl'', C_Variable id
    | C_InlineFunDefn (retty, name, args, sl) ->
        let sl' = fixup_rev_statements [] sl in
        deinlined_funs := (C_FunDefn (retty, name, args, Some sl')) :: !deinlined_funs;
        accum, name
    | C_IntLiteral _ | C_FloatLiteral _ | C_PointerLiteral _ | C_StringLiteral _ | C_CharLiteral _
    | C_Variable _ | C_GlobalVariable _ | C_Allocate _ -> accum, e
    | C_Blob (s1,e,s2) ->
        let (accum', e') = fixup_expression accum e in accum', C_Blob (s1,e',s2)
    | C_Field (e,f) ->
        let (accum', e') = fixup_expression accum e in accum', C_Field (e',f)
    | C_ArrayIndex (e, None) ->
        let (accum', e') = fixup_expression accum e in accum', C_ArrayIndex (e', None)
    | C_ArrayIndex (e, Some i) ->
        let (accum', e') = fixup_expression accum e in
        let (accum'', i') = fixup_expression accum' i in
        accum'', C_ArrayIndex (e', Some i')
    | C_Cast (ty,e) ->
        let (accum', e') = fixup_expression accum e in accum', C_Cast (ty,e')
    | C_UnaryOp (op,e) ->
        let (accum', e') = fixup_expression accum e in accum', C_UnaryOp (op,e')
    | C_BinaryOp (op,e1,e2) ->
        let (accum', e1') = fixup_expression accum e1 in
        let (accum'', e2') = fixup_expression accum' e2 in
        accum'', C_BinaryOp (op,e1',e2')
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
          | C_Return (Some e) -> let (accum', e') = fixup_expression accum e in (C_Return (Some e')) :: accum'
          | C_Return (None) | C_LabelDecl _ | C_LabelGoto _ ->
              s :: accum
        in
        loop accum'' statements
    in
    loop_start accum sl
  in


  let toplevels =
    match lambda with
    | Lprim (Psetglobal id, [lam]) when Ident.name id = modulename ->
        lambda_to_toplevels id Env.empty lam
    | lam -> failwith ("compile_implementation unexpected root: " ^ (formats Printlambda.lambda lam))
  in
  let fixed_toplevels =
    List.map (map_toplevel (fixup_rev_statements [])) toplevels
  in

  let module_id = Ident.create modulename in
  let fixed_static_constructors =
    C_If (C_GlobalVariable modulename,
          [C_Return None],
          fixup_rev_statements [] !static_constructors)
  in
  let the_module_constructor =
    C_FunDefn (C_Void, C_GlobalVariable (module_initialiser_name module_id), [], Some [fixed_static_constructors])
  in

  (* NB: should be evaluated last when we know all types *)
  let global_typedecls =
    List.map (fun ty -> C_TypeDefn ty) (TypeLibrary.dump_all_types_for_definition ())
  in

  [C_TopLevelComment "global typedecls:"] @ global_typedecls @
  [C_TopLevelComment "extern decls:"] @ !global_decls @
  [C_TopLevelComment "deinlined functions:"] @ !deinlined_funs @
  [C_TopLevelComment "fixed toplevels:"] @ fixed_toplevels @
  [C_TopLevelComment "the module constructor:" ; the_module_constructor]

;;
