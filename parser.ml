
open Ast

let debug_scope_flag = ref false
let debug_indent = ref 0

let rec debug_show_space = function
    | 0 -> ()
    | n -> print_char ' '; debug_show_space (n-1)

let debug_print s =
    if !debug_scope_flag then
        (debug_show_space !debug_indent; print_endline s)

let debug_in s =
    if !debug_scope_flag then
        (debug_print @@ "IN " ^ s; incr debug_indent)

let debug_out s =
    if !debug_scope_flag then
        (decr debug_indent; debug_print @@ "OUT " ^ s)

type t = {
    mutable toks : token list;
}

let make_expr edecl pos = (edecl, pos)

let is_eof p = p.toks = []

let get_pos p =
    if is_eof p then
        { filename = ""; line = 0; col = 0 }
    else
        snd @@ List.hd p.toks

let token p =
    if is_eof p then EOF
    else fst @@ List.hd p.toks

let next_token p =
    p.toks <- List.tl p.toks;
    if is_eof p then
        debug_print "next_token = EOF"
    else
        let tk = List.hd p.toks in
        debug_print @@ "next_token = " ^ s_token (fst tk) ^ " " ^ s_pos (snd tk)

let rec skip_newline p =
    debug_print "skip_newline";
    if token p = NEWLINE then
       (next_token p; skip_newline p)

let skip_semi p =
    if token p = SEMI then
        next_token p

let skip_indent p =
    if token p = INDENT then
        next_token p

let skip_dedent p =
    if token p = DEDENT then
        next_token p

let expect p tok =
    if token p <> tok then
        error (get_pos p) ("expect '" ^ s_token tok ^ "'")
    else
        next_token p

let is_apply e p =
    match e with
    | (ELambda _, _) | (EApply _, _) | (EId _, _) | (EMessage _, _) ->
        (match token p with
            | NIL | Id _ | Int _ | Float _ | Char _ | String _ 
            | LPAR | LBRA | UNIT -> true
            | _ -> false)
    | _ -> false

let is_rel_op = function EQ | NEQ | EQL | NEQL | LT | LE | GT | GE -> true | _ -> false
let token_to_op = function
    | PLUS -> BinAdd | MINUS -> BinSub | STAR -> BinMul | SLASH -> BinDiv
    | PERCENT -> BinMod | LT -> BinLT | LE -> BinLE | GT -> BinGT | GE -> BinGE
    | EQ -> BinEq | NEQ -> BinNeq | EQL -> BinEql | NEQL -> BinNeql
    | LOR -> BinLOr | LAND -> BinLAnd | DCOLON -> BinCons
    | _ -> failwith "token_to_op"


(*
record_expr
    = '{' {ID '=' expr sep} '}'
*)

(*
array_expr
    = '[|' expr {',' expr} [','] '|]'
*)

(*
list_expr
    = '[' [expr {',' expr}] [','] ']'
*)
let rec parse_list_expr p =
    debug_in "parse_list_expr";
    let rec loop acc p =
        match token p with
        | RBRA ->
            next_token p;
            List.rev acc
        | _ ->
            let e = parse_expr p in
            if token p = COMMA then begin
                next_token p;
                skip_newline p;
                loop (e :: acc) p
            end else begin
                expect p RBRA;
                List.rev (e :: acc)
            end
    in
    let pos = get_pos p in
    next_token p;
    skip_newline p;
    let el = loop [] p in
    let res = List.fold_right (fun a b -> make_expr (EBinary (BinCons, a, b)) pos) el (ENil, pos)
    in
    debug_out "parse_list_expr";
    res

(*
simple_expr
    = ID | BOOL_LIT | INT_LIT | CHAR_LIT | FLOAT_LIT | STRING_LIT 
    | list_expr | array_expr | '[]' | '(' expr {',' expr} ')'
    | record_expr
*)
and parse_simple_expr p =
    debug_in "parse_simple_expr";
    let pos = get_pos p in
    let res =
        match token p with
        | Id id ->
            debug_print "parse_simple_expr ID";
            next_token p;
            (EId id, pos)
        | LBRA ->
            parse_list_expr p
        | NIL ->
            debug_print "parse_simple_expr Nil";
            next_token p; (ENil, pos)
        | UNIT ->
            debug_print "parse_simple_expr Unit";
            next_token p; (EUnit, pos)
        | Int n ->
            debug_print "parse_simple_expr Int";
            next_token p;
            (EInt n, pos)
        | Float f ->
            debug_print "parse_simple_expr Float";
            next_token p;
            (EFloat f, pos)
        | Char ch ->
            debug_print "parse_simple_expr Char";
            next_token p;
            (EChar ch, pos)
        | String str ->
            debug_print "parse_simple_expr String";
            next_token p;
            (EString str, pos)
        | LPAR ->
            debug_print "parse_simple_expr ( )";
            next_token p;
            skip_newline p;
            let e = parse_expr p in
            if token p = COMMA then begin
                let rec loop lst =
                    let e = parse_expr p in
                    if token p = COMMA then begin
                        next_token p;
                        skip_newline p;
                        loop (e :: lst)
                    end else
                        List.rev (e :: lst)
                in
                let pos = get_pos p in
                next_token p;
                skip_newline p;
                let e2 = loop [] in
                expect p RPAR;
                (ETuple (e::e2), pos)
            end else begin
                expect p RPAR;
                e
            end
        | tk -> error (get_pos p) @@ "syntax error at " ^ s_token tk ^ " (simple_expr)"
    in
    debug_out @@ "parse_simple_expr:" ^ s_expr res;
    res

(*
postfix_expr
    = simple_expr {'.' ID}
    | simple_expr {'.[' expr ']'}
*)
and parse_postfix_expr p =
    debug_in "parse_postfix_expr";
    let rec parse_rhs lhs =
        match token p with
        | DOT -> begin
            debug_print " postfix DOT";
            next_token p;
            let e = match token p with
                    | Id id -> begin
                        next_token p;
                        make_expr (EMessage (lhs, id)) (get_pos p)
                      end
                    | _ -> error (get_pos p) "missing identifier"
            in
            parse_rhs e
          end
        | _ -> lhs
    in
    let res =
        let e = parse_simple_expr p in
        parse_rhs e
    in
    debug_out @@ "parse_postfix_expr:" ^ s_expr res;
    res

(*
unary_expr
    = ['!'|'-'] postfix_expr
*)
and parse_unary_expr p =
    debug_in "parse_unary_expr";
    let pos = get_pos p in
    let op =
        match token p with
        | NOT -> next_token p; Some UNot
        | MINUS -> next_token p; Some UMinus
        | _ -> None
    in
    let expr = parse_postfix_expr p in
    let res =
        match op with
        | None -> expr
        | Some uop -> make_expr (EUnary (uop, expr)) pos
    in
    debug_out @@ "parse_unary_expr:" ^ s_expr res;
    res

(*
apply_expr
    = unary_expr {postfix_expr}
*)
and parse_apply_expr p =
    debug_in "parse_apply_expr";
    let rec parse_apply_rhs lhs p =
        let pos = get_pos p in
        let expr = parse_postfix_expr p in
        let e = make_expr (EApply (lhs, expr)) pos in
        if is_apply e p then parse_apply_rhs e p
        else e
    in
    let e = parse_unary_expr p in
    let res =
        if is_apply e p then parse_apply_rhs e p
        else e
    in
    debug_out @@ "parse_apply_expr:" ^ s_expr res;
    res

(*
mul_expr
    = apply_expr {('*'|'/'|'%') mul_expr}
*)
and parse_mul_expr p =
    debug_in "parse_mul_expr";
    let rec parse_rhs lhs =
        if token p == STAR || token p == SLASH || token p == PERCENT then begin
            let op = token_to_op (token p) in
            next_token p;
            skip_newline p;
            let rhs = parse_mul_expr p in
            parse_rhs @@ make_expr (EBinary (op, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_apply_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_mul_expr:" ^ s_expr res;
    res

(*
add_expr
    = mul_expr {('+'|'-') add_expr}
*)
and parse_add_expr p =
    debug_in "parse_add_expr";
    let rec parse_rhs lhs =
        if token p == PLUS || token p == MINUS then begin
            let op = token_to_op (token p) in
            next_token p;
            skip_newline p;
            let rhs = parse_add_expr p in
            parse_rhs @@ make_expr (EBinary (op, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_mul_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_add_expr:" ^ s_expr res;
    res

(*
cons_expr
    = add_expr {'::' cons_expr}
*)
and parse_cons_expr p =
    debug_in "parse_cons_expr";
    let rec parse_rhs lhs =
        if token p == DCOLON then begin
            next_token p;
            skip_newline p;
            let rhs = parse_cons_expr p in
            parse_rhs @@ make_expr (EBinary (BinCons, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_add_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_cons_expr:" ^ s_expr res;
    res

(*
rel_expr
    = cons_expr {rel_op cons_expr}
*)
and parse_rel_expr p =
    debug_in "parse_rel_expr";
    let rec parse_rhs lhs =
        if is_rel_op (token p) then begin
            let op = token_to_op (token p) in
            next_token p;
            skip_newline p;
            let rhs = parse_cons_expr p in
            parse_rhs @@ make_expr (EBinary (op, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_cons_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_rel_expr:" ^ s_expr res;
    res

(*
land_expr
    = rel_expr {'&&' land_expr}
*)
and parse_land_expr p =
    debug_in "parse_land_expr";
    let rec parse_rhs lhs =
        if token p == LAND then begin
            next_token p;
            skip_newline p;
            let rhs = parse_land_expr p in
            parse_rhs @@ make_expr (EBinary (BinLAnd, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_rel_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_land_expr:" ^ s_expr res;
    res


(*
lor_expr
    = land_expr {'||' lor_expr}
*)
and parse_lor_expr p =
    debug_in "parse_lor_expr";
    let rec parse_rhs lhs =
        if token p == LOR then begin
            next_token p;
            skip_newline p;
            let rhs = parse_lor_expr p in
            parse_rhs @@ make_expr (EBinary (BinLOr, lhs, rhs)) (get_pos p)
        end else lhs
    in
    let e = parse_land_expr p in
    let res = parse_rhs e in
    debug_out @@ "parse_lor_expr:" ^ s_expr res;
    res

(*
cond_expr
    = lor_expr ['?' expr ':' expr]
*)
and parse_cond_expr p =
    debug_in "parse_cond_expr";
    let pos = get_pos p in
    let res =
        let e = parse_lor_expr p in
        if token p = QUES then begin
            next_token p;
            skip_newline p;
            let e2 = parse_expr p in
            skip_newline p;
            expect p COLON;
            skip_newline p;
            let e3 = parse_expr p in
            make_expr (ECond (e, e2, e3)) pos
        end else e
    in
    debug_out @@ "parse_cond_expr:" ^ s_expr res;
    res

(*
assign_expr
    = cond_expr ['<-' expr]
*)
and parse_assign_expr p =
    debug_in "parse_assign_expr";
    let pos = get_pos p in
    let res =
        let e = parse_cond_expr p in
        if token p = ASSIGN then begin
            next_token p;
            skip_newline p;
            let e2 = parse_expr p in
            skip_newline p;
            make_expr (EAssign (e, e2)) pos
        end else e
    in
    debug_out @@ "parse_assign_expr:" ^ s_expr res;
    res


(*
comp_expr
    = INDENT {expr sep} DEDENT
*)
and parse_comp_expr p =
    debug_in "parse_comp_expr";
    let rec loop acc =
        skip_semi p;
        skip_newline p;
        if token p = DEDENT then begin
            next_token p;
            List.rev acc
        end else begin
            let e = parse_expr p in
            match token p with
            | DEDENT ->
                next_token p;
                List.rev (e :: acc)
            | _ ->
                loop (e :: acc)
        end
    in
    let pos = get_pos p in
    next_token p;
    skip_newline p;
    let el = loop [] in
    let res = make_expr (EBlock el) pos in
    debug_out @@ "parse_comp_expr:" ^ s_expr res;
    res

(*
fn_expr
    = FN params '->' expr
*)
and parse_fn_expr p =
    debug_in "parse_fn";
    let res =
        next_token p;
        let ids = parse_params [] p in
        expect p ARROW;
        skip_newline p;
        let pos = get_pos p in
        let body = parse_expr p in
        List.fold_right (fun a b -> make_expr (ELambda (a, b)) pos) ids body
    in
    debug_out @@ "parse_fn:" ^ s_expr res;
    res

(*
if_expr
    = IF expr THEN expr [ELSE expr]
*)
and parse_if_expr p =
    debug_in "parse_if";
    let pos = get_pos p in
    next_token p;
    let e_cond = parse_expr p in
    skip_newline p;
    expect p THEN;
    skip_newline p;
    let e_then = parse_expr p in
    skip_newline p;
    let e_else =
        if token p = ELSE then begin
            next_token p;
            skip_newline p;
            parse_expr p
        end else
            make_expr EUnit (get_pos p)
    in
    skip_newline p;
    let res = make_expr (ECond (e_cond, e_then, e_else)) pos in
    debug_out @@ "parse_if:" ^ s_expr res;
    res

(*
let_expr
    = LET [MUT] id_def {[MUT] id_def} IN expr
*)
and parse_let_expr p =
    debug_in "parse_let";
    let pos = get_pos p in
    next_token p;
    let rec loop acc =
        skip_semi p;
        skip_newline p;
        if token p = IN then
            (next_token p; List.rev acc)
        else begin
            skip_indent p;
            let is_mutable =
                if token p = MUT then
                    (next_token p; true)
                else
                    false
            in
            match token p with
            | Id _ ->
                let e = parse_id_def is_mutable p in
                skip_semi p;
                skip_newline p;
                skip_dedent p;
                loop (e :: acc)
            | tk -> error (get_pos p) @@ "syntax error at " ^ s_token tk ^ " (let)"
        end
    in
    let el = loop [] in
    skip_newline p;
    let ebody = parse_expr p in
    let res = make_expr (ELet (el, ebody)) pos in
    debug_out @@ "parse_let:" ^ s_expr res;
    res

(*
expr
    = ';'
    | NEWLINE
    | let_expr
    | if_expr
    | fn_expr
    | comp_expr
    | assign_expr
*)
and parse_expr p =
    debug_in "parse_expr";
    let res =
        match token p with
        | EOF -> make_expr EEof (get_pos p)
        | SEMI | NEWLINE -> next_token p; parse_expr p
        | LET -> parse_let_expr p
        | IF -> parse_if_expr p
        | FN -> parse_fn_expr p
        | INDENT -> parse_comp_expr p
        | _ -> parse_assign_expr p
    in
    debug_out @@ "parse_expr:" ^ s_expr res;
    res

(*
type_def
    = TYPE type_decl 
*)
and parse_type_def p =
    debug_in "parse_type_def";
    let res =
        (*TODO*)
        next_token p;
        make_expr EUnit (get_pos p)
    in
    debug_out @@ "parse_type_def:" ^ s_expr res;
    res

(*
import
    = IMPORT ID [AS ID]
*)
and parse_import p =
    debug_in "parse_import";
    let pos = get_pos p in
    let res =
        next_token p;
        match token p with
            | Id id -> begin
                next_token p;
                if token p <> AS then
                    make_expr (EImport (id, None)) pos
                else begin
                    next_token p;
                    match token p with
                    | Id aid ->
                        next_token p;
                        make_expr (EImport (id, Some aid)) pos
                    | _ -> error pos "missing alias name"
                end
            end
            | _ -> error pos "missing module name"
    in
    debug_out @@ "parse_import: " ^ s_expr res;
    res

(*
module
    = MODULE ID
*)
and parse_module p =
    debug_in "parse_module";
    let pos = get_pos p in
    let res =
        next_token p;
        match token p with
        | Id id ->
            next_token p;
            make_expr (EModule id) pos
        | _ -> error pos "missing module name"
    in
    debug_out @@ "parse_module:" ^ s_expr res;
    res

(*
params
    = { ID | '()' }
*)
and parse_params acc p =
    debug_in "parse_params";
    let res =
        let pos = get_pos p in
        match token p with
        | UNIT ->
            next_token p;
            parse_params ((EUnit, pos) :: acc) p
        | Id id ->
            next_token p;
            parse_params ((EId id, pos) :: acc) p
        | _ ->
            List.rev acc
    in
    debug_out "parse_params";
    res

(*
id_def
    = ID {params} '=' expr
    | ID ':' typexpr
*)
and parse_id_def is_mutable p =
    debug_in "parse_id_def";
    let res = match token p with
        | Id id ->
            begin
                let pos = get_pos p in
                next_token p;
                let params = parse_params [] p in
                expect p EQ;
                skip_newline p;
                let body = parse_expr p in
                if params = [] then
                    make_expr (EValDef (is_mutable, id, body)) pos
                else
                    make_expr (EFuncDef (id, List.fold_right
                                    (fun a b -> make_expr (ELambda (a, b)) pos)
                                    params body)) pos
            end
        | tk -> error (get_pos p) @@ "syntax error at " ^ s_token tk ^ " (id_def)"
    in
    debug_out @@ "parse_id_def:" ^ s_expr res;
    res


(*
decl
    = module | import | type_def | id_def
*)
and parse_decl p =
    debug_in "parse_decl";
    let res = match token p with
        | MODULE -> parse_module p
        | IMPORT -> parse_import p
        | TYPE -> parse_type_def p
        | EOF -> make_expr EEof (get_pos p)
        | Id _ -> parse_id_def false p
        | tk -> error (get_pos p) @@ "syntax error at " ^ s_token tk ^ " (decl)"
    in
    debug_out @@ "parse_decl:" ^ s_expr res;
    res


(*
program
    = { ';' | NEWLINE | decl }
*)
let parse_program p =
    debug_in "parse_program";
    let rec loop acc =
        match token p with
        | SEMI | NEWLINE -> next_token p; loop acc
        | EOF -> List.rev acc
        | _ ->
            let e = parse_decl p in
            loop (e :: acc)
    in
    let res =
        let pos = get_pos p in
        let lst = loop [] in
        if List.length lst = 1 then
            List.hd lst
        else if List.length lst > 0 then
            make_expr (ESeq lst) pos
        else
            make_expr EEof pos
    in
    debug_out @@ "parse_program:" ^ s_expr res;
    res

(*
topdecl
    = module | import | type_def | expr
*)
let parse_topdecl p =
    debug_in "parse_topdecl";
    let res = match token p with
        | MODULE -> parse_module p
        | IMPORT -> parse_import p
        | TYPE -> parse_type_def p
        | EOF -> make_expr EEof (get_pos p)
        | _ -> parse_expr p
    in
    debug_out @@ "parse_topdecl:" ^ s_expr res;
    res

(*
toplevel
    = { ';' | NEWLINE | topdecl }
*)
let parse_toplevel p =
    debug_in "parse_toplevel";
    let rec loop acc =
        match token p with
        | SEMI | NEWLINE -> next_token p; loop acc
        | EOF -> List.rev acc
        | _ ->
            let e = parse_topdecl p in
            loop (e :: acc)
    in
    let res =
        let pos = get_pos p in
        let lst = loop [] in
        if List.length lst = 1 then
            List.hd lst
        else if List.length lst > 0 then
            make_expr (ESeq lst) pos
        else
            make_expr EEof pos
    in
    debug_out @@ "parse_toplevel:" ^ s_expr res;
    res

let debug_show_toks toks =
    if !debug_scope_flag then begin
        print_string "[";
        let rec loop = function
            | [] -> ()
            | x::xs ->
                print_string @@ s_token (fst x) ^ " ";
                loop xs
        in loop toks;
        print_endline "]"
    end
    

let parse_text toplevel txt =
    let pars = { toks = Scanner.open_text txt } in
    debug_show_toks pars.toks;
    if toplevel then
        parse_toplevel pars
    else
        parse_program pars

let parse_file filename =
    let pars = { toks = Scanner.open_file filename } in
    debug_show_toks pars.toks;
    parse_program pars

