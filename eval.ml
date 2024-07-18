
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


let rec typ_from_expr pos = function
    | TE_Name "unit" -> TUnit
    | TE_Name "int" -> TInt
    | TE_Name "float" -> TFloat
    | TE_Name "bool" -> TBool
    | TE_Name "char" -> TChar
    | TE_Name "string" -> TString
    | TE_Name id ->
        let tysym =
            try
                Symbol.lookup_tysym id
            with Not_found ->
                (try
                    Symbol.lookup_tysym_default id
                with Not_found -> error pos @@ "'" ^ id ^ "' not found")
        in
        tysym.tys.body
    | TE_Message (e, id) -> (*TODO*) failwith "TE_Message TODO"
    | TE_Var n -> TVar (n, {contents=None})
    | TE_Tuple el -> TTuple (List.map (typ_from_expr pos) el)
    | TE_Fun (e1, e2) -> TFun (typ_from_expr pos e1, typ_from_expr pos e2)
    | TE_Constr (e1, TE_Name "list") -> TList (typ_from_expr pos e1)
    | TE_Constr (e1, e2) ->
        let t1 = typ_from_expr pos e1 in
        let t2 = typ_from_expr pos e2 in
        TConstr (t1, t2)

let rec typ_from_decl pos id = function
    | TD_Alias e -> TAlias (id, typ_from_expr pos e)
    | TD_Record rl -> TAlias (id, TRecord (List.map (fun (s,b,e) -> (s,b, typ_from_expr pos e)) rl))
    | TD_Variant vl -> TAlias (id, TVariant (List.map (fun (s, oe) -> (s,
                                    match oe with
                                    | None -> None
                                    | Some e -> Some (typ_from_expr pos e))) vl))


let rec cons_append x y =
    match x with
    | VNil -> y
    | VCons (x, xs) -> VCons (x, cons_append xs y)
    | _ -> failwith "cons_append"

let rec cons_reverse = function
    | VNil -> VNil
    | VCons (x, xs) -> cons_append (cons_reverse xs) (VCons (x, VNil))
    | _ -> failwith "cons_reverse"

let string_to_cons s =
    let rec loop acc = function
        | "" -> cons_reverse acc
        | x -> loop (VCons (VChar (String.get x 0), acc)) (String.sub x 1 (String.length x - 1))
    in loop VNil s

let eval_unary pos = function
    | (UMinus, VInt n) -> VInt (-n)
    | (UMinus, VFloat f) -> VFloat (-.f)
    | (UNot, VBool b) -> VBool (not b)
    | _ -> error pos "type error (unary)"

let rec eval_add pos = function
    | (VNil, VNil) -> VNil
    | (VString l, VString r) -> VString (l ^ r)
    | (VString l, VNil) -> VString l
    | (VNil, VString r) -> VString r
    | (VCons _ as l, VString r) -> eval_add pos (l, string_to_cons r)
    | (VString l, (VCons _ as r)) -> eval_add pos (string_to_cons l, r)
    | (VCons _ as l, VNil) -> l
    | (VNil, (VCons _ as r)) -> r
    | (VCons _ as l, (VCons _ as r)) -> cons_append l r
    | (l,r) ->
        print_endline @@ "l=" ^ s_value l ^ ", r=" ^ s_value r;
        error pos "type error (binary add)"

let rec eval_shallow_equal pos = function
    | (VUnit, VUnit) -> true
    | (VNil, VNil) -> true
    | (VBool l, VBool r) -> (l == r)
    | (VInt l, VInt r) -> (l == r)
    | (VFloat l, VFloat r) -> Float.equal l r
    | (VChar l, VChar r) -> (l == r)
    | ((VString _ as l), (VString _ as r)) -> l == r
    | ((VCons _ as l), (VCons _ as r)) -> l == r
    | (VCons _, VNil) | (VNil, VCons _) -> false
    | (VString "", VNil) | (VNil, VString "") -> false
    | (VString _, VNil) | (VNil, VString _) -> false
    | (VTuple xl, VTuple yl) -> shallow_equal_tuple pos (xl, yl)
    | (lhs, rhs) -> error pos @@ "type error (shallow equal) " ^ s_value lhs ^ " & " ^ s_value rhs
and shallow_equal_tuple pos = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if not (eval_shallow_equal pos (x, y)) then false
        else shallow_equal_tuple pos (xs, ys)

let rec eval_deep_equal pos = function
    | (VUnit, VUnit) -> true
    | (VNil, VNil) -> true
    | (VBool l, VBool r) -> (l = r)
    | (VInt l, VInt r) -> (l = r)
    | (VFloat l, VFloat r) -> Float.equal l r
    | (VChar l, VChar r) -> (l = r)
    | (VString l, VString r) -> String.equal l r
    | (VCons (x,xs), VCons (y,ys)) ->
        (eval_deep_equal pos (x,y) && eval_deep_equal pos (xs,ys))
    | (VCons _, VNil) | (VNil, VCons _) -> false
    | (VString "", VNil) | (VNil, VString "") -> true
    | (VString _, VNil) | (VNil, VString _) -> false
    | (VTuple xl, VTuple yl) -> deep_equal_tuple pos (xl, yl)
    | (lhs, rhs) -> error pos @@ "type error (deep equal) " ^ s_value lhs ^ " & " ^ s_value rhs
and deep_equal_tuple pos = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if not (eval_deep_equal pos (x, y)) then false
        else deep_equal_tuple pos (xs, ys)

let eval_binary pos = function
    | (BinEq, vl, vr) -> VBool (eval_deep_equal pos (vl, vr))
    | (BinEql, vl, vr) -> VBool (eval_shallow_equal pos (vl, vr))
    | (BinNeq, vl, vr) -> VBool (not (eval_deep_equal pos (vl, vr)))
    | (BinNeql, vl, vr) -> VBool (not (eval_shallow_equal pos (vl, vr)))
    | (BinAdd, VInt l, VInt r) -> VInt (l + r)
    | (BinSub, VInt l, VInt r) -> VInt (l - r)
    | (BinMul, VInt l, VInt r) -> VInt (l * r)
    | (BinDiv, VInt l, VInt r) ->
        if r = 0 then error pos "division by zero"
        else VInt (l / r)
    | (BinMod, VInt l, VInt r) ->
        if r = 0 then error pos "division by zero"
        else VInt (l mod r)
    | (BinLT, VInt l, VInt r) -> VBool (l < r)
    | (BinLE, VInt l, VInt r) -> VBool (l <= r)
    | (BinGT, VInt l, VInt r) -> VBool (l > r)
    | (BinGE, VInt l, VInt r) -> VBool (l >= r)
    | (BinAdd, VFloat l, VFloat r) -> VFloat (l +. r)
    | (BinSub, VFloat l, VFloat r) -> VFloat (l -. r)
    | (BinMul, VFloat l, VFloat r) -> VFloat (l *. r)
    | (BinDiv, VFloat l, VFloat r) ->
        if Float.equal r 0.0 then error pos "division by zero"
        else VFloat (l /. r)
    | (BinLT, VFloat l, VFloat r) -> VBool (l < r)
    | (BinLE, VFloat l, VFloat r) -> VBool (l <= r)
    | (BinGT, VFloat l, VFloat r) -> VBool (l > r)
    | (BinGE, VFloat l, VFloat r) -> VBool (l >= r)
    | (BinLT, VChar l, VChar r) -> VBool (l < r)
    | (BinLE, VChar l, VChar r) -> VBool (l <= r)
    | (BinGT, VChar l, VChar r) -> VBool (l > r)
    | (BinGE, VChar l, VChar r) -> VBool (l >= r)
    | (BinLT, VString l, VString r) -> VBool (l < r)
    | (BinLE, VString l, VString r) -> VBool (l <= r)
    | (BinGT, VString l, VString r) -> VBool (l > r)
    | (BinGE, VString l, VString r) -> VBool (l >= r)
    | (BinCons, vl, vr) -> VCons (vl, vr)
    | (BinAdd, vl, vr) -> eval_add pos (vl, vr)
    | _ -> error pos "type error (binary)"


let rec eval e =
    debug_in "eval";
    let res =
        match e with
        | (EEof, _) ->
            debug_print "eval eof";
            VUnit
        | (EUnit, _) ->
            debug_print "eval unit";
            VUnit
        | (ENil, _) ->
            debug_print "eval nil";
            VNil
        | (EBool b, _) ->
            debug_print "eval bool";
            VBool b
        | (EInt n, _) ->
            debug_print "eval int";
            VInt n
        | (EFloat f, _) ->
            debug_print "eval float";
            VFloat f
        | (EChar c, _) ->
            debug_print "eval char";
            VChar c
        | (EString s, _) ->
            debug_print "eval string";
            VString s
        | (EId s, pos) ->
            debug_print @@ "eval Id '" ^ s ^ "'";
            let sym =
                try
                    Symbol.lookup s
                with Not_found ->
                    (try
                        Symbol.lookup_default s
                    with Not_found -> error pos @@ "'" ^ s ^ "' not found")
            in sym.v
        | (ERecord rl, _) ->
            debug_print @@ "eval record " ^ s_expr e;
            (*TODO is_mutable をどうするか*)
            VRecord (List.map (fun (s,e) -> (s, ref (eval e))) rl)
        | (ETuple el, _) ->
            debug_print @@ "eval tuple " ^ s_expr e;
            VTuple (List.map (fun x -> eval x) el)
        | (EUnary (op, e), pos) ->
            debug_print @@ "eval unary " ^ s_unop op ^ " " ^ s_expr e;
            let v = eval e in
            eval_unary pos (op, v)
        | (EBinary (BinLOr, lhs, rhs), _) ->
            debug_print @@ "eval binary BinLOr, " ^ s_expr lhs ^ ", " ^ s_expr rhs;
            let vl = eval lhs in
            if vl = VBool true then VBool true
            else eval rhs
        | (EBinary (BinLAnd, lhs, rhs), _) ->
            debug_print @@ "eval binary BinLAnd, " ^ s_expr lhs ^ ", " ^ s_expr rhs;
            let vl = eval lhs in
            if vl = VBool false then VBool false
            else eval rhs
        | (EBinary (op, lhs, rhs), pos) ->
            debug_print @@ "eval binary " ^ s_binop op ^ ", " ^ s_expr lhs ^ ", " ^ s_expr rhs;
            let vl = eval lhs in
            let vr = eval rhs in
            eval_binary pos (op, vl, vr)
        | (EApply (fn, arg), pos) ->
            debug_print @@ "eval apply " ^ s_expr fn ^ ", " ^ s_expr arg;
            let fn_part = eval fn in
            let v =
                match fn_part with
                | VClosure ((EId "_", _), body, old_env)
                | VClosure ((EUnit, _), body, old_env) ->
                    let ctx = Symbol.enter_new_env old_env in
                    let res = eval body in
                    Symbol.leave_env ctx;
                    res
                | VClosure ((EId id, _), body, old_env) ->
                    let arg_part = eval arg in
                    let ctx = Symbol.enter_new_env old_env in
                    let sym = { v = arg_part; is_mutable = false } in
                    Symbol.insert_sym id sym;
                    let res = eval body in
                    Symbol.leave_env ctx;
                    res
                | VBuiltin fn -> fn pos arg
                | x -> error pos @@ "application of non-function: " ^ s_value x
            in
            v
        | (ELet (el, body), pos) ->
            debug_print @@ "eval let ... in " ^ s_expr body;
            let ctx = Symbol.enter_new_env [] in
            eval_let el;
            let res = eval body in
            Symbol.leave_env ctx;
            res
        | (EValDef (ism, id, e), pos) ->
            debug_print @@ "eval defval " ^ id ^ " = " ^ s_expr e;
            let v = eval e in
            let sym = { v = v; is_mutable = ism } in
            Symbol.insert_sym id sym;
            VUnit
        | (EFuncDef (id, e), pos) ->
            debug_print @@ "eval defun " ^ id ^ " = " ^ s_expr e;
            let sym = { v = VUnit; is_mutable = false } in
            Symbol.insert_sym id sym;
            sym.v <- eval e;
            VUnit
        | (ELambda (arg, body), _) ->
            debug_print @@ "eval lambda " ^ s_expr arg ^ " -> " ^ s_expr body;
            VClosure (arg, body, Symbol.get_current_env())
        | (ECond (cond_e, then_e, else_e), pos) ->
            debug_print @@ "eval cond "
                ^ s_expr cond_e ^ ", " ^ s_expr then_e ^ ", " ^ s_expr else_e;
            let vc = eval cond_e in
            if vc = VBool true then
                eval then_e
            else
                eval else_e
        | (EAssign (e1, e2), pos) ->
            debug_print @@ "eval assign " ^ s_expr e1 ^ " <- " ^ s_expr e2;
            begin
                match e1 with
                | (EId s, pos) ->
                    debug_print @@ "eval assign '" ^ s ^ "'";
                    let sym =
                        try
                            Symbol.lookup s
                        with Not_found -> error pos @@ "'" ^ s ^ "' not found"
                    in
                    if sym.is_mutable then
                        let v = eval e2 in
                        sym.v <- v
                    else
                        error pos @@ "'" ^ s ^ "' is not mutable"
                | _ ->
                    error pos "left value required"
            end;
            VUnit
        | (EMessage (e, id), pos) ->
            debug_print @@ "eval message " ^ s_expr e ^ "." ^ id;
            let v = eval e in
            begin
                match v with
                | VModule modu ->
                    (try 
                        let sym = Symbol.lookup_from_module modu id in
                        sym.v
                    with Not_found -> error pos @@ "unknown symbol '" ^ id ^ "'")
                | _ ->
                    (*TODO object message *)
                    VUnit
            end
        | (EBlock el, pos) ->
            debug_print @@ "eval block " ^ s_list s_expr "; " el;
            let ctx = Symbol.enter_new_env [] in
            let res = eval_list el in
            Symbol.leave_env ctx;
            res
        | (ESeq el, pos) ->
            debug_print @@ "eval seq " ^ s_list s_expr "; " el;
            eval_list el;
        | (EModule mid, _) ->
            debug_print @@ "eval module " ^ mid;
            Symbol.set_module mid;
            VUnit
        | (EImport (mid, aid), pos) ->
            debug_print @@ "eval import " ^ mid;
            VUnit
        | (ETypeDecl (tvs, id, tyd), pos) ->
            debug_print @@ "type decl [" ^ s_list string_of_int "," tvs ^ "] " ^ id ^ " = " ^ s_typ_decl tyd;
            let ty = typ_from_decl pos id tyd in
            let sym = { v = VType ty; is_mutable = false } in
            Symbol.insert_sym id sym;
            VUnit
        | (EDecl (id, tye), _) ->
            debug_print @@ "decl " ^ id ^ " : " ^ s_typ_expr tye;
            VUnit
    in
    debug_out @@ "eval > " ^ s_value res;
    res

and eval_let = function
    | [] -> ()
    | x::xs ->
        ignore @@ eval x;
        eval_let xs

and eval_list = function
    | [] -> VUnit
    | x :: xs ->
        let v = eval x in
        if xs = [] then
            v
        else
            eval_list xs

