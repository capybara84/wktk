open Syntax

let debug_scope_flag = ref false
let debug_indent = ref 0

let rec debug_show_space = function
    | 0 -> ()
    | n -> print_char ' '; debug_show_space (n-1)

let debug_eval s =
    if !debug_scope_flag then
        (debug_show_space !debug_indent; print_endline s)

let debug_eval_in s =
    if !debug_scope_flag then
        (debug_eval @@ "IN " ^ s; incr debug_indent)

let debug_eval_out s =
    if !debug_scope_flag then
        (decr debug_indent; debug_eval @@ "OUT " ^ s)

let mod_lookup ml s env =
    match ml with
    | [x] ->
        let modu = Symbol.lookup_module x in
        !(Env.lookup s modu.env)
    | _ ->
        (*TODO*)
        raise Not_found

let rec eval env e =
    debug_eval_in @@ "eval: " ^ s_expr e;
    let rec eval_equal pos = function
        | (VUnit, VUnit) -> true
        | (VNull, VNull) -> true
        | (VBool l, VBool r) -> (l = r)
        | (VInt l, VInt r) -> (l = r)
        | (VChar l, VChar r) -> (l = r)
        | (VFloat l, VFloat r) -> (l = r)
        | (VString l, VString r) -> (l = r)
        | (VTuple xl, VTuple yl) -> equal_tuple pos (xl, yl)
        | ((VCons _ as lhs), (VCons _ as rhs)) -> list_equal pos (lhs, rhs)
        | (VCons _, VNull) | (VNull, VCons _) -> false
        | (VString s, (VCons _ as cs))
        | ((VCons _ as cs), VString s) -> str_equal pos (s, cs)
        | (VString "", VNull) | (VNull, VString "") -> true
        | (VString _, VNull) | (VNull, VString _) -> false
        | (lhs, rhs) -> error pos @@ "type error (equal) " (* ^ s_value_src lhs ^ " & " ^ s_value_src rhs *)
    and equal_tuple pos = function
        | ([], []) -> true
        | (_, []) | ([], _) -> false
        | (x::xs, y::ys) ->
            if not (eval_equal pos (x, y)) then false
            else equal_tuple pos (xs, ys)
    and list_equal pos = function
        | (VNull, VNull) -> true
        | (VCons _, VNull) | (VNull, VCons _) -> false
        | (VCons (lh, lt), VCons (rh, rt)) ->
            if not (eval_equal pos (lh, rh)) then false
            else list_equal pos (lt, rt)
        | _ -> failwith "list_equal bug"
    and str_equal pos = function
        | ("", VNull) -> true
        | (_, VNull) -> false
        | ("", VCons _) -> false
        | (x, VCons (VChar c, rest)) ->
            if String.get x 0 <> c then false
            else
                str_equal pos ((String.sub x 1 (String.length x - 1)), rest)
        | _ -> failwith "str_equal bug"
    in
    let eval_unary pos = function
        | (UMinus, VInt n) -> VInt (-n)
        | (UMinus, VFloat f) -> VFloat (-.f)
        | (UNot, VBool b) -> VBool (not b)
        | _ -> error pos "type error (unary)"
    in
    let eval_binary pos = function
        | (BinEql, vl, vr) -> VBool (eval_equal pos (vl, vr))
        | (BinNeq, vl, vr) -> VBool (not (eval_equal pos (vl, vr)))
        | (BinAdd, VInt l, VInt r) -> VInt (l + r)
        | (BinSub, VInt l, VInt r) -> VInt (l - r)
        | (BinMul, VInt l, VInt r) -> VInt (l * r)
        | (BinDiv, VInt l, VInt r) -> VInt (l / r)
        | (BinMod, VInt l, VInt r) -> VInt (l mod r)
        | (BinLT, VInt l, VInt r) -> VBool (l < r)
        | (BinLE, VInt l, VInt r) -> VBool (l <= r)
        | (BinGT, VInt l, VInt r) -> VBool (l > r)
        | (BinGE, VInt l, VInt r) -> VBool (l >= r)
        | (BinAdd, VFloat l, VFloat r) -> VFloat (l +. r)
        | (BinSub, VFloat l, VFloat r) -> VFloat (l -. r)
        | (BinMul, VFloat l, VFloat r) -> VFloat (l *. r)
        | (BinDiv, VFloat l, VFloat r) -> VFloat (l /. r)
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
        | _ -> error pos "type error (binary)"
    in
    let rec eval_list pos env = function
        | [] -> (env, VUnit)
        | x::xs ->
            let (new_env, v) = eval env x in
            if xs == [] then
                (new_env, v)
            else if v <> VUnit then
                error pos ("() required")
            else
                eval_list pos new_env xs
    in
    let res =
        match e with
        | (ENull, _) ->
            debug_eval "eval Null";
            (env, VNull)
        | (EUnit, _) ->
            debug_eval "eval Unit";
            (env, VUnit)
        | (ELit (Bool b), _) ->
            debug_eval @@ "eval bool";
            (env, VBool b)
        | (ELit (Int n), _) ->
            debug_eval @@ "eval int";
            (env, VInt n)
        | (ELit (Char c), _) ->
            debug_eval @@ "eval char";
            (env, VChar c)
        | (ELit (Float f), _) ->
            debug_eval @@ "eval float";
            (env, VFloat f)
        | (ELit (String s), _) ->
            debug_eval @@ "eval string";
            (env, VString s)
        | (EId s, pos) ->
            debug_eval @@ "eval Id " ^ s;
            let v =
                (try
                    !(Env.lookup s env)
                with Not_found ->
                    (try
                        !(Symbol.lookup_default s)
                    with Not_found -> error pos @@ "'" ^ s ^ "' not found"))
            in
            (env, v)
        | (EModId (ml, s), pos) ->
            let v =
                try
                    mod_lookup ml s env
                with Not_found -> error pos @@ "'" ^ s_list id "." ml ^ "." ^ s ^ "' not found"
            in
            (env, v)
        | (ETuple el, _) ->
            debug_eval @@ "eval tuple " ^ s_expr e;
            (env, VTuple (List.map (fun e -> let (_, v) = eval env e in v) el))
        | (EParen e, _) ->
            debug_eval @@ "eval paren " ^ s_expr e;
            eval env e
        | (EUnary (op, e), pos) ->
            debug_eval @@ "eval unary '" ^ s_unop op ^ "' , " ^ s_expr e;
            let (env, v) = eval env e in
            (env, eval_unary pos (op, v))
        | (EBinary (BinLOr, l, r), _) ->
            debug_eval @@ "eval binary '||', " ^ s_expr l ^ ", " ^ s_expr r;
            let (env, vl) = eval env l in
            if vl = VBool true then (env, VBool true)
            else eval env r
        | (EBinary (BinLAnd, l, r), _) ->
            debug_eval @@ "eval binary '&&', " ^ s_expr l ^ ", " ^ s_expr r;
            let (env, vl) = eval env l in
            if vl = VBool false then (env, VBool false)
            else eval env r
        | (EBinary (BinOp op, l, r), pos) ->
            (*TODO*)
            (env, VInt 0)
        | (EBinary (op, l, r), pos) ->
            debug_eval @@ "eval binary '" ^ s_binop op ^ "', " ^ s_expr l ^ ", " ^ s_expr r;
            let (env, vl) = eval env l in
            let (env, vr) = eval env r in
            (env, eval_binary pos (op, vl, vr))
        | (ECond (cond_e, then_e, else_e), _) ->
            debug_eval @@ "eval cond " ^ s_expr cond_e ^ ", " ^ s_expr then_e ^ ", " ^ s_expr else_e;
            let (env, vc) = eval env cond_e in
            if vc = VBool true then
                eval env then_e
            else
                eval env else_e
        | (ELambda (arg, body), _) ->
            debug_eval @@ "eval lambda " ^ s_expr arg ^ " -> " ^ s_expr body;
            (env, VClosure (arg, body, env))
        | (EApply (fn, arg), pos) ->
            debug_eval @@ "eval apply " ^ s_expr fn ^ ", " ^ s_expr arg;
            let (_, fn_part) = eval env fn in
            let (_, arg_part) = eval env arg in
            let (_, v) =
                match fn_part with
                | VClosure ((EId "_", _), body, old_env) ->
                    eval old_env body
                | VClosure ((EUnit, _), body, old_env) ->
                    eval old_env body
                | VClosure ((EId x, _), body, old_env) ->
                    let new_env = Env.extend x (ref arg_part) old_env in
                    eval new_env body
                | VBuiltin fn ->
                    fn pos env arg_part
                | v -> error pos @@ "application of non-function: " ^ s_value v
            in
            (env, v)
        | (ELet (id, e), _) ->
            debug_eval @@ "eval let " ^ id ^ " = " ^ s_expr e;
            let (env, v) = eval env e in
            let new_env = Env.extend id (ref v) env in
            (new_env, VUnit)
        | (ELetRec (id, e), _) ->
            debug_eval @@ "eval letrec " ^ id ^ " = " ^ s_expr e;
            let r = ref VUnit in
            let new_env = Env.extend id r env in
            let (env, v) = eval new_env e in
            r := v;
            (new_env, VUnit)
        | (ESeq el, pos) ->
            debug_eval @@ "eval seq " ^ s_list s_expr "; " el;
            let (_, v) = eval_list pos env el in
            (env, v)
        | (EModule mid, _) ->
            debug_eval @@ "eval module " ^ mid;
            (env, VUnit)
        | (EImport (mid, _), _) ->
            debug_eval @@ "eval import " ^ mid;
            (env, VUnit)
    in
    debug_eval_out @@ "eval = " ^ s_value @@ snd res;
    res

let eval_top e =
    let env = Symbol.get_current_env () in
    let (env, v) = eval env e in
    Symbol.set_current_env env;
    v

