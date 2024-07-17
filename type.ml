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



let rec s_typ_raw ty =
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TInt -> (5, "int") | TFloat -> (5, "float")
            | TBool -> (5, "bool") | TChar -> (5, "char") | TString -> (5, "string")
            | TModule s -> (5, s)
            | TConstr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " " ^ s2)
            | TList t -> (3, to_s 0 t ^ " list")
            | TTuple tl -> (3, s_list (to_s 4) " * " tl)
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents=None}) ->
                (5, "'" ^ string_of_int x)
            | TVar (_, {contents=Some t}) ->
                (3, "<" ^ to_s n t ^ ">")
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let get_pos x = (snd x)


let rec equal t1 t2 =
    match (t1, t2) with
    | (a, b) when a == b -> true
    | (TConstr (t11, t12), TConstr (t21, t22)) -> equal t11 t21 && equal t12 t22
    | (TList TChar, TString) | (TString, TList TChar) -> true
    | (TList t1, TList t2) -> equal t1 t2
    | (TTuple tl1, TTuple tl2) -> list_equal (tl1, tl2)
    | (TFun (t11, t12), TFun (t21, t22)) -> equal t11 t21 && equal t12 t22
    | (TVar (n, {contents=None}), TVar (m, {contents=None})) -> n = m
    | (TVar (_, {contents=None}), _) | (_, TVar (_, {contents=None})) -> true
    | (TVar (_, {contents=Some t1'}), _) -> equal t1' t2
    | (_, TVar (_, {contents=Some t2'})) -> equal t1 t2'
    | _ -> false
and list_equal = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if equal x y then
            list_equal (xs, ys)
        else false

let reloc_tvar t =
    let seed = ref 0 in
    let new_tvar () =
        let ty = TVar (!seed, ref None) in
        incr seed;
        ty
    in
    let mappings = ref [] in
    let rec conv t =
        match t with
        | TUnit | TInt | TFloat | TBool | TChar | TString | TModule _ -> t
        | TConstr (t1, t2) -> TConstr (conv t1, conv t2)
        | TList t -> TList (conv t)
        | TTuple tl -> TTuple (List.map conv tl)
        | TFun (t1, t2) -> TFun (conv t1, conv t2)
        | TVar (_, {contents = Some t}) -> TVar (0, ref (Some (conv t)))
        | TVar (n, {contents = None}) ->
            (try
                List.assoc n !mappings
            with Not_found -> begin
                let ty = new_tvar() in
                mappings := (n, ty) :: !mappings;
                ty
            end)
    in
    conv t

let decl_equal t1 t2 =
    let t1' = reloc_tvar t1 in
    let t2' = reloc_tvar t2 in
    equal t1' t2'

let rec is_type t1 t2 =
    match (t1, t2) with
    | (TVar (_, {contents=Some t1'}), _) -> is_type t1' t2
    | (_, TVar (_, {contents=Some t2'})) -> is_type t1 t2'
    | (TConstr (t11,t12), TConstr (t21,t22)) -> is_type t11 t21 && is_type t12 t22
    | (TList TChar, TString) | (TString, TList TChar) -> true
    | (TList t1, TList t2) -> is_type t1 t2
    | (TTuple tl1, TTuple tl2) -> list_is_type (tl1, tl2)
    | (TFun (t11,t12), TFun (t21,t22)) -> is_type t11 t21 && is_type t12 t22
    | _ when t1 = t2 -> true
    | _ -> false
and list_is_type = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if is_type x y then
            list_is_type (xs, ys)
        else false

let is_type_in t tl = List.exists (fun t2 -> is_type t t2) tl

let is_list = function | TList _ -> true | _ -> false

let is_tvar t =
    match t with
    | TVar _ -> true
    | _ -> false

let rec type_var_equal t1 t2 =
    match (t1, t2) with
    | (TVar (n, {contents = None}), TVar (m, {contents = None})) when n = m
        -> true
    | (TVar (_, {contents = Some t1'}), _)
        -> type_var_equal t1' t2
    | (_, TVar (_, {contents = Some t2'}))
        -> type_var_equal t1 t2'
    | _ -> false

let rec occurs_in_type t t2 =
    if type_var_equal t t2 then true
    else
        match t2 with
        | TList t' -> occurs_in_type t t'
        | TTuple tl -> occurs_in t tl
        | TConstr (tf1, tf2) -> occurs_in t [tf1;tf2]
        | TFun (tf1, tf2) -> occurs_in t [tf1;tf2]
        | _ -> false

and occurs_in t types =
    List.exists (fun t2 -> occurs_in_type t t2) types

let rec unify t1 t2 pos =
    debug_in @@ "unify: " ^ s_typ_raw t1 ^ ", " ^ s_typ_raw t2;
    (match (t1, t2) with
    | (TUnit, TUnit) | (TInt, TInt) | (TFloat, TFloat) | (TBool, TBool)
    | (TChar, TChar) | (TString, TString) | (TList TChar, TString)
    | (TString, TList TChar) -> ()
    | (TList t, TString) | (TString, TList t) ->
        unify t TChar pos
    | (TList tl, TList tr) ->
        unify tl tr pos
    | (TTuple tl1, TTuple tl2) when List.length tl1 = List.length tl2 ->
        List.iter2 (fun x y -> unify x y pos) tl1 tl2
    | (TConstr (t11,t12), TConstr (t21,t22)) ->
        unify t11 t21 pos;
        unify t12 t22 pos
    | (TFun (t11,t12), TFun (t21,t22)) ->
        unify t11 t21 pos;
        unify t12 t22 pos
    | (TVar (n1, {contents=None}), TVar (n2, {contents=None})) when n1 = n2 -> ()
    | (TVar (_, {contents = Some t1'}), _) -> unify t1' t2 pos
    | (_, TVar (_, {contents = Some t2'})) -> unify t1 t2' pos
    | (TVar (_, ({contents = None} as r1)), _) ->
        if occurs_in_type t1 t2 then
            error pos @@ "type circularity between " ^ s_typ t1 ^ " and " ^ s_typ t2
        else begin
            debug_print @@ "unify result " ^ s_typ_raw t1 ^ " ... ";
            r1 := Some t2;
            debug_print @@ "... " ^ s_typ_raw t1
        end
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error pos @@ "type circularity between " ^ s_typ t2 ^ " and " ^ s_typ t1
        else begin
            debug_print @@ "unify result " ^ s_typ_raw t2 ^ " ... ";
            r2 := Some t1;
            debug_print @@ "... " ^ s_typ_raw t2
        end
    | (_, _) -> error pos @@ "type mismatch between " ^ s_typ t2 ^ " and " ^ s_typ t1);
    debug_out @@ "unify"


let rec free_tyvars = function
    | TList t -> free_tyvars t
    | TTuple tl -> List.fold_left (fun fvs x -> fvs @ free_tyvars x) [] tl
    | TConstr (t1, t2) -> free_tyvars t1 @ free_tyvars t2
    | TFun (t1, t2) -> free_tyvars t1 @ free_tyvars t2
    | TVar (x, {contents=None}) -> [x]
    | TVar (_, {contents=Some t}) -> free_tyvars t
    | _ -> []

let generalize ty =
    let tenv = Symbol.get_current_tenv() in
    let free_vars = List.filter
        (fun x -> not (List.mem x (List.concat (List.map (fun (_, tysym) -> tysym.tys.vars) tenv))))
        (free_tyvars ty)
    in
    make_typ_scheme free_vars ty

let rec substitute subst = function
    | TList t -> TList (substitute subst t)
    | TTuple tl -> TTuple (List.map (fun x -> substitute subst x) tl)
    | TConstr (t1, t2) -> TConstr (substitute subst t1, substitute subst t2)
    | TFun (t1, t2) -> TFun (substitute subst t1, substitute subst t2)
    | TVar (x, {contents=None}) as t -> (try List.assoc x subst with Not_found -> t)
    | TVar (_, {contents=Some t}) -> substitute subst t
    | t -> t

let instantiate tys =
    let subst = List.map (fun x -> (x, new_tvar())) tys.vars in
    substitute subst tys.body

let infer_unary op t pos =
    match op with
    | UNot ->
        if is_type t TBool then TBool
        else error pos @@ "This expression has type " ^ s_typ t ^
            " but an expression was expected of type int"
    | UMinus ->
        if is_type t TInt then TInt
        else if is_type t TFloat then TFloat
        else error pos @@ "This expression has type " ^ s_typ t ^
            " but an expression was expected of type int/float"

let infer_binary op tl tr pos =
    match op with
    | BinAdd | BinSub | BinMul | BinDiv | BinMod ->
        unify tl tr pos;
        if is_type_in tl [TInt;TFloat;TString] || is_tvar tl || is_list tl then tl
        else
            error pos @@ "The expression has type " ^ s_typ tl ^
                        " but an expression was expected of type int/float/string/list"
    | BinLT | BinLE | BinGT | BinGE ->
        unify tl tr pos;
        if is_type_in tl [TChar;TInt;TFloat;TString;TList TChar]
            || is_tvar tl then TBool
        else error pos @@ "The expression has type " ^ s_typ tl ^
                        " but an expression was expected of type char/int/float/string/list"
    | BinEq | BinNeq | BinEql | BinNeql ->
        unify tl tr pos;
        TBool;
    | BinLOr | BinLAnd ->
        unify TBool tl pos;
        unify TBool tr pos;
        TBool
    | BinCons ->
        unify (TList tl) tr pos;
        tr


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

let rec typ_from_decl pos = function
    | TD_Alias e -> typ_from_expr pos e
    | TD_Record _ -> (*TODO*) failwith "TD_Record TODO"
    | TD_Variant _ -> (*TODO*) failwith "TD_Variant TODO"

let rec infer e = 
    debug_in "infer";
    let res =
        match e with
        | (EEof, _) ->
            debug_print "infer eof";
            TUnit
        | (EUnit, _) ->
            debug_print "infer unit";
            TUnit
        | (ENil, _) ->
            debug_print "infer nil";
            TList (new_tvar ())
        | (EBool _, _) ->
            debug_print "infer bool";
            TBool
        | (EInt _, _) ->
            debug_print "infer int";
            TInt
        | (EFloat _, _) ->
            debug_print "infer float";
            TFloat
        | (EChar _, _) ->
            debug_print "infer char";
            TChar
        | (EString _, _) ->
            debug_print "infer string";
            TString
        | (EId s, pos) ->
            debug_print @@ "infer Id '" ^ s ^ "'";
            let tysym =
                try
                    Symbol.lookup_tysym s
                with Not_found -> 
                    (try
                        Symbol.lookup_tysym_default s
                    with Not_found -> error pos @@ "'" ^ s ^ "' not found")
            in
            instantiate tysym.tys
        | (ETuple el, _) ->
            debug_print "infer tuple";
            TTuple (List.map (fun x -> infer x) el)
        | (EUnary (op, e), pos) ->
            debug_print "infer unary";
            let t = infer e in
            infer_unary op t pos
        | (EBinary (op, l, r), pos) ->
            debug_print @@
                "infer binary '" ^ s_binop op ^ "', " ^ s_expr l ^ ", " ^ s_expr r;
            let tl = infer l in
            let tr = infer r in
            infer_binary op tl tr pos
        | (EApply (fn, arg), pos) ->
            debug_print @@ "infer apply " ^ s_expr fn ^ ", " ^ s_expr arg;
            let t_fn = infer fn in
            let t_arg = infer arg in
            let t = new_tvar () in
            unify t_fn (TFun (t_arg, t)) pos;
            t
        | (ELet (el, body), pos) ->
            debug_print @@ "infer let ... in " ^ s_expr body;
            let ctx = Symbol.enter_new_tenv () in
            ignore @@ infer_list el;
            let t = infer body in
            Symbol.leave_tenv ctx;
            t
        | (EValDef (ism, id, e), pos) ->
            debug_print @@ "infer defval " ^ id ^ " = " ^ s_expr e;
            (try
                let tysym = Symbol.lookup_tysym id in
                let t = infer e in
                if not (decl_equal tysym.tys.body t) then
                    error pos @@ "Type mismatch between " ^ s_typ tysym.tys.body
                                    ^ " and " ^ s_typ t ^ " (let)";
                TUnit
            with Not_found ->
                let t = infer e in
                let tys = generalize t in
                let tysym = { tys = tys; is_mutable = ism } in
                Symbol.insert_tysym id tysym;
                TUnit)
        | (EFuncDef (id, e), pos) ->
            debug_print @@ "infer defun " ^ id ^ " = " ^ s_expr e;
            (try
                let tysym = Symbol.lookup_tysym id in
                let t = infer e in
                if not (decl_equal tysym.tys.body t) then
                    error pos @@ "Type mismatch between " ^ s_typ tysym.tys.body ^ " and " ^ s_typ t; TUnit
            with Not_found ->
                let tys = generalize (new_tvar()) in
                let tysym = { tys = tys; is_mutable = false } in
                Symbol.insert_tysym id tysym;
                let t = infer e in
                tysym.tys <- generalize t;
                TUnit)
        | (ELambda ((EUnit,_), body), _) ->
            debug_print @@ "infer lambda () -> " ^ s_expr body;
            let t_body = infer body in
            TFun (TUnit, t_body)
        | (ELambda ((EId "_", _), body), _) ->
            debug_print @@ "infer lambda _ -> " ^ s_expr body;
            let t_arg = new_tvar () in
            let t_body = infer body in
            TFun (t_arg, t_body)
        | (ELambda ((EId arg, _), body), pos) ->
            debug_print @@ "infer lambda " ^ arg ^ " -> " ^ s_expr body;
            let t_arg = new_tvar () in
            let tys = make_typ_scheme [] t_arg in
            let ctx = Symbol.enter_new_tenv () in
            let tysym = { tys = tys; is_mutable = false } in
            Symbol.insert_tysym arg tysym;
            let t_body = infer body in
            let res = TFun (t_arg, t_body) in
            Symbol.leave_tenv ctx;
            res
        | (ELambda _, pos) -> error pos "lambda syntax error"
        | (ECond (cond_e, then_e, else_e), pos) ->
            debug_print @@ "infer cond "
                ^ s_expr cond_e ^ ", " ^ s_expr then_e ^ ", " ^ s_expr else_e;
            let t_cond = infer cond_e in
            unify TBool t_cond pos;
            let t_then = infer then_e in
            let t_else = infer else_e in
            unify t_then t_else pos;
            t_then
        | (EAssign (e1, e2), pos) ->
            debug_print @@ "infer assign " ^ s_expr e1 ^ " <- " ^ s_expr e2;
            let t1 = infer e1 in
            let t2 = infer e2 in
            unify t1 t2 pos;
            TUnit
        | (EMessage (e, id), pos) ->
            debug_print @@ "infer message " ^ s_expr e ^ "." ^ id;
            let t = infer e in
            begin
                match t with
                | TModule name ->
                    (try
                        let modu = Symbol.lookup_module name in
                        (try
                            let tysym = Symbol.lookup_tysym_from_module modu id in
                            tysym.tys.body
                        with Not_found -> error pos @@ "unknown Symbol '" ^ id ^ "'")
                    with Not_found -> error pos @@ "unknown module '" ^ name ^ "'")
                | _ ->
                    (*TODO object message *)
                    TUnit
            end
        | (EBlock el, _) ->
            debug_print @@ "infer block " ^ s_list s_expr "; " el;
            let ctx = Symbol.enter_new_tenv () in
            let res = infer_list el in
            Symbol.leave_tenv ctx;
            res
        | (ESeq el, _) ->
            debug_print @@ "infer seq " ^ s_list s_expr "; " el;
            infer_list el
        | (EModule mid, _) ->
            debug_print @@ "infer module " ^ mid;
            Symbol.set_module mid;
            TUnit
        | (EImport (mid, aid), _) ->
            debug_print @@ "infer import " ^ mid;
            load_module mid aid;
            TUnit
        | (ETypeDecl (tvs, id, tyd), pos) ->
            debug_print @@ "type decl [" ^ s_list string_of_int "," tvs ^ "] " ^ id ^ " = " ^ s_typ_decl tyd;
            let ty = typ_from_decl pos tyd in
            let tys = generalize ty in
            let tysym = { tys = tys; is_mutable = false } in
            Symbol.insert_tysym id tysym;
            TUnit
        | (EDecl (id, tye), pos) ->
            debug_print @@ "decl " ^ id ^ " : " ^ s_typ_expr tye;
            let ty = typ_from_expr pos tye in
            (try
                let tysym = Symbol.lookup_tysym id in
                if not (decl_equal tysym.tys.body ty) then
                    error pos @@ "Type mismatch between " ^ s_typ tysym.tys.body
                                    ^ " and " ^ s_typ ty ^ " (decl)";
                TUnit
            with Not_found ->
                let tys = generalize ty in
                let tysym = { tys = tys; is_mutable = false } in
                Symbol.insert_tysym id tysym;
                TUnit)

    in
    debug_out @@ "infer > " ^ s_typ_raw res;
    res

and infer_list = function
    | [] -> TUnit
    | x::[] -> infer x
    | x::xs ->
        let t = infer x in
        if not (is_type t TUnit) then
            error (get_pos x) @@ "expression should have type unit (at '" ^ s_typ_raw t ^ "')";
        infer_list xs

and load_module mid aid =
    debug_print @@ "load_module '" ^ mid ^ "'"
        ^ (match aid with Some id -> " as '" ^ id ^ "'" | None -> "");
    if not (Symbol.exist_module mid aid) then begin
        let filename = module_name_to_filename mid in
        if load_source filename then begin
            match aid with
            | Some id -> Symbol.rename_module mid id
            | None -> ()
        end
    end

and load_source filename =
    verbose @@ "load '" ^ filename ^ "'...";
    try
        let modu_name = filename_to_module_name filename in
        Symbol.set_module modu_name;
        let e = Parser.parse_file filename in
        let t = infer e in
        let v = Eval.eval e in
        verbose @@ s_value v ^ " : " ^ s_typ t;
        Symbol.set_default_module();
        true
    with
        | Error (pos, msg) -> print_endline @@ s_pos pos ^ "Error: " ^ msg; false
        | End_of_file -> (); print_endline "End_of_file"; false
        | Sys_error s -> print_endline s; false

