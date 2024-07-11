
open Ast

let fn_ignore _ _ =
    VUnit

let fn_newline _ _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_head pos arg =
    let arg_v = Eval.eval arg in
    match arg_v with
    | VCons (x, _) -> x
    | VString s ->
        if s = "" then VNil
        else VChar (String.get s 0)
    | _ -> error pos "type error ('list' required)"

let fn_tail pos arg =
    let arg_v = Eval.eval arg in
    match arg_v with
    | VCons (_, xs) -> xs
    | VString s ->
        let len = String.length s in
        if len > 0 then VString (String.sub s 1 (len-1))
        else error pos "tail: null string"
    | _ -> error pos "type error ('list' required)"

let fn_fst pos arg =
    let arg_v = Eval.eval arg in
    match arg_v with
    | VTuple (x::_) -> x
    | VTuple _ -> error pos "tuple error"
    | _ -> error pos "type error ('tuple' required)"

let fn_snd pos arg =
    let arg_v = Eval.eval arg in
    match arg_v with
    | VTuple (_::x::_) -> x
    | VTuple _ -> error pos "tuple error"
    | _ -> error pos "type error ('tuple' required)"


let fn_print pos arg =
    let arg_v = Eval.eval arg in
    print_string (s_value arg_v);
    VUnit

let fn_println pos arg =
    let arg_v = Eval.eval arg in
    print_endline (s_value arg_v);
    VUnit

let fn_to_s pos arg =
    let arg_v = Eval.eval arg in
    VString (s_value arg_v)

(*
add : 'a -> 'a -> 'a
add = fn x y -> x + y
    = (fn x -> (fn y -> x + y))
*)
let fn_add pos x =
    VClosure ((EId "y", pos), (EBinary (BinAdd, x, (EId "y", pos)), pos), Symbol.get_current_env())

let fn_modules _ _ =
    print_endline "modules:";
    Symbol.show_all_modules();
    VUnit

let rec fn_builtins _ _ =
    List.iter (fun (mod_id, id, ty, _) ->
                if mod_id = "" then
                    print_endline @@ id ^ " : " ^ s_typ ty) func_list;
    VUnit

and hd_t = new_tvar ()
and tl_t = new_tvar ()
and fst_t = new_tvar ()
and fst_any_t = new_tvar ()
and snd_t = new_tvar ()
and snd_any_t = new_tvar ()
and add_t = new_tvar ()
and func_list = [
    ("", "ignore", TFun (new_tvar(), TUnit), fn_ignore);
    ("", "nl", TFun (TUnit, TUnit), fn_newline);
    ("List", "hd", TFun (TList hd_t, hd_t), fn_head);
    ("List", "tl", TFun (TList tl_t, TList tl_t), fn_tail);
    ("Tuple", "fst", TFun (TTuple (fst_t::[fst_any_t]), fst_t), fn_fst);
    ("Tuple", "snd", TFun (TTuple (snd_any_t::[snd_t]), snd_t), fn_snd);
    ("", "pr", TFun (new_tvar(), TUnit), fn_print);
    ("", "prn", TFun (new_tvar(), TUnit), fn_println);
    ("", "to_s", TFun (new_tvar(), TString), fn_to_s);

    ("", "add", TFun (add_t, TFun (add_t, add_t)), fn_add);

    ("", "modules", TFun (TUnit, TUnit), fn_modules);
    ("", "builtins", TFun (TUnit, TUnit), fn_builtins);
]


let insert mid id ty v =
    Symbol.insert_default mid id (Type.generalize ty) v false

let init () =
    insert "" "true" TBool (VBool true);
    insert "" "false" TBool (VBool false);
    List.iter (fun (mid, id, ty, fn) ->
        insert mid id ty (VBuiltin fn)) func_list

