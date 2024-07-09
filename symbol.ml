
open Ast

let default_module_name = "Main"

let all_modules = ref []

let insert_module name =
    let modu = { name = name; tenv = []; env = []; } in
    all_modules := (name, modu) :: !all_modules;
    modu

let lookup_module name =
    List.assoc name !all_modules

let exist_module mid aid =
    let name =
        match aid with
        | Some id -> id
        | None -> mid
    in
    List.mem_assoc name !all_modules

let default_module = insert_module default_module_name
let current_module = ref default_module

let get_current_tenv () = !current_module.tenv

let lookup_tysym_from_module modu id =
    List.assoc id modu.tenv

let lookup_tysym_default id =
    lookup_tysym_from_module default_module id

let lookup_tysym id =
    lookup_tysym_from_module !current_module id

let insert_tysym id tysym =
    !current_module.tenv <- (id, tysym) :: !current_module.tenv

let enter_new_tenv () =
    !current_module.tenv

let leave_tenv ctx =
    !current_module.tenv <- ctx


let get_current_env () = !current_module.env

let lookup_from_module modu id =
    List.assoc id modu.env

let lookup_default id =
    lookup_from_module default_module id

let lookup id =
    lookup_from_module !current_module id

let insert_sym id sym =
    !current_module.env <- (id, sym) :: !current_module.env

let enter_new_env env =
    let res = !current_module.env in
    if env <> [] then
        !current_module.env <- env;
    res

let leave_env env =
    !current_module.env <- env




let set_module mid =
    verbose @@ "set_module: " ^ mid;
    try
        current_module := lookup_module mid
    with Not_found -> begin
        current_module := insert_module mid;

        let tys = make_typ_scheme [] (TModule mid) in
        let tysym = { tys = tys; is_mutable = false } in
        default_module.tenv <- (mid, tysym) :: default_module.tenv;

        let v = VModule !current_module in
        let sym = { v = v; is_mutable = false } in
        default_module.env <- (mid, sym) :: default_module.env
    end

let insert_default mid id tys v ism =
    if mid <> "" then
        set_module mid;
    let tysym = { tys = tys; is_mutable = ism } in
    insert_tysym id tysym;
    let sym = { v = v; is_mutable = ism } in
    insert_sym id sym

let init () =
    let tys = make_typ_scheme [] (TModule default_module_name) in
    let tysym = { tys = tys; is_mutable = false } in
    default_module.tenv <- (default_module_name, tysym) :: default_module.tenv;

    let v = VModule default_module in
    let sym = { v = v; is_mutable = false } in
    default_module.env <- (default_module_name, sym) :: default_module.env


let show_typ_tables (tenv : tenv) = 
    print_endline "  SHOW TYP TABLES";
    List.iter (fun (id, (tysym : typ_sym)) ->
        print_endline @@ "    " ^ (if tysym.is_mutable then "mut " else "" )
            ^ id ^ " : " ^ s_typ (tysym.tys.body) ) tenv

let show_tables env =
    print_endline "  SHOW SYM TABLES";
    List.iter (fun (id, sym) ->
        print_endline @@ "    " ^ (if sym.is_mutable then "mut " else "" )
            ^ id ^ " = " ^ s_value sym.v) env

let show_all_symbols modu =
    print_endline @@ "SHOW MODULE : " ^ modu.name;
    show_typ_tables modu.tenv;
    show_tables modu.env;
    print_endline @@ "current = " ^ !current_module.name

let show_all_modules () =
    print_endline "SHOW ALL MODULES";
    List.iter (fun (_, m) -> show_all_symbols m) !all_modules


