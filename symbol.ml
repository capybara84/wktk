
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

let set_module mid =
    (*TODO*)
    ()

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

