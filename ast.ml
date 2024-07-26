
let verbose_flag = ref false
let verbose msg =
    if !verbose_flag then
        print_endline msg

let default_module_name = "Main"
let default_extension = ".wt"
let default_directory = "./"

let filename_to_module_name filename =
    try
        String.capitalize_ascii @@ Filename.chop_suffix (Filename.basename filename) default_extension
    with Invalid_argument _ -> String.capitalize_ascii filename

let module_name_to_filename name =
    default_directory ^ String.uncapitalize_ascii name ^ default_extension


type pos = {
    filename : string;
    line : int;
    col : int;
}

let s_pos pos =
    Printf.sprintf "\"%s\":line=%d:col=%d:" pos.filename pos.line pos.col

exception Error of pos * string

let error pos msg = raise (Error (pos, msg))
let error_msg p m = s_pos p ^ ":" ^ m

type token_decl =
    | EOF | NEWLINE | INDENT | DEDENT
    | Int of int | Float of float | Char of char
    | String of string | Id of string | TypId of int
    | MODULE | IMPORT | AS | DECL | TYPE | LET | IN | MUT | FN
    | IF | THEN | ELSE | BEGIN | END | SEMI | COLON | DCOLON
    | DOT | COMMA | ARROW | ASSIGN | OR | AND | LOR | LAND | LPAR
    | RPAR | LBRA | RBRA | NOT | EQ | NEQ | EQL | NEQL
    | LT | LE | GT | GE | PLUS | MINUS | STAR | SLASH | PERCENT
    | QUES | UNIT | NIL
and token = token_decl * pos

type typ =
    | TUnit | TInt | TFloat | TBool | TChar | TString
    | TModule of string
    | TAlias of string * typ
    | TConstr of typ * typ
    | TList of typ
    | TTuple of typ list
    | TFun of typ * typ
    | TVar of int * typ option ref
    | TRecord of (string * bool * typ) list
    | TVariant of string * (string * typ option) list
and
    typ_scheme = {
        vars : int list;
        body : typ;
    }
and
    typ_sym = {
        mutable tys : typ_scheme;
        is_mutable : bool;
    }
and
    tenv = (string * typ_sym) list

let make_typ_scheme vars ty = { vars = vars; body = ty }

let seed = ref 0
let new_tvar () =
    let ty = TVar (!seed, ref None) in
    incr seed;
    ty


type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE
    | BinGT | BinGE | BinEq | BinNeq | BinEql | BinNeql | BinLOr | BinLAnd
    | BinCons
type unop = UNot | UMinus

type expr_decl =
    | EEof | EUnit | ENil
    | EBool of bool | EInt of int | EFloat of float | EChar of char
    | EString of string | EId of string
    | EModule of string | EImport of string * string option
    | ERecord of (string * expr) list
    | ETuple of expr list
    | EUnary of unop * expr
    | EBinary of binop * expr * expr
    | EApply of expr * expr
    | ELet of expr list * expr
    | EValDef of bool * string * expr
    | EFuncDef of string * expr
    | ELambda of expr * expr
    | ECond of expr * expr * expr
    | EAssign of expr * expr
    | EMessage of expr * string
    | EBlock of expr list
    | ESeq of expr list
    | ETypeDecl of int list * string * typ_decl
    | EDecl of string * typ_expr
and expr = expr_decl * pos

and typ_expr =
    | TE_Name of string
    | TE_Message of typ_expr * string
    | TE_Var of int
    | TE_Tuple of typ_expr list
    | TE_Fun of typ_expr * typ_expr
    | TE_Constr of typ_expr * typ_expr
and typ_decl =
    | TD_Alias of typ_expr
    | TD_Record of (string * bool * typ_expr) list
    | TD_Variant of (string * typ_expr option) list

and value =
    | VUnit | VNil | VInt of int | VFloat of float
    | VBool of bool | VChar of char | VString of string
    | VModule of module_def
    | VType of typ
    | VCons of value * value
    | VTuple of value list
    | VClosure of expr * expr * env
    | VBuiltin of (pos -> expr -> value)
    | VRecord of (string * value ref) list
    | VVariant of typ * string * value option
and
    symbol = {
        mutable v : value;
        is_mutable : bool;
    }
and
    module_def = {
        name : string;
        mutable tenv : tenv;
        mutable env : env;
    }
and
    env = (string * symbol) list


let int_to_alpha x =
    if x <= Char.code 'z' - Char.code 'a' then
        String.make 1 (Char.chr ((Char.code 'a') + x))
    else
        string_of_int x

let rec s_list to_s sep = function
    | [] -> ""
    | [x] -> to_s x
    | x::xs -> to_s x ^ sep ^ s_list to_s sep xs

let s_token = function
    | EOF -> "<EOF>" | NEWLINE -> "<NEWLINE>"
    | INDENT -> "INDENT"
    | DEDENT -> "DEDENT"
    | Int n -> string_of_int n | Float f -> string_of_float f
    | Char c -> "'" ^ (String.make 1 c) ^ "'"
    | String s -> "\"" ^ s ^ "\""
    | Id s -> s
    | TypId n -> "'" ^ int_to_alpha n
    | MODULE -> "module" | IMPORT -> "import" | AS -> "as"
    | DECL -> "decl"
    | TYPE -> "type" | LET -> "let" | IN -> "in"|  MUT -> "mut"
    | FN -> "fn" | IF -> "if" | THEN -> "then" | ELSE -> "else"
    | BEGIN -> "{" | END -> "}" | SEMI -> ";" | COLON -> ":"
    | DCOLON -> "::" | DOT -> "." | COMMA -> "," | ARROW -> "->"
    | ASSIGN -> "<-" | OR -> "|" | AND -> "&" | LOR -> "||"
    | LAND -> "&&" | LPAR -> "(" | RPAR -> ")" | LBRA -> "[" | RBRA -> "]"
    | NOT -> "!" | EQ -> "=" | NEQ -> "<>" | EQL -> "==" | NEQL -> "!="
    | LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">=" | PLUS -> "+" | MINUS -> "-"
    | STAR -> "*" | SLASH -> "/" | PERCENT -> "%"
    | QUES -> "?" | UNIT -> "()" | NIL -> "[]"

let rec s_typ ty =
    let counter = ref 0 in
    let dic = ref [] in
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TInt -> (5, "int") | TFloat -> (5, "float")
            | TBool -> (5, "bool") | TChar -> (5, "char") | TString -> (5, "string")
            | TModule s -> (5, s)
            | TAlias (s, _) -> (5, s)
            | TConstr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (3, s1 ^ " " ^ s2)
            | TList t -> (3, to_s 0 t ^ " list")
            | TTuple tl -> (3, "(" ^ s_list (to_s 4) " * " tl ^ ")")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents=None}) ->
                let y = try List.assoc x !dic
                    with Not_found ->
                        dic := (x, !counter) :: !dic;
                        let n = !counter in
                        incr counter;
                        n
                in (5, "'" ^ int_to_alpha y)
            | TVar (_, {contents=Some t}) ->
                (3, to_s n t)
            | TRecord rl ->
                (3, "{" ^ s_list (fun (s,b,t) ->
                        (if b then "mut " else "") ^ s ^ ":" ^ to_s 0 t)
                        ";" rl ^ "}")
            | TVariant (s, vl) ->
                (3, s)
                (*
                    "|" ^ s_list (fun (s, ot) ->
                        match ot with
                        | None -> s
                        | Some t -> s ^ " " ^ to_s 0 t)
                        "|" vl)
                *)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*"
    | BinDiv -> "/" | BinMod -> "%" | BinLT -> "<"
    | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">="
    | BinEq -> "=" | BinNeq -> "<>"
    | BinEql -> "==" | BinNeql -> "!=" | BinLOr -> "||"
    | BinLAnd -> "&&" | BinCons -> "::"
let s_unop = function UNot -> "!" | UMinus -> "-"

let rec s_expr = function
    | (EEof, _) -> "<EOF>"
    | (EUnit, _) -> "()"
    | (ENil, _) -> "[]"
    | (EBool true, _) -> "true"
    | (EBool false, _) -> "false"
    | (EInt n, _) -> string_of_int n
    | (EFloat n, _) -> string_of_float n
    | (EChar c, _) -> "'" ^ (String.make 1 c) ^ "'"
    | (EString s, _) -> "\"" ^ s ^ "\""
    | (EId s, _) -> s
    | (EModule s, _) -> "module " ^ s ^ "\n"
    | (EImport (s, None), _) -> "import " ^ s ^ "\n"
    | (EImport (s, Some a), _) -> "import " ^ s ^ " as " ^ a ^ "\n"
    | (ERecord rl, _) -> "{" ^ s_list (fun (id, e) -> id ^ " = " ^ s_expr e) ";" rl ^ "}"
    | (ETuple el, _) -> "(" ^ s_list s_expr ", " el ^ ")"
    | (EUnary (op, e), _) -> "(" ^ s_unop op ^ s_expr e ^ ")"
    | (EBinary (op, lhs, rhs), _) ->
        "(" ^ s_expr lhs ^ " " ^ s_binop op ^ " " ^ s_expr rhs ^ ")"
    | (EApply (f, a), _) -> s_expr f ^ " " ^ s_expr a
    | (ELet (ll, e), _)
        -> "let" ^ s_list s_expr "; " ll ^ " in " ^ s_expr e
    | (EValDef (b, id, e), _) -> " " ^ (if b then "mut " else "") ^ id ^ " = " ^ s_expr e
    | (EFuncDef (id, e), _) -> " fun " ^ id ^ " = " ^ s_expr e
    | (ELambda (a, b), _) -> "(fn " ^ s_expr a ^ " -> (" ^ s_expr b ^ "))"
    | (ECond (c, t, e), _) ->
        "(if " ^ s_expr c ^ " then " ^ s_expr t ^ " else " ^ s_expr e ^ ")"
    | (EAssign (lhs, rhs), _) -> s_expr lhs ^ " <- " ^ s_expr rhs
    | (EMessage (lhs, s), _) -> s_expr lhs ^ "." ^ s
    | (EBlock el, _) -> "{ " ^ s_list s_expr "; " el ^ " }"
    | (ESeq el, _) -> s_list s_expr "" el
    | (ETypeDecl (fs, id, tyd), _) -> "type " ^ s_list (fun x -> "'" ^ int_to_alpha x) "," fs ^ " " ^ id ^ " = " ^ s_typ_decl tyd
    | (EDecl (id, tye), _) -> id ^ " : " ^ s_typ_expr tye ^ ""

and s_typ_expr tye =
    let rec to_s n tye =
        let (m, str) =
            match tye with
            | TE_Name id -> (5, id)
            | TE_Message (e, id) -> (5, to_s 1 e ^ "." ^ id)
            | TE_Var n -> (5, "'" ^ int_to_alpha n)
            | TE_Tuple tl -> (3, "(" ^ s_list s_typ_expr " * " tl ^ ")")
            | TE_Fun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TE_Constr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " " ^ s2)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) tye

and s_typ_decl = function
    | TD_Alias tye -> s_typ_expr tye
    | TD_Record rl ->
        "{" ^ s_list
            (fun (s,b,t) -> (if b then "mut " else "") ^ s ^ ":" ^ s_typ_expr t)
            ";" rl ^ "}"
    | TD_Variant vl ->
        "|" ^ s_list
            (fun (s, ot) ->
                match ot with
                | None -> s
                | Some t -> s ^ " " ^ s_typ_expr t)
            "|" vl

let rec s_value = function
    | VUnit -> "()"
    | VNil -> "[]"
    | VInt n -> string_of_int n
    | VFloat f -> string_of_float f
    | VBool true -> "true"
    | VBool false -> "false"
    | VChar c -> String.make 1 c
    | VString s -> s
    | VModule _ -> "<module>"
    | VType ty -> "<type " ^ s_typ ty ^ ">"
    | (VCons (_,_)) as v -> "[" ^ (cons_to_string v) ^ "]"
    | VTuple vl -> "(" ^ s_list s_value ", " vl ^ ")"
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"
    | VRecord rl -> "{" ^ s_list (fun (s,vr) -> s ^ "=" ^ s_value !vr) ";" rl ^ "}"
    | VVariant (TVariant (t, _), s, None) -> t ^ "." ^ s
    | VVariant (TVariant (t, _), s, Some v) -> t ^ "." ^ s ^ " " ^ s_value v
    | VVariant (_, _, _) -> failwith "VVariant"
and cons_to_string = function
    | VCons (x, VNil) -> s_value x
    | VCons (x, (VCons _ as xs)) -> (s_value x) ^ ", " ^ (cons_to_string xs)
    | VCons (x, y) -> (s_value x) ^ " :: " ^ (s_value y)
    | _ -> failwith "cons bug"

