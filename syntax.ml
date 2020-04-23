
type source_pos = {
    filename : string;
    line : int;
    col : int;
}

exception Error of source_pos * string

type lit = Bool of bool | Int of int | Char of char | Float of float | String of string

type token_decl =
    | Eof | Newline | Id of string | CId of string | Lit of lit | Op of string | TId of int
    | Module | Import | As | Type | Mut | Let | Rec | If | Then | Else | Fn
    | Semi | Colon | DColon | Comma | Dot | Null | Unit | Vertical | Ques | Eq | LOr | LAnd
    | Eql | Neq | LT | LE | GT | GE | Plus | Minus | Star | Slash | Percent | Not
    | LBrace | RBrace | LParen | RParen | LBracket | RBracket | RArrow

type token = token_decl * source_pos

type typ =
    | TUnit | TBool | TInt | TChar | TFloat | TString
    | TName of string
    | TTuple of typ list | TList of typ
    | TFun of typ * typ
    | TVar of int * typ option ref
    | TConstr of typ * typ
and type_schema = {
    vars : int list;
    body : typ;
}

type typ_expr =
    | EName of string
    | EVar of int
    | ETuple of typ_expr list
    | EFun of typ_expr * typ_expr
    | EConstr of typ_expr * typ_expr

type typ_decl =
    | EAlias of typ_expr
    | ERecord of (string * bool * typ_expr) list
    | EVariant of (string * typ_expr option) list

type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE | BinGT | BinGE
    | BinEql | BinNeq | BinLOr | BinLAnd | BinCons | BinOp of string
type unop = UNot | UMinus

type expr_decl =
    | ENull | EUnit
    | ELit of lit
    | EId of string
    | EModId of string list * string
    | ETuple of expr list
    | EParen of expr
    | EUnary of unop * expr
    | EBinary of binop * expr * expr
    | ECond of expr * expr * expr
    | ELambda of expr * expr
    | EApply of expr * expr
    | ELet of string * expr
    | ELetRec of string * expr
    | ESeq of expr list
    | EModule of string
    | EImport of string * string option
    | ETypeDef of int list * string * typ_decl

and expr = expr_decl * source_pos

type tenv = type_schema ref Env.t

type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char
    | VFloat of float | VString of string
    | VTuple of value list | VCons of value * value
    | VClosure of expr * expr * env
    | VBuiltin of (source_pos -> env -> value -> (env * value))
and env = value ref Env.t

type symtab = {
    module_name : string;
    mutable env : env;
    mutable tenv : tenv;
}


let error pos msg = raise (Error (pos, msg))

let s_pos pos = Printf.sprintf "%s, line=%d, col=%d: " pos.filename pos.line pos.col

let id x = x

let quote x = "\"" ^ x ^ "\""

let escape_char = function
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | '\\' -> "\\\\"
    | c ->
        if c >= '\032' && c <= '\126' then
            String.make 1 c
        else
            Printf.sprintf "\\%.3d" (int_of_char c)

let escape_str s =
    let buffer = Buffer.create (String.length s) in
    for i = 0 to (String.length s) - 1 do
        Buffer.add_string buffer @@ escape_char s.[i]
    done;
    Buffer.contents buffer


let s_lit = function
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Char c -> "'" ^ escape_char c ^ "'"
    | Float f -> string_of_float f
    | String s -> quote @@ escape_str s

let rec s_list to_s sep = function
    | [] -> ""
    | [x] -> to_s x
    | x::xs -> to_s x ^ sep ^ s_list to_s sep xs

let int_to_alpha x =
    if x <= Char.code 'z' - Char.code 'a' then
        String.make 1 (Char.chr ((Char.code 'a') + x))
    else
        string_of_int x

let s_token = function
    | Eof -> "<EOF>" | Newline -> "<NEWLINE>" | Id s -> s | CId s -> s | Lit l -> s_lit l | Op s -> s
    | TId n -> "'" ^ int_to_alpha n
    | Module -> "module" | Import -> "import" | As -> "as" | Type -> "type" | Mut -> "mut"
    | Let -> "let" | Rec -> "rec" | If -> "if" | Then -> "then" | Else -> "else" | Fn -> "fn"
    | Semi -> ";" | Colon -> ":" | DColon -> "::" | Comma -> "," | Dot -> "." | Null -> "[]"
    | Unit -> "()" | Vertical -> "|" | Ques -> "?" | Eq -> "=" | LOr -> "||" | LAnd -> "&&"
    | Eql -> "==" | Neq -> "!=" | LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">="
    | Plus -> "+" | Minus -> "-" | Star -> "*" | Slash -> "/" | Percent -> "%" | Not -> "!"
    | LBrace -> "{" | RBrace -> "}" | LParen -> "(" | RParen -> ")" | LBracket -> "["
    | RBracket -> "]" | RArrow -> "->" 

let s_typ ty =
    let counter = ref 0 in
    let dic = ref [] in
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TBool -> (5, "bool") | TInt -> (5, "int") | TChar -> (5, "char")
            | TFloat -> (5, "float") | TString -> (5, "string")
            | TName id -> (5, id)
            | TTuple tl -> (3, s_list (to_s 4) " * " tl)
            | TList t -> (3, to_s 0 t ^ " list")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents = None}) ->
                let y = try List.assoc x !dic
                    with Not_found ->
                        dic := (x, !counter) :: !dic;
                        let n = !counter in
                        incr counter;
                        n
                in
                (5, "'" ^ int_to_alpha y)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t)
            | TConstr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (3, s1 ^ " " ^ s2)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_typ_raw ty =
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TBool -> (5, "bool") | TInt -> (5, "int") | TChar -> (5, "char")
            | TFloat -> (5, "float") | TString -> (5, "string")
            | TName id -> (5, id)
            | TTuple tl -> (3, s_list (to_s 4) " * " tl)
            | TList t -> (3, to_s 0 t ^ " list")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents = None}) ->
                (5, "'" ^ string_of_int x)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t ^ "!")
            | TConstr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (3, s1 ^ " " ^ s2)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_type_schema ts =
    "{ vars:[" ^ s_list string_of_int "," ts.vars ^ "], body:" ^ s_typ_raw ts.body ^ " }"


let rec s_typ_expr = function
    | EName s -> s
    | EVar n -> "'" ^ int_to_alpha n
    | ETuple tl -> "(" ^ s_list s_typ_expr " * " tl ^ ")"
    | EFun (t1, t2) -> "(" ^ s_typ_expr t1 ^ " -> " ^ s_typ_expr t2 ^ ")"
    | EConstr (t, s) -> "(" ^ s_typ_expr t ^ " " ^ s_typ_expr s ^ ")"

let s_typ_decl_record _ = ""    (*TODO*)
let s_typ_decl_variant _ = ""   (*TODO*)

let s_typ_decl = function
    | EAlias te -> s_typ_expr te
    | ERecord rl -> s_typ_decl_record rl
    | EVariant vl -> s_typ_decl_variant vl

let s_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*" | BinDiv -> "/" | BinMod -> "%"
    | BinLT -> "<" | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">=" | BinEql -> "=="
    | BinNeq -> "!=" | BinLOr -> "||" | BinLAnd -> "&&" | BinCons -> "::"
    | BinOp op -> op
let s_unop = function UNot -> "!" | UMinus -> "-"

let rec s_expr = function
    | (ENull, _) -> "[]" | (EUnit, _) -> "()"
    | (ELit l, _) -> s_lit l | (EId s, _) -> s | (EModId (ml, s), _) -> s_list id "." ml ^ "." ^ s
    | (ETuple el, _) -> "(" ^ s_list s_expr ", " el ^ ")"
    | (EParen e, _) -> "(" ^ s_expr e ^ ")"
    | (EUnary (op, e), _) -> s_unop op ^ s_expr e
    | (EBinary (op, l, r), _) -> s_expr l ^ " " ^ s_binop op ^ " " ^ s_expr r
    | (ECond (c, t, e), _) -> "(" ^ s_expr c ^ ") ? " ^ s_expr t ^ " : " ^ s_expr e
    | (ELambda (a, b), _) -> "fn " ^ s_expr a ^ " -> " ^ s_expr b 
    | (EApply (f, a), _) -> s_expr f ^ " " ^ s_expr a
    | (ELet (s, e), _) -> "let " ^ s ^ " = " ^ s_expr e
    | (ELetRec (s, e), _) -> "let rec " ^ s ^ " = " ^ s_expr e
    | (ESeq el, _) -> "{ " ^ s_exprlist "; " el ^ " }"
    | (EModule mid, _) -> "module " ^ mid
    | (EImport (mid, None), _) -> "import " ^ mid
    | (EImport (mid, Some aid), _) -> "import " ^ mid ^ " as " ^ aid
    | (ETypeDef (params, tid, td), _) -> "type" ^ s_params params  ^ " " ^ tid ^ " = " ^ s_typ_decl td
and s_exprlist sep = s_list s_expr sep
and s_params ps =
    let rec aux = function
        | [] -> ""
        | [x] -> "'" ^ int_to_alpha x
        | x::xs -> "'" ^ int_to_alpha x ^ " " ^ s_params xs
    in match ps with
        | [] -> ""
        | [x] -> " " ^ aux ps
        | _ -> " (" ^ aux ps ^ ")"


let rec str_of = function
    | VUnit -> "()"
    | VNull -> "[]"
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VChar c -> String.make 1 c
    | VFloat f -> string_of_float f
    | VString s -> s
    | VTuple vl -> "(" ^ s_list str_of ", " vl ^ ")"
    | VCons (VChar c, VNull) -> String.make 1 c
    | VCons (VChar c, cdr) -> String.make 1 c ^ str_of cdr
    | VCons _ as xs -> "[" ^ str_of_cons xs ^ "]"
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"
and str_of_cons = function
    | VNull -> ""
    | VCons (h, VNull) -> str_of h
    | VCons (h, t) -> str_of h ^ "," ^ str_of_cons t
    | _ -> failwith "str_of_cons"

let rec s_value = function
    | VUnit -> "()"
    | VNull -> "[]"
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VChar c -> "'" ^ String.make 1 c ^ "'"
    | VFloat f -> string_of_float f
    | VString s -> quote s
    | VTuple vl -> "(" ^ s_list s_value ", " vl ^ ")"
    | VCons (VChar _, _) as s -> quote (s_value_str s)
    | VCons _ as xs -> "[" ^ s_value_cons xs ^ "]"
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"
and s_value_cons = function
    | VNull -> ""
    | VCons (h, VNull) -> s_value h
    | VCons (h, t) -> s_value h ^ "," ^ s_value_cons t
    | _ -> failwith "s_value_cons"
and s_value_str = function
    | VCons (VChar c, VNull) -> String.make 1 c
    | VCons (VChar c, cdr) -> String.make 1 c ^ s_value_str cdr
    | _ -> failwith "s_value_str"

let s_lit_src = function
    | Bool b -> "Bool " ^ string_of_bool b
    | Int i -> "Int " ^ string_of_int i
    | Char c -> "Char '" ^ escape_char c ^ "'"
    | Float f -> "Float " ^ string_of_float f
    | String s -> "String " ^ quote @@ escape_str s

let s_token_src = function
    | Eof -> "Eof" | Newline -> "Newline" | Id s -> "Id " ^ quote s | CId s -> "CId " ^ quote s
    | Lit l -> "Lit " ^ s_lit_src l | Op s -> "Op " ^ quote s | TId n -> "TId " ^ string_of_int n
    | Module -> "Module" | Import -> "Import" | As -> "As" | Type -> "Type" | Mut -> "Mut"
    | Let -> "Let" | Rec -> "Rec" | If -> "If" | Then -> "Then" | Else -> "Else" | Fn -> "Fn"
    | Semi -> "Semi" | Colon -> "Colon" | DColon -> "DColon" | Comma -> "Comma"
    | Dot -> "Dot" | Null -> "Null" | Unit -> "Unit" | Vertical -> "Vertical"
    | Ques -> "Ques" | Eq -> "Eq" | LOr -> "LOr" | LAnd -> "LAnd"
    | Eql -> "Eql" | Neq -> "Neq" | LT -> "LT" | LE -> "LE" | GT -> "GT" | GE -> "GE"
    | Plus -> "Plus" | Minus -> "Minus" | Star -> "Star" | Slash -> "Slash"
    | Percent -> "Percent" | Not -> "Not"
    | LBrace -> "LBrace" | RBrace -> "RBrace" | LParen -> "LParen"
    | RParen -> "RParen" | LBracket -> "LBracket" | RBracket -> "RBracket" | RArrow -> "RArrow" 

let s_token_src_list toks = "[" ^ s_list (fun (x, _) -> s_token_src x) "; " toks ^ "]"

let s_typ_src ty =
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "TUnit") | TBool -> (5, "TBool") | TInt -> (5, "TInt") | TChar -> (5, "TChar")
            | TFloat -> (5, "TFloat") | TString -> (5, "TString")
            | TName id -> (5, "TName " ^ id)
            | TTuple tl -> (3, "TTuple (" ^ s_list (to_s 4) ", " tl ^ ")")
            | TList t -> (3, "TList " ^ to_s 0 t)
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, "TFun (" ^ s1 ^ ", " ^ s2 ^ ")")
            | TVar (x, {contents = None}) ->
                (5, "TVar (" ^ string_of_int x ^ ", None)")
            | TVar (x, {contents = Some t}) ->
                (3, "TVar (" ^ string_of_int x ^ ", Some " ^ to_s n t)
            | TConstr (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (3, "TConstr (" ^ s1 ^ ", " ^ s2 ^ ")")
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let rec s_typ_expr_src = function
    | EName s -> "(EName " ^ quote s ^ ")"
    | EVar n -> "(EVar " ^ string_of_int n ^ ")"
    | ETuple tl -> "(ETuple [" ^ s_list s_typ_expr_src "; " tl ^ "])"
    | EFun (t1, t2) -> "(EFun (" ^ s_typ_expr_src t1 ^ ", " ^ s_typ_expr_src t2 ^ "))"
    | EConstr (t, s) -> "(EConstr (" ^ s_typ_expr_src t ^ ", " ^ s_typ_expr_src s ^ "))"

let s_typ_decl_record_src _ = ""    (*TODO*)
let s_typ_decl_variant_src _ = ""   (*TODO*)

let s_typ_decl_src = function
    | EAlias te -> "EAlias " ^ s_typ_expr_src te
    | ERecord rl -> "ERecord " ^ s_typ_decl_record_src rl
    | EVariant vl -> "EVariant " ^ s_typ_decl_variant_src vl

let s_binop_src = function
    | BinAdd -> "BinAdd" | BinSub -> "BinSub" | BinMul -> "BinMul" | BinDiv -> "BinDiv" | BinMod -> "BinMod"
    | BinLT -> "BinLT" | BinLE -> "BinLE" | BinGT -> "BinGT" | BinGE -> "BinGE" | BinEql -> "BinEql"
    | BinNeq -> "BinNeq" | BinLOr -> "BinLOr" | BinLAnd -> "BinLAnd" | BinCons -> "BinCons"
    | BinOp op -> "BinOp " ^ quote op
let s_unop_src = function UNot -> "UNot" | UMinus -> "UMinus"

let rec s_expr_src = function
    | (ENull, _) -> "ENull" | (EUnit, _) -> "EUnit"
    | (ELit l, _) -> "(ELit (" ^ s_lit_src l ^ "))" | (EId s, _) -> "(EId " ^ quote s ^ ")"
    | (EModId (ml, s), _) -> "(EModId ([" ^ s_list quote ";" ml ^ "]," ^ quote s ^ "))"
    | (ETuple el, _) -> "(ETuple [" ^ s_list s_expr_src "; " el ^ "])"
    | (EParen e, _) -> "(EParen " ^ s_expr_src e ^ ")"
    | (EUnary (op, e), _) -> "(EUnary (" ^ s_unop_src op ^ ", " ^ s_expr_src e ^ "))"
    | (EBinary (op, l, r), _) -> "(EBinary (" ^ s_binop_src op ^ ", " ^ s_expr_src l ^ ", " ^ s_expr_src r ^ "))"
    | (ECond (c, t, e), _) -> "(ECond (" ^ s_expr_src c ^ ", " ^ s_expr_src t ^ ", " ^ s_expr_src e ^ "))"
    | (ELambda (a, b), _) -> "(ELambda (" ^ s_expr_src a ^ ", " ^ s_expr_src b ^ "))"
    | (EApply (f, a), _) -> "(EApply (" ^ s_expr_src f ^ ", " ^ s_expr_src a ^ "))"
    | (ELet (s, e), _) -> "(ELet (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ELetRec (s, e), _) -> "(ELetRec (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ESeq el, _) -> "(ESeq " ^ s_exprlist_src el ^ ")"
    | (EModule mid, _) -> "(EModule " ^ quote mid ^ ")"
    | (EImport (mid, None), _) -> "(EImport (" ^ quote mid ^ ", None))"
    | (EImport (mid, Some aid), _) -> "(EImport (" ^ quote mid ^ ", Some " ^ quote aid ^ "))"
    | (ETypeDef (params, tid, td), _) -> "(ETypeDef ([" ^ s_list string_of_int ";" params ^ "], " ^ quote tid ^ ", " ^ s_typ_decl_src td ^ "))"
and s_exprlist_src el = "[" ^ s_list s_expr_src "; " el ^ "]"

let rec s_value_src = function
    | VUnit -> "VUnit"
    | VNull -> "VNull"
    | VBool b -> "VBool " ^ string_of_bool b
    | VInt n -> "VInt " ^ string_of_int n
    | VChar c -> "VChar '" ^ String.make 1 c ^ "'"
    | VFloat f -> "VFloat " ^ string_of_float f
    | VString s -> "VString " ^ quote s
    | VTuple vl -> "VTuple [" ^ s_list s_value_src "; " vl ^ "]"
    | VCons (car, cdr) -> "VCons (" ^ s_value_src car ^ ", " ^ s_value_src cdr ^ ")"
    | VClosure _ -> "VClosure"
    | VBuiltin _ -> "VBuiltin"

