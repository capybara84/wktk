open Ast

let do_scanner_test = ref true
let do_parser_test = ref true
let do_type_test = ref true
let do_eval_test = ref true


let color c s = "\x1b[3" ^ (string_of_int c) ^ "m" ^ s ^ "\x1b[0m"
let red = color 1 and green = color 2 and yellow = color 3 and blue = color 4
and magenta = color 5 and cyan = color 6 and white = color 7

let n_ok = ref 0
let n_fail = ref 0

let test_ok () = incr n_ok; print_string @@ green "."
let test_fail s = incr n_fail; print_endline @@ red @@ "! " ^ s
let test_eq a b m = if a = b then test_ok () else test_fail m

let test_report () =
    let n_all = !n_ok + !n_fail in
    print_endline @@ "All   : " ^ string_of_int n_all;
    print_endline @@ "OK    : " ^ green @@ string_of_int !n_ok;
    print_endline @@ "Failed: " ^
        if !n_fail = 0 then "0" else (red @@ string_of_int !n_fail)


let s_binop_src = function
    | BinAdd -> "BinAdd" | BinSub -> "BinSub" | BinMul -> "BinMul"
    | BinDiv -> "BinDiv" | BinMod -> "BinMod" | BinLT -> "BinLT"
    | BinLE -> "BinLE" | BinGT -> "BinGT" | BinGE -> "BinGE"
    | BinEq -> "BinEq" | BinNeq -> "BinNeq"
    | BinEql -> "BinEql" | BinNeql -> "BinNeql" | BinLOr -> "BinLOr"
    | BinLAnd -> "BinLAnd" | BinCons -> "BinCons"
let s_unop_src = function UNot -> "UNot" | UMinus -> "UMinus"

let rec s_expr_src = function
    | (EEof, _) -> "EEof"
    | (EUnit, _) -> "EUnit"
    | (ENil, _) -> "ENil"
    | (EBool b, _) -> "(EBool " ^ string_of_bool b ^ ")"
    | (EInt n, _) -> "(EInt " ^ string_of_int n ^ ")"
    | (EFloat n, _) -> "(EFloat " ^ string_of_float n ^ ")"
    | (EChar c, _) -> "(EChar '" ^ (String.make 1 c) ^ "')"
    | (EString s, _) -> "(EString \"" ^ s ^ "\")"
    | (EId s, _) -> "(EId \"" ^ s ^ "\")"
    | (EModule s, _) -> "(EModule \"" ^ s ^ "\")"
    | (EImport (s, None), _) -> "(EImport (\"" ^ s ^ "\", None))"
    | (EImport (s, Some a), _) -> "(EImport (\"" ^ s ^ "\", Some \"" ^ a ^ "\"))"
    | (ERecord rl, _) -> "(ERecord [" ^ s_list (fun (id, e) -> "(\"" ^ id ^ "\"," ^ s_expr e ^ ")") ";" rl ^ "])"
    | (ETuple el, _) -> "(ETuple [" ^ s_list s_expr_src "; " el ^ "])"
    | (EUnary (op, e), _) -> "(EUnary (" ^ s_unop_src op ^ ", " ^ s_expr_src e ^ "))"
    | (EBinary (op, lhs, rhs), _) ->
        "(EBinary (" ^ s_binop_src op ^ ", " ^ s_expr_src lhs ^ ", " ^ s_expr_src rhs ^ "))"
    | (EApply (f, a), _) -> "(EApply (" ^ s_expr_src f ^ ", " ^ s_expr_src a ^ "))"
    | (ELet (ll, e), _) ->
        "(ELet ([" ^ s_list s_expr_src "; " ll ^ "], " ^ s_expr_src e ^ "))"
    | (EValDef (b, id, e), _) ->
        "(EValDef (" ^ string_of_bool b ^ ", \"" ^ id ^ "\", " ^ s_expr_src e ^ "))"
    | (EFuncDef (id, e), _) ->
        "(EFuncDef (\"" ^ id ^ "\", " ^ s_expr_src e ^ "))"
    | (ELambda (a, b), _) -> "(ELambda (" ^ s_expr_src a ^ ", " ^ s_expr_src b ^ "))"
    | (ECond (c, t, e), _) ->
        "(ECond (" ^ s_expr_src c ^ ", " ^ s_expr_src t ^ ", " ^ s_expr_src e ^ "))"
    | (EAssign (lhs, rhs), _) ->
        "(EAssign (" ^ s_expr_src lhs ^ ", " ^ s_expr_src rhs ^ "))"
    | (EMessage (lhs, s), _) -> "(EMessage (" ^ s_expr_src lhs ^ ", \"" ^ s ^ "\"))"
    | (EBlock el, _) -> "(EBlock [" ^ s_list s_expr "; " el ^ "])"
    | (ESeq el, _) -> "(ESeq [" ^ s_list s_expr ";\n" el ^ "])"
    | (ETypeDecl (fs, id, tyd), _) ->
        "(ETypeDecl ([" ^ s_list string_of_int ";" fs ^ "], \"" ^ id ^ "\", " ^ s_typ_decl_src tyd ^ "))"
    | (EDecl (id, tye), _) ->
        "(EDecl (\"" ^ id ^ "\", " ^ s_typ_expr_src tye ^ "))"

and s_typ_expr_src = function
    | TE_Name id -> "(TE_Name \"" ^ id ^ "\")"
    | TE_Message (e, id) -> "(TE_Message (" ^ s_typ_expr_src e ^ ", \"" ^ id ^ "\"))"
    | TE_Var n -> "(TE_Var " ^ string_of_int n ^ ")"
    | TE_Tuple tl -> "(TE_Tuple [" ^ s_list s_typ_expr_src ";" tl ^ "])"
    | TE_Fun (t1, t2) -> "(TE_Fun (" ^ s_typ_expr_src t1 ^ ", " ^ s_typ_expr_src t2 ^ "))"
    | TE_Constr (t1, t2) -> "(TE_Constr (" ^ s_typ_expr_src t1 ^ ", " ^ s_typ_expr_src t2 ^ "))"

and s_typ_decl_src = function
    | TD_Alias tye -> "(TD_Alias " ^ s_typ_expr_src tye ^ ")"
    | TD_Record rl -> "(TD_Record [" ^ s_list s_typ_record_src ";" rl ^ "])"
    | TD_Variant vl -> "(TD_Variant [" ^ s_list s_typ_variant_src ";" vl ^ "])"

and s_typ_record_src = function
    | (id, b, te) ->
        "(\"" ^ id ^ "\", " ^ string_of_bool b ^ ", " ^ s_typ_expr_src te ^ ")"

and s_typ_variant_src = function
    | (id, None) -> "(\"" ^ id ^ "\", None)"
    | (id, Some te) -> "(\"" ^ id ^ "\", Some " ^ s_typ_expr_src te ^ ")"



let scanner_test_text = "a
/* test
    /* nest */
*/

/*  6 */ identifier Ident 12345
/*  7 */ 'a' '\\t' \"abc\\n\"

// comment

/* 11 */ mut let in fn if then else
/* 12 */ : :: , -> <- | & || && ( ) [ ] ! = <> == != ;
/* 13 */ < <= > >= + - * / % ? () []

aaa
    bbb
        ccc
        ddd
    eee
        fff
"

let scanner_test_result = [
    Id "a"; NEWLINE; NEWLINE; NEWLINE; NEWLINE;
    Id "identifier"; Id "Ident"; Int 12345; NEWLINE;
    Char 'a'; Char '\t'; String "abc\n"; NEWLINE;
    NEWLINE; NEWLINE; NEWLINE; NEWLINE;
    MUT; LET; IN; FN; IF; THEN; ELSE; NEWLINE;
    COLON; DCOLON; COMMA; ARROW; ASSIGN; OR; AND;
    LOR; LAND; LPAR; RPAR; LBRA; RBRA; NOT; EQ;
    NEQ; EQL; NEQL; SEMI; NEWLINE;
    LT; LE; GT; GE; PLUS; MINUS; STAR; SLASH;
    PERCENT; QUES; UNIT; NIL; NEWLINE;
    NEWLINE; NEWLINE;
    Id "aaa"; NEWLINE;
    INDENT; Id "bbb"; NEWLINE;
    INDENT; Id "ccc"; NEWLINE;
    Id "ddd"; NEWLINE;
    DEDENT; Id "eee"; NEWLINE;
    INDENT; Id "fff"; NEWLINE;
    DEDENT;
    DEDENT;
]

let scanner_test () =
    print_string "Scanner Test: ";
    try
        let toks = Scanner.open_text scanner_test_text in
        let rec loop toks result_list =
            match toks with
            | [] ->
                if result_list <> [] then
                    test_fail "too short"
            | x'::xs ->
                if result_list = [] then begin
                    test_fail @@ "too long";
                    loop xs []
                end else begin
                    let x = fst x' in
                    let y = List.hd result_list in
                    let rest = List.tl result_list in
                    if !verbose_flag then begin
                        print_endline @@ "result:   " ^ s_token x;
                        print_endline @@ "expected: " ^ s_token y
                    end;
                    test_eq x y @@ s_token x ^ " <> " ^ s_token y;
                    loop xs rest
                end
        in
        loop toks scanner_test_result;
        print_newline ()

    with
        | Error (pos, msg) -> print_endline @@ s_pos pos ^ "Error: " ^ msg
        | End_of_file -> ()
        | Sys_error s -> print_endline s

let parser_test_data = [
    (";", "EEof");
    ("\n\n;\n", "EEof");
    ("let one = 1 in one", "(ELet ([(EValDef (false, \"one\", (EInt 1)))], (EId \"one\")))");
    ("let _ = 2 in ()", "(ELet ([(EValDef (false, \"_\", (EInt 2)))], EUnit))");
    ("let id x = x in id 0", "(ELet ([(EFuncDef (\"id\", (ELambda ((EId \"x\"), (EId \"x\")))))], (EApply ((EId \"id\"), (EInt 0)))))");
    ("let add x y = x + y in add 1 2", "(ELet ([(EFuncDef (\"add\", (ELambda ((EId \"x\"), (ELambda ((EId \"y\"), (EBinary (BinAdd, (EId \"x\"), (EId \"y\")))))))))], (EApply ((EApply ((EId \"add\"), (EInt 1))), (EInt 2)))))");
    ("let foo () = () in foo", "(ELet ([(EFuncDef (\"foo\", (ELambda (EUnit, EUnit))))], (EId \"foo\")))");
    ("let bar _ = () in bar ()", "(ELet ([(EFuncDef (\"bar\", (ELambda ((EId \"_\"), EUnit))))], (EApply ((EId \"bar\"), EUnit))))");
    ("let _ = 1 in ()", "(ELet ([(EValDef (false, \"_\", (EInt 1)))], EUnit))");
    ("let one = 1; two = 2 in one + two", "(ELet ([(EValDef (false, \"one\", (EInt 1))); (EValDef (false, \"two\", (EInt 2)))], (EBinary (BinAdd, (EId \"one\"), (EId \"two\")))))");
    ("let mut var = 1 in var <- 2", "(ELet ([(EValDef (true, \"var\", (EInt 1)))], (EAssign ((EId \"var\"), (EInt 2)))))");
    ("if 1 == 1 then 2 else 3", "(ECond ((EBinary (BinEql, (EInt 1), (EInt 1))), (EInt 2), (EInt 3)))");
    ("fn x -> x", "(ELambda ((EId \"x\"), (EId \"x\")))");
    ("fn x y -> x + y", "(ELambda ((EId \"x\"), (ELambda ((EId \"y\"), (EBinary (BinAdd, (EId \"x\"), (EId \"y\")))))))");
    ("x <- 1", "(EAssign ((EId \"x\"), (EInt 1)))");
    ("1 ? 2 : 3", "(ECond ((EInt 1), (EInt 2), (EInt 3)))");
    ("1 || 2", "(EBinary (BinLOr, (EInt 1), (EInt 2)))");
    ("1 && 2", "(EBinary (BinLAnd, (EInt 1), (EInt 2)))");
    ("1 < 2", "(EBinary (BinLT, (EInt 1), (EInt 2)))");
    ("1 <= 2", "(EBinary (BinLE, (EInt 1), (EInt 2)))");
    ("1 > 2", "(EBinary (BinGT, (EInt 1), (EInt 2)))");
    ("1 >= 2", "(EBinary (BinGE, (EInt 1), (EInt 2)))");
    ("1 == 2", "(EBinary (BinEql, (EInt 1), (EInt 2)))");
    ("1 != 2", "(EBinary (BinNeql, (EInt 1), (EInt 2)))");
    ("1 = 2", "(EBinary (BinEq, (EInt 1), (EInt 2)))");
    ("1 <> 2", "(EBinary (BinNeq, (EInt 1), (EInt 2)))");
    ("1::2", "(EBinary (BinCons, (EInt 1), (EInt 2)))");
    ("1::2::[3]", "(EBinary (BinCons, (EInt 1), (EBinary (BinCons, (EInt 2), (EBinary (BinCons, (EInt 3), ENil))))))");
    ("1 + 2 - 3", "(EBinary (BinAdd, (EInt 1), (EBinary (BinSub, (EInt 2), (EInt 3)))))");
    ("1 - 2 * 3", "(EBinary (BinSub, (EInt 1), (EBinary (BinMul, (EInt 2), (EInt 3)))))");
    ("1 - 2 / 3", "(EBinary (BinSub, (EInt 1), (EBinary (BinDiv, (EInt 2), (EInt 3)))))");
    ("1 - 2 % 3", "(EBinary (BinSub, (EInt 1), (EBinary (BinMod, (EInt 2), (EInt 3)))))");
    ("foo ()", "(EApply ((EId \"foo\"), EUnit))");
    ("foo 1", "(EApply ((EId \"foo\"), (EInt 1)))");
    ("bar 1 2", "(EApply ((EApply ((EId \"bar\"), (EInt 1))), (EInt 2)))");
    ("-1", "(EUnary (UMinus, (EInt 1)))");
    ("!2", "(EUnary (UNot, (EInt 2)))");
    ("'a'", "(EChar 'a')");
    ("\"abc\"", "(EString \"abc\")");
    ("[1,2,3]", "(EBinary (BinCons, (EInt 1), (EBinary (BinCons, (EInt 2), (EBinary (BinCons, (EInt 3), ENil))))))");
    ("[]", "ENil");
    ("()", "EUnit");
    ("[ ]", "ENil");
    ("type c = char",
        "(ETypeDecl ([], \"c\", (TD_Alias (TE_Name \"char\"))))");
    ("type f = unit -> int",
        "(ETypeDecl ([], \"f\", (TD_Alias (TE_Fun ((TE_Name \"unit\"), (TE_Name \"int\"))))))");
    ("type t = (int * char)",
        "(ETypeDecl ([], \"t\", (TD_Alias (TE_Tuple [(TE_Name \"int\");(TE_Name \"char\")]))))");
    ("type l = int list",
        "(ETypeDecl ([], \"l\", (TD_Alias (TE_Constr ((TE_Name \"int\"), (TE_Name \"list\"))))))");
    ("type ITree = int tree",
        "(ETypeDecl ([], \"ITree\", (TD_Alias (TE_Constr ((TE_Name \"int\"), (TE_Name \"tree\"))))))");
    ("type c = (float)",
        "(ETypeDecl ([], \"c\", (TD_Alias (TE_Name \"float\"))))");
    ("type 'a x = 'a",
        "(ETypeDecl ([0], \"x\", (TD_Alias (TE_Var 0))))");
    ("type 'a pair = 'a * 'a",
        "(ETypeDecl ([0], \"pair\", (TD_Alias (TE_Tuple [(TE_Var 0);(TE_Var 0)]))))");
    ("type ('a, 'b) pair = 'a * 'b",
        "(ETypeDecl ([0;1], \"pair\", (TD_Alias (TE_Tuple [(TE_Var 0);(TE_Var 1)]))))");
    ("type ('a, 'b, 'c) pairs = 'c * ('a, 'b) pair",
        "(ETypeDecl ([0;1;2], \"pairs\", (TD_Alias (TE_Tuple [(TE_Var 2);(TE_Constr ((TE_Tuple [(TE_Var 0);(TE_Var 1)]), (TE_Name \"pair\")))]))))");
    ("type list = List.t",
        "(ETypeDecl ([], \"list\", (TD_Alias (TE_Message ((TE_Name \"List\"), \"t\")))))");
    ("type color = | Red | Green | Blue",
        "(ETypeDecl ([], \"color\", (TD_Variant [(\"Red\", None);(\"Green\", None);(\"Blue\", None)])))");
    ("type color = | Red | Green | Blue | RGB (int * int * int)",
        "(ETypeDecl ([], \"color\", (TD_Variant [(\"Red\", None);(\"Green\", None);(\"Blue\", None);(\"RGB\", Some (TE_Tuple [(TE_Name \"int\");(TE_Name \"int\");(TE_Name \"int\")]))])))");
    ("type 'a option = | None | Some 'a",
        "(ETypeDecl ([0], \"option\", (TD_Variant [(\"None\", None);(\"Some\", Some (TE_Var 0))])))");
    ("type 'a tree = | Node 'a | Leaf ('a tree * 'a tree)",
        "(ETypeDecl ([0], \"tree\", (TD_Variant [(\"Node\", Some (TE_Var 0));(\"Leaf\", Some (TE_Tuple [(TE_Constr ((TE_Var 0), (TE_Name \"tree\")));(TE_Constr ((TE_Var 0), (TE_Name \"tree\")))]))])))");
    ("type point2d = { mut x : int; mut y : int }",
        "(ETypeDecl ([], \"point2d\", (TD_Record [(\"x\", true, (TE_Name \"int\"));(\"y\", true, (TE_Name \"int\"))])))");
    ("type 'a point2d = { x : 'a; y : 'a }",
        "(ETypeDecl ([0], \"point2d\", (TD_Record [(\"x\", false, (TE_Var 0));(\"y\", false, (TE_Var 0))])))");
    ("type point3d = {\n  mut x : float\n  mut y : float\n  mut z : float\n}",
        "(ETypeDecl ([], \"point3d\", (TD_Record [(\"x\", true, (TE_Name \"float\"));(\"y\", true, (TE_Name \"float\"));(\"z\", true, (TE_Name \"float\"))])))");
    ("type 'a ll = 'a list", "(ETypeDecl ([0], \"ll\", (TD_Alias (TE_Constr ((TE_Var 0), (TE_Name \"list\"))))))");
    ("decl len : 'a ll -> int", "(EDecl (\"len\", (TE_Fun ((TE_Constr ((TE_Var 0), (TE_Name \"ll\"))), (TE_Name \"int\")))))");
    ("type Bool = | False | True", "(ETypeDecl ([], \"Bool\", (TD_Variant [(\"False\", None);(\"True\", None)])))");

    ("let p = { x = 1; y = 2 } in p", "(ELet ([(EValDef (false, \"p\", (ERecord [(\"x\",1);(\"y\",2)])))], (EId \"p\")))");
]

let parser_test () =
    print_string "Parser Test: ";
    let do_test (txt, expected) =
        try
            verbose @@ "parse: " ^ txt;
            let e = Parser.parse_text true txt in
            verbose @@ "result: " ^ s_expr e;
            verbose @@ "result: " ^ s_expr_src e;
            if String.equal (s_expr_src e) expected then
                test_ok()
            else
                test_fail ("\n" ^ s_expr_src e ^ " <>\n" ^ expected)
        with
            | Error (pos, msg) -> test_fail @@ s_pos pos ^ "Error: " ^ msg
            | End_of_file -> test_fail "End_of_file"
            | Sys_error s -> test_fail @@ "Sys Error: " ^ s
    in
    List.iter do_test parser_test_data;
    print_newline ()

let type_test_data = [
    ("123", "int");
    ("'a'", "char");
    ("\"abc\"", "string");
    ("300+12", "int");
    ("300*12+3", "int");
    ("fn x -> x + 1", "int -> int");
    ("fn _ -> ()", "'a -> unit");
    ("(fn x -> x + 1) (300 * (12 + 3))", "int");
    ("1+2 < 3*4", "bool");
    ("2 * -(1+2)", "int");
    ("fn x y -> x + y", "'a -> 'a -> 'a");
    ("(fn x -> x) 1", "int");
    ("(fn x -> x) 1==1", "bool");
    ("(fn _ -> 1) 'a'", "int");
    ("(fn _ -> 2) 3", "int");
    ("[]", "'a list");
    ("[1,2,3]", "int list");
    ("1::2::[]", "int list");
    ("['a','b']", "char list");
    ("\"abc\"", "string");
    ("let id x = x in id 1", "int");
    ("(1,2)", "(int * int)");
    ("('a',2)", "(char * int)");
    ("[(1,2,'a')]", "(int * int * char) list");
    ("(fn x -> x)", "'a -> 'a");
    ("fn x -> fn y -> x", "'a -> 'b -> 'a");
    ("fn x -> fn y -> y", "'a -> 'b -> 'b");
    ("(fn x -> x + 1) 2 + (fn x -> x + -1) 3", "int");
    ("fn f -> fn g -> fn x -> g (f x)", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c");
    ("fn x -> fn y -> fn z -> x z (y z)", "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c");
    ("fn x -> let y = x + 1 in x", "int -> int");
    ("fn x -> let y = x + 1 in y", "int -> int");
    ("fn b -> fn x -> if x b then x else (fn x -> b)",
        "bool -> (bool -> bool) -> bool -> bool");
    ("fn x -> if 1==1 then x else (if x then 1==1 else 1==2)", "bool -> bool");
    ("fn x -> fn y -> if x then x else y", "bool -> bool -> bool");
    ("fn n -> (fn x -> x (fn y -> y)) (fn f -> f n)", "'a -> 'a");
    ("fn x -> fn y -> x y", "('a -> 'b) -> 'a -> 'b");
    ("fn x -> fn y -> x (y x)", "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> x (y x) (y x)", "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> fn z -> x (z x) (y (z x y))",
        "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c");
    ("let id = fn x -> x in let f = fn y -> id (y id) in f",
        "(('a -> 'a) -> 'b) -> 'b"); 
    ("let k = fn x -> fn y -> x in let k1 = fn x -> fn y -> k (x k) in k1",
        "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c");
    ("let s = fn x -> fn y -> fn z -> x z (y z) in let s1 = fn x -> fn y -> fn z -> x s (z s) (y s (z s)) in s1",
        "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f");
    ("let g = fn h -> fn t -> fn f -> fn x -> f h (t f x) in g",
        "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c");
    ("let s = fn x -> fn y -> fn z -> x z (y z) in let k = fn x -> fn y -> x in let kk = fn x -> fn y -> x in s k kk", "'a -> 'a");
    ("let s = fn x -> fn y -> fn z -> x z (y z) in let k = fn x -> fn y -> x in s k k",
        "'a -> 'a");
    ("let s = fn x -> fn y -> fn z -> x z (y z) in let kk = fn x -> fn y -> y in s kk kk",
        "'a -> 'b -> 'b");
(*
    ("fn x -> fn y -> fn z -> let b = x y z in if b then z y else y", "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a");
*)
    ("let pair = fn x1 -> fn x2 -> fn y -> y x1 x2 in let proj1 = fn p -> p (fn x1 -> fn x2 -> x1) in let proj2 = fn p -> p (fn x1 -> fn x2 -> x2) in proj1 (pair 1 100)", "int");
    ("let pair = fn x1 -> fn x2 -> fn y -> y x1 x2 in let proj1 = fn p -> p (fn x1 -> fn x2 -> x1) in let proj2 = fn p -> p (fn x1 -> fn x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)))", "int");
    ("let f = fn x -> x in if f 1==1 then f 1 else f 2", "int");
    ("let f = fn x -> 3 in f (1==1) + f 4", "int");
    ("fn b -> let f = fn x -> x in let g = fn y -> y in if b then f g else g f",
        "bool -> 'a -> 'a");
(*
    ("fn b -> fn f -> let g1 = fn x -> x f in let g2 = fn x -> x f in fn z -> if b then g1 z g2 else g2 z g1", "bool -> 'a -> ('a -> (('a -> 'b) -> 'b) -> 'c) -> 'c");
*)


    ("type integer = int", "unit");
    ("decl d : integer", "unit");
    ("d", "integer");

    ("type 'a ll = 'a list", "unit");
    ("decl c : int ll", "unit");
    ("c", "int ll");

    ("decl len : 'a list -> int", "unit");
    ("let len x = if x = [] then 0 else 1 + len (List.tl x) in len [1,2,3]", "int");

    ("fn x -> !x", "bool -> bool");

    ("type Point2D = { x : int; y : int }", "unit");
    ("{ x = 1; y = 2 }", "{x:int;y:int}");

    ("type Color = | Red | Green | Blue", "unit");
    ("Red", "Color");
    ("Color.Green", "Color");
    ("Color.Red", "Color");
    ("type TrafficLight = | Blue | Yellow | Red", "unit");
    ("Red", "TrafficLight");
    ("Color.Red", "Color");
    ("TrafficLight.Red", "TrafficLight");

    ("type A = | A | B int | C char", "unit");
    ("A.A", "A");
    ("B 10", "A");
    ("A.B 20", "A");
    ("C 'a'", "A");

    ("type 'a aaa = ('a * int)", "unit");
]

let type_test () =
    print_string "Type Test: ";
    let do_type_test (txt, expected) =
        try
            verbose @@ "source : " ^ txt;
            let e = Parser.parse_text true txt in
            let t = Type.infer e in
            verbose @@ "result  : " ^ s_expr e ^ " : " ^ s_typ t;
            verbose @@ "expected: " ^ expected;
            if String.equal (s_typ t) expected then
                test_ok ()
            else
                test_fail (s_typ t ^ " <> " ^ expected)
        with
            | Error (pos, msg) -> test_fail @@ s_pos pos ^ "Error: " ^ msg
            | End_of_file -> test_fail "End_of_file"
            | Sys_error s -> test_fail @@ "Sys Error: " ^ s
    in
    List.iter do_type_test type_test_data;
    print_newline ()

let eval_test_data = [
    ("[]", VNil);
    ("'a'", VChar 'a');
    ("\"abc\"", VString "abc");
    ("12", (VInt 12));
    ("300 + 12", (VInt 312));
    ("300 * 12 + 3", (VInt 3603));
    ("300 * (12 + 3)", (VInt 4500));
    ("300 / (12 - 3)", (VInt 33));
    ("1 < 2", VBool true);
    ("1 <= 1", VBool true);
    ("1 > 2", VBool false);
    ("2 >= 2", VBool true);
    ("2 == 2", VBool true);
    ("2 == 1", VBool false);
    ("2 != 1", VBool true);
    ("2 != 2", VBool false);
    ("2 = 2", VBool true);
    ("2 = 1", VBool false);
    ("2 <> 1", VBool true);
    ("2 <> 2", VBool false);
    ("'a' == 'a'", VBool true);
    ("'a' == 'b'", VBool false);
    ("'a' != 'a'", VBool false);
    ("'a' != 'b'", VBool true);
    ("'a' = 'a'", VBool true);
    ("'a' = 'b'", VBool false);
    ("'a' <> 'a'", VBool false);
    ("'a' <> 'b'", VBool true);
    ("'a' < 'b'", VBool true);
    ("'b' < 'a'", VBool false);
    ("'a' <= 'a'", VBool true);
    ("'b' <= 'a'", VBool false);
    ("'a' > 'b'", VBool false);
    ("'b' > 'a'", VBool true);
    ("'a' >= 'b'", VBool false);
    ("'a' >= 'a'", VBool true);
    ("\"abc\" == \"abc\"", VBool false);
    ("\"abc\" == \"def\"", VBool false);
    ("\"abc\" != \"abc\"", VBool true);
    ("\"abc\" != \"def\"", VBool true);
    ("\"abc\" = \"abc\"", VBool true);
    ("\"abc\" = \"def\"", VBool false);
    ("\"abc\" <> \"abc\"", VBool false);
    ("\"abc\" <> \"def\"", VBool true);
    ("\"abc\" < \"def\"", VBool true);
    ("\"abc\" > \"def\"", VBool false);
    ("1 > 2 || 2 > 1", VBool true);
    ("1 < 2 && 2 < 1", VBool false);
    ("-5", (VInt (-5)));
    ("1:: [2,3]", VCons ((VInt 1), VCons ((VInt 2), VCons ((VInt 3), VNil))));
    ("1::2:: [3]", VCons ((VInt 1), VCons ((VInt 2), VCons ((VInt 3), VNil))));
    ("[1,2,3]", VCons ((VInt 1), VCons ((VInt 2), VCons ((VInt 3), VNil))));
    ("[1,2,3] == 1:: [2,3]", VBool false);
    ("[1,2,3] == 1:: [2,3,4]", VBool false);
    ("[1,2,3] = 1:: [2,3]", VBool true);
    ("[1,2,3] = 1:: [2,3,4]", VBool false);
    ("let x = 1 in x", (VInt 1));
    ("let f = fn () -> 5 in f ()", VInt 5);
    ("let g = fn _ -> 8 in g 3", VInt 8);
    ("let b = fn x -> x + 1 in b 4", VInt 5);
    ("let add = fn x -> fn y -> x + y in add 1 2", VInt 3);
    ("let add = fn x -> fn y -> x + y in let add5 = add 5 in add5 3", VInt 8);
    ("let foo = fn x -> x + 2 in foo 4", VInt 6);
    ("let fact n = if n < 1 then 1 else n * fact (n-1) in fact 5", VInt 120);
    ("let id x = x in id 1", VInt 1);
    ("let mut x = 1 in\n  x <- 2\n  x", VInt 2);
    ("let x = 1; y = 2 in x + y", VInt 3);
    ("module A", VUnit);
    ("let length x = x = [] ? 0 : 1 + length (List.tl x) in length [1,2]", VInt 2);
    ("[1,2] + [3]", VCons(VInt 1, VCons(VInt 2, VCons(VInt 3, VNil))));
    ("\"ab\" + ['c']", VCons (VChar 'a', VCons (VChar 'b', VCons (VChar 'c', VNil))));
    ("(1,2)", VTuple [VInt 1;VInt 2]);
    ("('a',2)", VTuple [VChar 'a';VInt 2]);
    ("type 'a pair = 'a * 'a ", VUnit);
    ("decl a : int pair", VUnit);
    ("(1,2)", VTuple [VInt 1;VInt 2]);
    ("type Light = | Blue | Yellow | Red", VUnit);
    ("type Color = | Red | Green | Blue", VUnit);
    ("if Red = Green then 1 else 2", VInt 2);
    ("if Red = Blue then 1 else 2", VInt 2);
    ("if Red = Red then 1 else 2", VInt 1);
    ("if Color.Red = Color.Red then 1 else 2", VInt 1);
    ("if Light.Red = Yellow then 1 else 2", VInt 2);

    ("Blue", VVariant (TVariant ("Color", [("Red",None);("Green",None);("Blue",None)]), "Blue", None));
    ("Yellow", VVariant (TVariant ("Light", [("Blue",None);("Yellow",None);("Red",None)]), "Yellow", None));

    ("type Point2D = { x : int; y : int; }", VUnit);
    ("let p = { x = 1; y = 2 } in p", VRecord [("x",{contents=VInt 1});("y",{contents=VInt 2})]);

    ("type AA = | A | B int | C char", VUnit);
    ("A", VVariant (TVariant ("AA", [("A",None);("B",Some TInt);("C",Some TChar)]), "A", None));
    ("AA.A", VVariant (TVariant ("AA", [("A",None);("B",Some TInt);("C",Some TChar)]), "A", None));
    ("B 10", VVariant (TVariant ("AA", [("A",None);("B",Some TInt);("C",Some TChar)]), "B", Some (VInt 10)));
    ("AA.B 20", VVariant (TVariant ("AA", [("A",None);("B",Some TInt);("C",Some TChar)]), "B", Some (VInt 20)));
    ("C 'a'", VVariant (TVariant ("AA", [("A",None);("B",Some TInt);("C",Some TChar)]), "C", Some (VChar 'a')));
]


let eval_test () =
    print_string "Eval Test: ";
    let rec do_test (txt, expected) =
        try
            verbose @@ "parse: " ^ txt;
            let e = Parser.parse_text true txt in
            verbose @@ "result: " ^ s_expr e;
            let t = Type.infer e in
            let v = Eval.eval e in
            verbose @@ s_value v ^ " : " ^ s_typ t;
            test_eq v expected (s_value v ^ " <> " ^ s_value expected)
        with
            | Error (pos, msg) -> test_fail @@ s_pos pos ^ "Error: " ^ msg
            | End_of_file -> test_fail "End_of_file"
            | Sys_error s -> test_fail @@ "Sys Error: " ^ s
    in
    List.iter do_test eval_test_data;
    print_newline ()

let test () =
    if !do_scanner_test then
        scanner_test ();
    if !do_parser_test then
        parser_test ();
    if !do_type_test then
        type_test ();
    if !do_eval_test then
        eval_test ();

    test_report ();
    ()
