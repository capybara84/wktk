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
    (";");
    ("\n\n;\n");
    ("let one = 1 in one");
    ("let _ = 2 in ()");
    ("let id x = x in id 0");
    ("let add x y = x + y in add 1 2");
    ("let foo () = () in foo");
    ("let bar _ = () in bar ()");
    ("let _ = 1 in ()");
    ("let one = 1; two = 2 in one + two");
    ("let mut var = 1 in var <- 2");
    ("if 1 == 1 then 2 else 3");
    ("fn x -> x");
    ("fn x y -> x + y");
    ("x <- 1");
    ("1 ? 2 : 3");
    ("1 || 2");
    ("1 && 2");
    ("1 < 2");
    ("1 <= 2");
    ("1 > 2");
    ("1 >= 2");
    ("1 == 2");
    ("1 != 2");
    ("1 = 2");
    ("1 <> 2");
    ("1::2");
    ("1::2::[3]");
    ("1 + 2 - 3");
    ("1 - 2 * 3");
    ("1 - 2 / 3");
    ("1 - 2 % 3");
    ("foo ()");
    ("foo 1");
    ("bar 1 2");
    ("-1");
    ("!2");
    ("'a'");
    ("\"abc\"");
    ("[1,2,3]");
    ("[]");
    ("()");
]

let parser_test () =
    print_string "Parser Test: ";
    let do_test txt =
        try
            verbose @@ "parse: " ^ txt;
            let e = Parser.parse_text true txt in
            verbose @@ "result: " ^ s_expr e;
            test_ok()
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
]

let type_test () =
    print_string "Type Test: ";
    let do_type_test (txt, expected) =
        try
            let e = Parser.parse_text true txt in
            let t = Type.infer e in
            verbose @@ s_expr e ^ " : " ^ s_typ t;
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
    ("let a = fn x -> x + 1 in a 4", VInt 5);
    ("let add = fn x -> fn y -> x + y in add 1 2", VInt 3);
    ("let add = fn x -> fn y -> x + y in let add5 = add 5 in add5 3", VInt 8);
    ("let foo = fn x -> x + 2 in foo 4", VInt 6);
    ("let fact n = if n < 1 then 1 else n * fact (n-1) in fact 5", VInt 120);
    ("let id x = x in id 1", VInt 1);
    ("let mut x = 1 in\n  x <- 2\n  x", VInt 2);
    ("let x = 1; y = 2 in x + y", VInt 3);
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
