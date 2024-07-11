
open Ast


let rec repl () =
    try
        print_string "> ";
        flush stdout;
        let txt = input_line stdin in
        verbose @@ "input=" ^ txt;
        let e = Parser.parse_text true txt in
        verbose @@ "parsed:" ^ s_expr e;
        let t = Type.infer e in
        let v = Eval.eval e in
        print_endline @@ s_value v ^ " : " ^ s_typ t;
        repl ()
    with
        | Error (pos, msg) ->
            print_endline @@ s_pos pos ^ ":Error: " ^ msg;
            repl ()
        | End_of_file -> ()
        | Sys_error s -> print_endline s


let () =
    print_endline "wktk version 0.1";
    Symbol.init ();
    Builtins.init ();

    let filenames = ref [] in
    let do_test = ref false in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose_flag := true), "  verbose");
            ("-t", Arg.Unit (fun () -> do_test := true), "  test");
            ("-ds", Arg.Unit (fun () -> Scanner.debug_scope_flag := true), " debug scanner");
            ("-dp", Arg.Unit (fun () -> Parser.debug_scope_flag := true), " debug parser");
            ("-dt", Arg.Unit (fun () -> Type.debug_scope_flag := true), " debug type");
            ("-de", Arg.Unit (fun () -> Eval.debug_scope_flag := true), " debug eval");
            ("-ns", Arg.Unit (fun () -> Test.do_scanner_test := false), " skip scanner test");
            ("-np", Arg.Unit (fun () -> Test.do_parser_test := false), " skip parser test");
            ("-nt", Arg.Unit (fun () -> Test.do_type_test := false), " skip type test");
            ("-ne", Arg.Unit (fun () -> Test.do_eval_test := false), " skip eval test");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v][-ds][-dp][-dt][-de][-ns][-np][-nt][-ne] filename...";

    ignore @@ Type.load_source "builtins.wt";

    if !do_test then
        Test.test ()
    else if !filenames <> [] then
        List.iter (fun name -> ignore @@ Type.load_source name) !filenames
    else
        repl ()

