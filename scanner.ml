
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

type scan_t = {
    text : string;
    len : int;
    mutable current : int;
    mutable pos : pos;
}
let s_scanner s = Printf.sprintf "len=%d,current=%d" s.len s.current

let make_tokens tk pos acc = (tk, pos) :: acc

let is_eof scan = scan.current = scan.len
let peek scan =
    let res =
        if is_eof scan then
            None
        else
            Some scan.text.[scan.current]
    in
    debug_print @@ "peek[" ^ string_of_int scan.current ^ "]='" ^
        (match res with Some c -> (String.make 1 c) | None -> "EOF") ^ "'";
    res

let next scan =
    debug_in @@ "next [" ^ string_of_int scan.current ^ "]";
    if not (is_eof scan) then begin
        scan.current <- scan.current + 1;
        scan.pos <- { scan.pos with col = scan.pos.col + 1 }
    end;
    debug_out "next";
    scan

let next_line scan =
    debug_in @@ "next_line [" ^ string_of_int scan.current ^ "]";
    if not (is_eof scan) then begin
        scan.current <- scan.current + 1;
        scan.pos <- { scan.pos with line = scan.pos.line + 1; col = 1 }
    end;
    debug_out "next_line";
    scan

let span pred scan =
    let buf = Buffer.create 10 in
    let rec loop scan =
        match peek scan with
        | Some c when pred c ->
            Buffer.add_char buf c;
            loop @@ next scan
        | _ -> ()
    in loop scan;
    Buffer.contents buf


let scan_number scan =
    debug_in @@ "scan_number [" ^ string_of_int scan.current ^ "]";
    let get_num () =
        let is_digit = function '0'..'9' -> true | _ -> false in
        let buf = Buffer.create 10 in
        let dot_appear = ref false in
        let rec loop scan =
            match peek scan with
            | Some c when is_digit c ->
                Buffer.add_char buf c;
                loop @@ next scan
            | Some '.' ->
                if not !dot_appear then begin
                    dot_appear := true;
                    Buffer.add_char buf '.';
                    loop @@ next scan
                end
            | _ -> ()
        in loop scan;
        (!dot_appear, Buffer.contents buf)
    in
    let (is_float, num) = get_num () in
    let res =
        if is_float then
            Float (float_of_string num)
        else
            Int (int_of_string num)
    in
    debug_out "scan_number";
    res

let get_char scan =
    debug_print "get_char";
    match peek scan with
    | Some '\\' ->
        begin
            begin match peek @@ next scan with
                | Some 'n' -> ignore @@ next scan; Some '\n'
                | Some 'r' -> ignore @@ next scan; Some '\r'
                | Some 't' -> ignore @@ next scan; Some '\t'
                | Some c -> ignore @@ next scan; Some c
                | None -> None
            end
        end
    | Some c ->
        ignore @@ next scan;
        Some c
    | None -> None

let rec scan_char scan =
    let is_alpha = function 'a'..'z' -> true | _ -> false in
    let char_to_int c = Char.code c - Char.code 'a' in
    match get_char scan with
    | Some c ->
        if is_alpha c then
            if peek scan = Some '\'' then begin
                ignore @@ next scan;
                Char c
            end else
                TypId (char_to_int c)
        else if peek scan <> Some '\'' then
            error scan.pos "missing single-quote"
        else begin
            ignore @@ next scan;
            Char c
        end
    | None -> EOF

let rec scan_string scan =
    debug_print "scan_string";
    let buf = Buffer.create 10 in
    let rec loop scan =
        match peek scan with
        | Some '"' ->
            debug_print "scan_string \"";
            ignore @@ next scan
        | Some x ->
            debug_print @@ "scan_string '" ^ (String.make 1 x) ^ "'";
            (match get_char scan with
            | Some c ->
                Buffer.add_char buf c;
                loop scan
            | None ->
                error scan.pos "untermianted string")
        | None ->
            error scan.pos "unterminated string"
    in
    loop scan;
    String (Buffer.contents buf)
    
let get_ident scan =
    let is_alnum = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false in
    span is_alnum scan

let indent_stack = ref [0]

(*
let show_indent_stack () =
    let rec loop = function
        | [] -> ()
        | x::xs ->
            print_string @@ string_of_int x ^ " ";
            loop xs
    in
    print_string "indent_stack [ ";
    loop !indent_stack;
    print_endline "]"
*)

let rec scan_indent scan acc =
    debug_in @@ "scan_indent " ^ s_scanner scan;

    let rec loop scan n acc =
        debug_print @@ "scan_indent loop " ^ string_of_int n;
        match peek scan with
        | None -> acc
        | Some c ->
            (match c with
            | ' ' -> loop (next scan) (n+1) acc
            | '\t' -> loop (next scan) (n+8) acc
            | '\n' ->
                debug_print "scan_indent NEWLINE";
                let acc = make_tokens NEWLINE scan.pos acc in
                scan_indent scan acc
            | _ ->
                debug_print "scan_indent ANY";
                if !indent_stack = [] then
                    failwith "indent_stack BUG"
                else begin
                    let last_indent = List.hd !indent_stack in
                    debug_print @@ "last_indent=" ^
                            string_of_int last_indent ^ ", n=" ^ string_of_int n;
                    if last_indent = n then
                        acc
                    else if last_indent < n then begin
                        debug_print "push ";
                        indent_stack := n :: !indent_stack;
                        make_tokens INDENT scan.pos acc
                    end else begin (* last_indent > n *)
                        let rec dedent acc =
                            debug_print "dedent";
                            indent_stack := List.tl !indent_stack;
                            let acc = make_tokens DEDENT scan.pos acc in
                            if !indent_stack = [] then
                                error scan.pos "invalid indent"
                            else begin
                                let current = List.hd !indent_stack in
                                if current = n then
                                    acc
                                else
                                    dedent acc
                            end
                        in
                        dedent acc
                    end
                end)
    in
    let res =
        loop (next_line scan) 0 @@ make_tokens NEWLINE scan.pos acc
    in
    debug_out "scan_indent";
    res

let rec get_2char scan ch one two pos acc =
    debug_print @@ "get_2char '" ^ String.make 1 ch ^ "'";
    match peek @@ next scan with
    | Some c when c = ch ->
        get_tokens (next scan) @@ make_tokens two pos acc
    | Some _ | None ->
        get_tokens scan @@ make_tokens one pos acc

and skip_comment scan acc =
    match peek @@ next scan with
    | None ->
        get_tokens scan acc
    | Some '\n' ->
        get_tokens (next_line scan) @@ make_tokens NEWLINE scan.pos acc
    | Some _ ->
        skip_comment scan acc

and skip_nested_comment scan =
    let rec loop scan =
        match peek scan with
        | None -> error scan.pos "unexpected EOF"
        | Some '*' ->
            (match peek @@ next scan with
            | Some '/' ->
                next scan
            | _ -> loop scan)
        | Some '/' ->
            (match peek @@ next scan with
            | Some '*' -> loop (skip_nested_comment @@ next scan)
            | _ -> loop scan)
        | Some '\n' ->
            loop @@ next_line scan
        | _ ->
            loop @@ next scan
    in loop scan

and get_tokens scan acc =
    debug_in @@ "get_tokens current=" ^ string_of_int scan.current;
    let pos = scan.pos in
    let res =
        match peek scan with
        | None ->
            let rec dedent acc =
                debug_print "dedent";
                if !indent_stack = [] then
                    failwith "indent_stack empty"
                else begin
                    let last_indent = List.hd !indent_stack in
                    if last_indent = 0 then
                        acc
                    else begin
                        indent_stack := List.tl !indent_stack;
                        dedent (make_tokens DEDENT pos acc)
                    end
                end
            in
            let acc = dedent acc in
            debug_print "REV";
            List.rev acc
        | Some c ->
            (match c with
            | ' ' | '\t' | '\r' ->
                debug_print "get_tokens: space tab cr";
                get_tokens (next scan) acc
            | '\n' ->
                debug_print "get_tokens: newline";
                get_tokens scan @@ scan_indent scan acc
            | '\'' ->
                debug_print "get_tokens: char";
                let tk = scan_char @@ next scan in
                get_tokens scan @@ make_tokens tk pos acc
            | '"' ->
                debug_print "get_tokens: string";
                let tk = scan_string @@ next scan in
                get_tokens scan @@ make_tokens tk pos acc
            | '0'..'9' ->
                debug_print "get_tokens: number";
                let tk = scan_number scan in
                get_tokens scan @@ make_tokens tk pos acc
            | 'a'..'z' | 'A'..'Z' | '_' ->
                debug_print "get_tokens: identifier";
                let id = get_ident scan in
                let tk = match id with
                    | "module" -> MODULE | "import" -> IMPORT | "as" -> AS
                    | "type" -> TYPE | "let" -> LET | "in" -> IN | "mut" -> MUT
                    | "fn" -> FN | "if" -> IF | "then" -> THEN | "else" -> ELSE
                    | _ -> Id id
                in
                get_tokens scan @@ make_tokens tk pos acc
            | '{' -> get_tokens (next scan) @@ make_tokens BEGIN pos acc
            | '}' -> get_tokens (next scan) @@ make_tokens END pos acc
            | ';' -> get_tokens (next scan) @@ make_tokens SEMI pos acc
            | '.' -> get_tokens (next scan) @@ make_tokens DOT pos acc
            | ',' -> get_tokens (next scan) @@ make_tokens COMMA pos acc
            | ')' -> get_tokens (next scan) @@ make_tokens RPAR pos acc
            | ']' -> get_tokens (next scan) @@ make_tokens RBRA pos acc
            | '+' -> get_tokens (next scan) @@ make_tokens PLUS pos acc
            | '*' -> get_tokens (next scan) @@ make_tokens STAR pos acc
            | '%' -> get_tokens (next scan) @@ make_tokens PERCENT pos acc
            | '?' -> get_tokens (next scan) @@ make_tokens QUES pos acc
            | ':' -> get_2char scan ':' COLON DCOLON pos acc
            | '-' -> get_2char scan '>' MINUS ARROW pos acc
            | '|' -> get_2char scan '|' OR LOR pos acc
            | '&' -> get_2char scan '&' AND LAND pos acc
            | '(' -> get_2char scan ')' LPAR UNIT pos acc
            | '[' -> get_2char scan ']' LBRA NIL pos acc
            | '!' -> get_2char scan '=' NOT NEQL pos acc
            | '=' -> get_2char scan '=' EQ EQL pos acc
            | '>' -> get_2char scan '=' GT GE pos acc
            | '<' ->
                (match peek @@ next scan with
                | Some '-' ->
                    get_tokens (next scan) @@ make_tokens ASSIGN pos acc
                | Some '>' ->
                    get_tokens (next scan) @@ make_tokens NEQ pos acc
                | Some '=' ->
                    get_tokens (next scan) @@ make_tokens LE pos acc
                | _ ->
                    get_tokens scan @@ make_tokens LT pos acc)
            | '/' ->
                (match peek @@ next scan with
                | Some '/' ->
                    skip_comment (next scan) acc
                | Some '*' ->
                    get_tokens (skip_nested_comment @@ next scan) acc
                | _ ->
                    get_tokens scan @@ make_tokens SLASH pos acc)
            | '\\' ->
                (match peek @@ next scan with
                | Some '\n' ->
                    get_tokens (next_line scan) acc
                | Some _ ->
                    get_tokens scan acc
                | None ->
                    error pos "unexpected EOF")
            | c ->
                error pos @@ "unknown character '" ^ String.make 1 c ^ "'")
    in
    debug_out "get_tokens";
    res

let open_text txt =
    let scan = {
        text = txt;
        len = String.length txt;
        current = 0;
        pos = { filename = ""; line = 1; col = 1 };
    } in
    get_tokens scan []

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let open_file filename =
    let txt = load_file filename in
    let scan = {
        text = txt;
        len = String.length txt;
        current = 0;
        pos = { filename = filename; line = 1; col = 1 };
    } in
    get_tokens scan []

