
type 'a ll = 'a list

let rec length : 'a ll -> int =
    fun x -> if x = [] then 0
        else 1 + length (List.tl x)

type integer = int
let foo : integer -> int = fun x -> x + 1

type number = int

let _ =
    print_endline ("legnth [1,2] = " ^ string_of_int (length [1,2]));
    let x : number = 2 in
    print_endline ("foo x = " ^ string_of_int (foo x))

