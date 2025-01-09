open Printf
[@@@warning "-8"]

let int_of_string x = match int_of_string_opt x with
    | Some x -> x
    | None -> sprintf "String %s cannot be converted to int" x |> failwith

let float_of_string x = match float_of_string_opt x with
    | Some x -> x
    | None -> sprintf "String %s cannot be converted to float" x |> failwith

let eval env = function str :: [] -> Interp.eval env str
let subst env = function str :: [] -> Interp.subst env str

let set env = function
    | name :: [] -> Env.var env name
    | name :: value :: [] -> Env.set_var env name value; value

let unset env = function name :: [] -> Env.remove_var env name; ""

let proc env = function name :: args :: body :: [] ->
    let args = Interp.split args in
    let arity = List.length args in
    let proc env vals =
        let env = Env.create_inner env in
        List.iter2 (Env.set_var env) args vals;
        try
            Interp.eval env body
        with Interp.Return s -> s in
    Env.set_proc env name arity arity proc; ""

let puts _   = function str :: [] -> print_endline str; ""
let gets env = function str :: [] ->
    let s = read_line () in
    Env.set_var env str s; s

let rec cond env = function
    | [] -> ""
    | str :: [] -> Interp.eval env str
    | tst :: str :: rest ->
        if Interp.eval env tst = ""
        then cond env rest
        else Interp.eval env str

let rec while_ env = function (tst :: body :: []) as args ->
    match Interp.eval env tst with
        | "" -> ""
        | _ ->
            begin try Interp.eval env body with
                | Interp.Break -> ""
                | Interp.Continue -> while_ env args
            end |> ignore;
            while_ env args

let for_ env = function start :: tst :: next :: body :: [] ->
    let rec loop step =
        Interp.eval env step |> ignore;
        match Interp.eval env tst with
            | "" -> ""
            | _ ->
                begin try Interp.eval env body with
                    | Interp.Break -> ""
                    | Interp.Continue -> loop next
                end |> ignore;
                loop next in
    loop start

let foreach env = function name :: list :: body :: [] ->
    let rec loop = function
        | [] -> ""
        | arg :: args ->
            Env.set_var env name arg;
            begin try Interp.eval env body with
                | Interp.Break -> ""
                | Interp.Continue -> loop args
            end |> ignore;
            loop args in
    Interp.split list |> loop

let uplevel env = function lvl :: body :: [] ->
    let len = String.length lvl in
    let depth =
        if String.starts_with ~prefix:"#" lvl && (len > 1)
        then
            len - 1 |> String.sub lvl 1 |> int_of_string
        else Env.depth env - int_of_string lvl in
    Interp.eval (Env.with_depth env depth) body

let return _ = function
    | []        -> Interp.Return ""  |> raise
    | str :: [] -> Interp.Return str |> raise

let break _ =    function [] -> Interp.Break    |> raise
let continue _ = function [] -> Interp.Continue |> raise

let expr_int op _ args =
    let rec expr = function
        | x :: [] -> int_of_string x
        | x :: xs -> expr xs |> op @@ int_of_string x in
    expr args |> string_of_int

let expr_float op _ args =
    let rec expr = function
        | x :: [] -> float_of_string x
        | x :: xs -> expr xs |> op @@ float_of_string x in
    expr args |> string_of_float

let expr_bool op _ args =
    let rec expr = function
        | x :: [] -> x <> ""
        | x :: xs -> expr xs |> op @@ (x <> "") in
    if expr args then "1" else ""

let unary_float_to_int op _ = function x :: [] ->
    float_of_string x |> op |> truncate |> string_of_int

let unary_float op _ = function x :: [] ->
    float_of_string x |> op |> string_of_float

let unary_int op _ = function x :: [] ->
    int_of_string x |> op |> string_of_int

let unary_bool op _ = function b :: [] ->
    if b <> "" |> op then "1" else "" 

let string_cmp op _ = function l :: r :: [] ->
    if op l r then "1" else ""

let float_cmp op _ = function l :: r :: [] ->
    if op (float_of_string l) (float_of_string r) then "1" else ""

let list =
    let m = Int.max_int in
    [ "eval",     1, 1, eval
    ; "subst",    1, 1, subst
    ; "set",      1, 2, set
    ; "unset",    1, 1, unset
    ; "proc",     3, 3, proc
    ; "puts",     1, 1, puts
    ; "gets",     1, 1, gets
    ; "cond",     2, m, cond
    ; "while",    2, 2, while_
    ; "for",      4, 4, for_
    ; "foreach",  3, 3, foreach
    ; "uplevel",  2, 2, uplevel
    ; "return",   0, 1, return
    ; "break",    0, 0, break
    ; "continue", 0, 0, continue
    ; "+",        2, m, expr_float ( +. )
    ; "-",        2, m, expr_float ( -. )
    ; "*",        2, m, expr_float ( *. )
    ; "/",        2, m, expr_float ( /. )
    ; "%",        2, m, expr_float mod_float
    ; "**",       2, m, expr_float ( ** )
    ; "&",        2, m, expr_int ( land )
    ; "|",        2, m, expr_int ( lor )
    ; "^",        2, m, expr_int ( lxor )
    ; "<<",       2, 2, expr_int ( lsl )
    ; ">>",       2, 2, expr_int ( lsr )
    ; "!>>",      2, 2, expr_int ( asr )
    ; "~",        1, 1, unary_int lnot
    ; "neg",      1, 1, unary_float ( ~-. )
    ; "abs",      1, 1, unary_float Float.abs
    ; "ceil",     1, 1, unary_float_to_int Float.ceil
    ; "floor",    1, 1, unary_float_to_int Float.floor
    ; "round",    1, 1, unary_float_to_int Float.round
    ; "=",        2, 2, float_cmp ( = )
    ; ">=",       2, 2, float_cmp ( >= )
    ; "<=",       2, 2, float_cmp ( <= )
    ; ">",        2, 2, float_cmp ( > )
    ; "<",        2, 2, float_cmp ( < )
    ; "!=",       2, 2, float_cmp ( <> )
    ; "eq",       2, 2, string_cmp ( = )
    ; "ge",       2, 2, string_cmp ( >= )
    ; "le",       2, 2, string_cmp ( <= )
    ; "gt",       2, 2, string_cmp ( > )
    ; "lt",       2, 2, string_cmp ( < )
    ; "ne",       2, 2, string_cmp ( <> )
    ; "and",      2, m, expr_bool ( && )
    ; "or",       2, m, expr_bool ( || )
    ; "xor",      2, m, expr_bool (fun l r -> (l || r) && not (l && r))
    ; "not",      1, 1, unary_bool not ]
