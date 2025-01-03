open Printf
[@@@warning "-8"]

let eval env = function str :: [] -> Interp.eval env str
let subst env = function str :: [] -> Interp.subst env str

let set env = function
    | name :: [] -> Env.var env name
    | name :: value :: [] -> Env.set_var env name value; value

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
    let depth = try
        if String.starts_with ~prefix:"#" lvl && (len > 1)
        then
            len - 1 |> String.sub lvl 1 |> int_of_string
        else Env.depth env - int_of_string lvl
    with Failure _ -> sprintf "String %s cannot be converted to int" lvl |> failwith in
    Interp.eval (Env.with_depth env depth) body

let return _ = function
    | []        -> Interp.Return ""  |> raise
    | str :: [] -> Interp.Return str |> raise

let break _ =    function [] -> Interp.Break    |> raise
let continue _ = function [] -> Interp.Continue |> raise

let list =
    let m = Int.max_int in
    [ "eval",     1, 1, eval
    ; "subst",    1, 1, subst
    ; "set",      1, 2, set
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
    ; "continue", 0, 0, continue ]
