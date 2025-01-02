[@@@warning "-8"]

let set env = function
    | name :: [] -> Env.var env name
    | name :: value :: [] -> Env.set_var env name value; value

let subst env = function str :: [] -> Interp.subst env str

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

let rec if_ env = function
    | [] -> ""
    | str :: [] -> Interp.eval env str
    | cond :: str :: rest ->
        if Interp.eval env cond = ""
        then if_ env rest
        else Interp.eval env str

let rec while_ env = function (cond :: body :: []) as strs ->
    if Interp.eval env cond <> ""
    then begin
        begin try
            Interp.eval env body
        with
            | Interp.Break -> ""
            | Interp.Continue -> while_ env strs 
        end |> ignore;
        while_ env strs
    end else ""

let for_ env = function start :: test :: next :: body :: [] ->
    let rec inner step =
        Interp.eval env step |> ignore;
        if Interp.eval env test <> ""
        then begin
            begin try
                Interp.eval env body
            with
                | Interp.Break -> ""
                | Interp.Continue -> inner next
            end |> ignore;
            inner next
        end else "" in
    inner start

let return _ = function
    | []        -> Interp.Return ""  |> raise
    | str :: [] -> Interp.Return str |> raise

let break _ =    function [] -> Interp.Break    |> raise
let continue _ = function [] -> Interp.Continue |> raise

let defaults env =
    let m = Int.max_int in
    [ "set",      1, 2, set
    ; "subst",    1, 1, subst
    ; "proc",     3, 3, proc
    ; "if",       2, m, if_
    ; "while",    2, 2, while_
    ; "for",      4, 4, for_
    ; "return",   0, 1, return
    ; "break",    0, 0, break
    ; "continue", 0, 0, continue
    ] |> List.iter (fun (n, l, r, p) -> Env.set_proc env n l r p)
