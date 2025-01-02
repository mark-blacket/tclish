open Printf
exception Return of string
exception Break
exception Continue

let ( .![] ) s p = String.unsafe_get s !p
let ( .:[] ) s p = String.unsafe_get s p

let rec split str =
    printf "len = %d\n" @@ String.length str;
    split_pos str |> List.concat |> List.map @@ fun (s, e) ->
        printf "%d-%d\n" s e;
        e - s |> String.sub str s

and split_pos str =
    let len = String.length str in
    let rec inner p =
        if !p >= len then []
        else
            let cmd = split_cmd str !p p in
            cmd :: inner p in
    ref 0 |> inner

and split_cmd str start p =
    let curr = !p in incr p;
    match str.:[curr] with
        | '\n' | '\r' | ';' | '\x00' -> skip_spaces str p; [start, curr]
        | ' ' | '\t' -> skip_spaces str p; (start, curr) :: split_cmd str !p p
        | '\\' -> skip_backslash str p; split_cmd str start p
        | '#' -> comment str p; skip_spaces str p; [start, curr]
        | '"' -> split_quotes str start p  ; split_cmd str start p
        | '{' -> split_braces str start p 0; split_cmd str start p
        | '[' -> split_subcmd str start p 0; split_cmd str start p
        | ']' -> failwith "Unbalanced brackets"
        | '}' -> failwith "Unbalanced braces"
        | _ -> split_cmd str start p

and skip_spaces str p = match str.![p] with
    | ' ' | '\t' -> incr p; skip_spaces str p
    | _ -> ()

and skip_backslash str p =
    if str.![p] <> '\x00' then incr p

and comment str p = match str.![p] with
    | '\n' | '\x00' -> ()
    | _ -> incr p; comment str p

and split_quotes str start p =
    let c = str.![p] in incr p;
    match c with
        | '\x00' -> failwith "Unbalanced quotes"
        | '\\' -> skip_backslash str p; split_quotes str start p
        | '#' -> comment str p; split_quotes str start p
        | '"' -> ()
        | '[' -> split_subcmd str !p p 0; split_quotes str start p
        | ']' -> failwith "Unbalanced brackets"
        | _ -> split_quotes str start p

and split_braces str start p depth =
    let c = str.![p] in incr p;
    match c with
        | '\x00' -> failwith "Unbalanced braces"
        | '{' -> depth + 1 |> split_braces str start p
        | '}' ->
            if depth > 0
            then depth - 1 |> split_braces str start p
        | _ -> split_braces str start p depth

and split_subcmd str start p depth =
    let c = str.![p] in incr p;
    match c with
        | '\x00' -> failwith "Unbalanced braces"
        | '[' -> depth + 1 |> split_subcmd str start p
        | ']' ->
            if depth > 0
            then depth - 1 |> split_subcmd str start p
        | '"' -> split_quotes str start p  ; split_subcmd str start p depth
        | '{' -> split_braces str start p 0; split_subcmd str start p depth
        | '}' -> failwith "Unbalanced braces"
        | _ -> split_subcmd str start p depth

let rec eval env str =
    let eval_cmd pos = subst_pos env str pos |> function
        | [] -> ()
        | name :: args -> Env.proc env name args in
    try
        split_pos str |> List.iter eval_cmd;
        Env.return env
    with
        | Return _ -> failwith "Return command outside of procedure"
        | Break    -> failwith "Break command outside of loop"
        | Continue -> failwith "Continue command outside of loop"

and subst env str =
    let p = ref 0 in
    let buf = Buffer.create 16 in
    let e = String.length str in
    subst_word env str buf p e

and subst_pos env str =
    let p = ref 0 in
    let buf = Buffer.create 16 in
    List.map @@ fun (s, e) ->
        p := s; Buffer.clear buf;
        subst_word env str buf p e

and subst_word env str buf p e =
    while !p < e do
        let c = str.![p] in incr p;
        match c with
            | '\\' -> subst_backslash str buf p
            | '$' -> subst_var env str buf p
            | '"' -> subst_quotes env str buf p
            | '{' -> subst_braces str buf p 0
            | '[' -> subst_subcmd env str buf p
            | c -> Buffer.add_char buf c
    done; Buffer.contents buf

and subst_backslash str buf p =
    begin match str.![p] with
        | 'a'  -> '\x07'
        | 'b'  -> '\x08'
        | 'f'  -> '\x0C'
        | 'n'  -> '\x0A'
        | 't'  -> '\x0D'
        | 'v'  -> '\x09'
        | '\n' -> ' '
        | c    -> c
    end |> Buffer.add_char buf;
    incr p;

and subst_var env str buf p =
    let name = Buffer.create 8 in
    let rec inner () = match str.![p] with
        | '\x00' | '\n' | '\r' | ';' | ' ' | '\t' | '$'
        | '\\' | '#' | '"' | '{' | '[' | ']' | '}' ->
            let name = Buffer.contents name in
            Env.var env name |> Buffer.add_string buf
        | c -> incr p; Buffer.add_char name c; inner () in
    inner ()

and subst_quotes env str buf p =
    let c = str.![p] in incr p;
    match c with
        | '"' -> ()
        | '\\' -> subst_backslash str buf p; subst_quotes env str buf p
        | '$' -> subst_var env str buf p; subst_quotes env str buf p
        | '[' -> subst_subcmd env str buf p; subst_quotes env str buf p
        | c -> Buffer.add_char buf c; subst_quotes env str buf p

and subst_braces str buf p lvl =
    let c = str.![p] in incr p;
    match c with
        | '\\' ->
            if str.:[!p + 1] == '\n'
            then begin Buffer.add_char buf ' '; incr p end
            else Buffer.add_char buf '\\';
            subst_braces str buf p lvl
        | '{' ->
            Buffer.add_char buf '{';
            lvl + 1 |> subst_braces str buf p
        | '}' ->
            if lvl > 0
            then begin
                Buffer.add_char buf '}';
                lvl - 1 |> subst_braces str buf p
            end
        | c -> Buffer.add_char buf c; subst_braces str buf p lvl

and subst_subcmd env str buf p =
    let cmd = Buffer.create 16 in
    let rec inner lvl =
        let c = str.![p] in incr p;
        match c with
            | '[' -> Buffer.add_char cmd c; lvl + 1 |> inner
            | ']' ->
                if lvl <> 0
                then begin
                    Buffer.add_char cmd c; lvl - 1 |> inner
                end
            | c -> Buffer.add_char cmd c; inner lvl in
    inner 0; Buffer.contents cmd |> eval env |> Buffer.add_string buf
