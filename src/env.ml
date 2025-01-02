open Printf
module Dict = Hashtbl.Make(String)

type proc =
    { arity_min : int
    ; arity_max : int
    ; proc : t -> string list -> string }
and t =
    { vars : string Dict.t
    ; procs : proc Dict.t
    ; outer : t option
    ; mutable ret : string }

let create () = { vars = Dict.create 32 ; procs = Dict.create 8 ; outer = None ; ret = "" }
let create_inner env = { vars = Dict.create 8 ; procs = Dict.create 1 ; outer = Some env ; ret = "" }

let return env =
    let r = env.ret in
    env.ret <- ""; r

let rec reset env =
    Dict.reset env.vars;
    Dict.reset env.procs;
    env.ret <- "";
    match env.outer with
        | Some env -> reset env
        | None -> ()

let set_var env k v = Dict.replace env.vars k v
let set_proc env k arity_min arity_max proc =
    Dict.replace env.procs k { arity_min ; arity_max ; proc }

let rec var env k = match Dict.find_opt env.vars k with
    | Some v -> v
    | None -> match env.outer with
        | Some env -> var env k
        | None -> sprintf "Variable %s not found" k |> failwith

let rec proc env k args = match Dict.find_opt env.procs k with
    | Some v ->
        let l = List.length args in
        if v.arity_min <= l && v.arity_max >= l
        then
            env.ret <- v.proc env args
        else
            failwith @@ if v.arity_min = v.arity_max
            then
                sprintf "Procedure %s expects %i argument(s)"
                k v.arity_min
            else
                sprintf "Procedure %s expects between %i and %i argument(s)"
                k v.arity_min v.arity_max
    | None -> match env.outer with
        | Some env -> proc env k args
        | None -> sprintf "Procedure %s not found" k |> failwith

