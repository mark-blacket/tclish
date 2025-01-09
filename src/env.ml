open Printf
module Dict = Hashtbl.Make(String)

type t =
    { vars : string Dict.t
    ; procs : proc Dict.t
    ; outer : t option
    ; depth : int
    ; mutable ret : string }
and proc =
    { arity_min : int
    ; arity_max : int
    ; cmd : cmd }
and cmd = t -> string list -> string

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

let depth env = env.depth
let with_depth env depth =
    let rec find env =
        if env.depth = depth then env else Option.get env.outer |> find in
    if depth < 0 || depth > env.depth
    then sprintf "No environment with depth %d" depth |> failwith
    else find env

let set_var env k v = Dict.replace env.vars k v
let set_proc env k arity_min arity_max cmd =
    Dict.replace env.procs k { arity_min ; arity_max ; cmd }
let remove_var env k = Dict.remove env.vars k
let remove_proc env k = Dict.remove env.procs k

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
            env.ret <- v.cmd env args
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

let new_env v p outer depth =
    { vars = Dict.create v ; procs = Dict.create p ; outer ; depth ; ret = "" }

let create procs =
    let env = new_env 32 8 None 0 in
    List.iter (fun (name, arity_min, arity_max, proc) ->
        set_proc env name arity_min arity_max proc) procs;
    env

let create_inner env =
    env.depth + 1 |> new_env 32 8 @@ Some env
