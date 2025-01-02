open Tclish

let run env s = Interp.eval env s |> print_endline

let rec repl env =
    let rec next_line () = match read_line () with
        | "" -> []
        | s -> s :: next_line () in
    try
        next_line () |> String.concat "\n" |> run env;
        repl env
    with
        | End_of_file -> ()

let file env f =
    let ch = open_in f in
    let rec read lst =
        try input_line ch :: lst |> read with End_of_file -> lst
    in read [] |> List.rev |> String.concat "\n" |> run env

let () =
    let mode = ref false in
    let spec = ["-i", Arg.Set mode, "Enter interactive mode after executing scripts"] in
    let usage = "Usage: tclish [-i] [script ...]" in
    let env = Env.create () in
    Cmd.defaults env;
    Arg.parse spec (fun f -> file env f) usage;
    if !mode then repl env
