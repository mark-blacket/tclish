open Tclish

let run env s = String.concat "\n" s |> Interp.eval env |> print_endline

let rec repl env =
    let rec next_line () = match read_line () with
        | "" -> []
        | s -> s :: next_line () in
    try
        next_line () |> run env;
        repl env
    with End_of_file -> ()

let file env f =
    let ch = open_in f in
    let rec read () =
        try input_line ch :: read () with End_of_file -> []
    in read () |> run env

let () =
    let mode = ref false in
    let spec = ["-i", Arg.Set mode, "Enter interactive mode after executing scripts"] in
    let env = Env.create Cmd.defaults in
    Arg.parse spec (fun f -> file env f) "Usage: tclish [-i] [script ...]";
    if !mode then repl env
