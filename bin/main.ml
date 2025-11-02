open Cmdliner

(* Command: --repl *)
let repl_term =
  let doc = "Start the interactive REPL." in
  Arg.(value & flag & info [ "repl" ] ~doc)

(* Command: --compile FILE *)
let compile_term =
  let doc = "Compile a Karuta source file." in
  let file = Arg.(value & opt (some string) None & info [ "compile" ] ~docv:"FILE" ~doc) in
  file

(* Map arguments to behavior *)
let main repl_flag compile_opt =
  match repl_flag, compile_opt with
  | true, None ->
     Lwt_main.run (Lib.REPL.main ());
     `Ok ()
  | false, Some file -> begin
      match Lib.Executor.run file with
      | None -> failwith @@ "Could not execute file: " ^ file
      | Some computer -> print_endline @@ Lib.Crawler.StandardOut.query_args computer;
      `Ok ()
     end
  | true, Some _ ->
      `Error (false, "Options --repl and --compile cannot be used together.")
  | false, None ->
      `Error (false, "No command given. Try --repl or --compile <file>.")

let cmd =
  let open Cmdliner in
  let term =
    Term.(ret (const main $ repl_term $ compile_term))
  in
  let info =
    Cmd.info "karuta" ~version:"0.1.0" ~doc:"Karuta CLI"
  in
  Cmd.v info term

let () =
  exit (Cmd.eval cmd)

