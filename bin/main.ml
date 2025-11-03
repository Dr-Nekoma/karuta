open Cmdliner

module Repl = struct
  type t = { files : string list }

  let file_term =
    let info =
      Arg.info [] ~doc:"List of optional Karuta source files."
        ~docv:"[FILE.krt]"
    in
    Arg.value (Arg.pos_all Arg.string [] info)

  let doc =
    "Start the interactive REPL with optional list of Karuta source files"

  let man = [ `S Manpage.s_description; `P "Start the interactive REPL." ]
  let term combine = Term.(const combine $ file_term)

  let cmd combine =
    let info = Cmd.info "repl" ~doc ~man in
    Cmd.v info (term combine)
end

module Compile = struct
  type t = { file : string }

  let file_term =
    let info =
      Arg.info [] ~doc:"Mandatory Karuta source file." ~docv:"FILE.krt"
    in
    Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

  let doc = "Compile a Karuta source file"
  let man = [ `S Manpage.s_description; `P "Compile a Karuta source file." ]
  let term combine = Term.(const combine $ file_term)

  let cmd combine =
    let info = Cmd.info "compile" ~doc ~man in
    Cmd.v info (term combine)
end

type cmd = Repl of Repl.t | Compile of Compile.t

let repl run =
  let combine files = Repl { files } |> run in
  Repl.cmd combine

let compile run =
  let combine file = Compile { file } |> run in
  Compile.cmd combine

let root_doc = "Welcome to Karuta!"

let root_man =
  [
    `S Manpage.s_description;
    `P "A relational programming language for both applications and databases!";
  ]

let root_term = Term.ret (Term.const (`Help (`Pager, None)))
let root_info = Cmd.info "karuta" ~doc:root_doc ~man:root_man

let help =
  let info = Cmd.info "help" in
  Cmd.v info root_term

let subcommands run = [ repl run; compile run; help ]

let parse_command_line_and_run (run : cmd -> unit) =
  run |> subcommands |> Cmd.group root_info |> Cmd.eval

let run : cmd -> unit = function
  | Repl { files } -> Lwt_main.run (Lib.REPL.main files)
  | Compile { file } -> (
      match Lib.Executor.run file with
      | None -> failwith @@ "Could not execute file: " ^ file
      | Some (_, computer) ->
          print_endline @@ Lib.Crawler.StandardOut.query_string computer)

let () = exit @@ parse_command_line_and_run run
