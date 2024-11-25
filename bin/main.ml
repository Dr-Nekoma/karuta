module Option = struct
  let ( let+ ) = Option.bind
end

let _ =
  let open Option in
  let+ content =
    In_channel.with_open_text "examples/l0.krt" (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Lib.Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries ->
      let initialComputer = Lib.Machine.initialize () in
      let finalStore =
        Lib.Compiler.compile
          (decls_queries, Lib.Compiler.initialize (), initialComputer.store)
      in
      print_endline @@ Lib.Machine.show_store finalStore;
      None
