module Option = struct
  let (let+) = Option.bind
end

let _ =
  begin
    let open Option in
    let+ content = In_channel.with_open_text
                     "examples/test.krt"
                     (fun fc -> try Some(In_channel.input_all fc) with End_of_file -> None)
    in match Lib.Parse.parse content with
    | [] -> print_endline "File could not be parsed."; None
    | decls_queries ->
       let initialComputer = Lib.Machine.initialize() in
       let (queries, finalComputer) = Lib.Compiler.compile decls_queries initialComputer [] in
       print_endline @@ List.map (Lib.Compiler.query finalComputer) queries; None
  end
