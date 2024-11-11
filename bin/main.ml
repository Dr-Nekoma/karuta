module Option = struct
  let (let+) = Option.bind
end

let _ =
  begin
    let fc = open_in "examples/test.kat" in
    let open Option in
    let+ content = try Some(input_line fc)
                   with End_of_file -> None in
    let () = close_in fc in
    match Lib.Parse.parse content with
    | None -> print_endline "File could not be parsed."; None
    | Some ast -> print_endline @@ Lib.Ast.show ast; None
  end
