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
       let queries = List.filter
                       (fun dq -> match dq with
                                  | Lib.Ast.Query _ -> true
                                  | _ -> false) decls_queries in
       let decls = List.filter
                       (fun dq -> match dq with
                                  | Lib.Ast.Query _ -> false
                                  | _ -> true) decls_queries in
       let unified = List.map 
       print_endline @@ Lib.Ast.show_ts asts; None
  end
