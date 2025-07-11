module Option = struct
  let ( let+ ) = Option.bind
end

let bimap f g (a1, a2) = (f a1, g a2)

let update_store (computer : Karuta_lib.Machine.t)
    (store : Karuta_lib.Machine.Cell.t Karuta_lib.Machine.Store.t) : Karuta_lib.Machine.t =
  { computer with store }

let _ =
  let open Option in
  let+ content =
    In_channel.with_open_text "examples/triangular.krt" (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Karuta_lib.Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries -> (
      let compiler, computer =
        Karuta_lib.Machine.initialize () |> fun initialComputer ->
        Karuta_lib.Compiler.compile
          ( Karuta_lib.Preprocessor.group_clauses decls_queries,
            Karuta_lib.Compiler.initialize (),
            initialComputer.store )
        |> bimap Fun.id (update_store initialComputer)
      in
      match compiler.entry_point with
      | None -> None
      | Some entry_point ->
          let computer =
            Karuta_lib.Evaluator.eval compiler.functor_table
              { computer with p_register = entry_point.p_register }
          in
          print_endline @@ Karuta_lib.Print.query_args computer;
          Some computer)

let _ =
  let open Option in
  let+ content =
    In_channel.with_open_text "examples/triangular.krt" (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Karuta_lib.Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries -> begin
      let compiler, computer =
        Karuta_lib.Machine.initialize () |> fun initialComputer ->
        Karuta_lib.Compiler.compile
          ( Karuta_lib.Preprocessor.group_clauses decls_queries,
            Karuta_lib.Compiler.initialize (),
            initialComputer.store )
        |> bimap Fun.id (update_store initialComputer)
      in
      match compiler.entry_point with
      | None -> None
      | Some entry_point ->
          let computer =
            Karuta_lib.Evaluator.eval compiler.functor_table
              { computer with p_register = entry_point.p_register }
          in
          List.iter (fun x -> print_endline @@ Karuta_lib.Protocol.show_ast x) @@ Karuta_lib.Print.query_ast_args computer;
          Some computer 
      end

let () = Karuta_lib.Server.start ()
