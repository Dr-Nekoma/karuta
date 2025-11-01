module Option = struct
  let ( let+ ) = Option.bind
end

let bimap f g (a1, a2) = (f a1, g a2)

let update_store (computer : Machine.t)
      (store : Machine.Cell.t Machine.Store.t) : Machine.t =
  { computer with store }

let load filepath =
  let open Option in
  let+ content =
    In_channel.with_open_text filepath (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Parse.parse content with
  | [] ->
     print_endline "File could not be parsed.";
     None
  | decls_queries -> (
    let compiler, computer =
      Machine.initialize () |> fun initialComputer ->
                               Compiler.compile
                                 ( Preprocessor.group_clauses decls_queries,
                                   Compiler.initialize (),
                                   initialComputer.store )
                               |> bimap Fun.id (update_store initialComputer)
    in
    match compiler.entry_point with
    | None -> None
    | Some entry_point ->
       let computer =
         Evaluator.eval compiler.functor_table
           { computer with p_register = entry_point.p_register }
       in
       Some (compiler,computer))

let run content compiler_and_computer: (Compiler.t * Machine.t) option =
  match Parse.parse content, compiler_and_computer with
  | [], _ ->
     print_endline "Incorrect definition :(";
     None
  | decls_queries, Some (current_compiler, current_computer) -> (
    let compiler, computer =
      Compiler.compile
        ( Preprocessor.group_clauses decls_queries,
          {current_compiler with entry_point = None},
          current_computer.store )
      |> bimap Fun.id (update_store current_computer)
    in
    match compiler.entry_point with
    | None -> Some (compiler, computer)
    | Some entry_point ->
       let computer =
         Evaluator.eval compiler.functor_table
           { computer with p_register = entry_point.p_register }
       in
       Some (compiler, computer))
  | decls_queries, None -> begin
      let compiler, computer =
        Machine.initialize () |> fun initialComputer ->
                                 Compiler.compile
                                   ( Preprocessor.group_clauses decls_queries,
                                     Compiler.initialize (),
                                     initialComputer.store )
                                 |> bimap Fun.id (update_store initialComputer)
      in
      match compiler.entry_point with
      | None -> Some (compiler, computer)
      | Some entry_point ->
         let computer =
           Evaluator.eval compiler.functor_table
             { computer with p_register = entry_point.p_register }
         in
         Some (compiler, computer)
    end
