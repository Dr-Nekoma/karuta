let bimap f g (a1, a2) = (f a1, g a2)
let flip f x y = f y x

let update_store (computer : Machine.t) (store : Machine.Cell.t Machine.Store.t)
    : Machine.t =
  { computer with store }

module List = struct
  let to_option : 'a list -> 'a list option = function
    | [] -> None
    | other -> Some other

  let flatten = List.flatten
end

let verify_parsed filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses

let parse (filepath : string) : Ast.parser_clause list =
  let get_input fc =
    try Some (In_channel.input_all fc) with End_of_file -> None
  in
  filepath
  |> flip In_channel.with_open_text get_input
  |> flip Option.bind (Fun.compose List.to_option Parse.parse)
  |> Option.to_list |> List.flatten |> verify_parsed filepath

module Option = struct
  let ( let+ ) = Option.bind
end

let compile' ((compiler, computer) : Compiler.t * Machine.t) :
    Ast.parser_clause list -> Compiler.t * Machine.t = function
  | [] ->
      failwith "Compiler error: unreachable when executing compile function."
  | decls_queries ->
      (Preprocessor.group_clauses decls_queries, compiler, computer.store)
      |> Compiler.compile
      |> bimap Fun.id (update_store computer)

let compile = compile' (Compiler.initialize (), Machine.initialize ())

let eval ((compiler, computer) : Compiler.t * Machine.t) :
    (Compiler.t * Machine.t) option =
  match compiler.entry_point with
  | None -> None
  | Some entry_point ->
      let computer =
        Evaluator.eval compiler.functor_table
          { computer with p_register = entry_point.p_register }
      in
      Some (compiler, computer)

let run (filepath : string) : (Compiler.t * Machine.t) option =
  filepath |> parse |> compile |> eval

let load (filepath : string) : Compiler.t * Machine.t =
  filepath |> parse |> compile

let continue content compiler_and_computer : (Compiler.t * Machine.t) option =
  match (parse content, compiler_and_computer) with
  | [], _ ->
      print_endline ("Parser error. Incorrect definition: " ^ content);
      None
  | decls_queries, Some (current_compiler, current_computer) ->
      decls_queries |> compile' (current_compiler, current_computer) |> eval
  | decls_queries, None -> decls_queries |> compile |> eval
