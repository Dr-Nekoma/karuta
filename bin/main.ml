module Option = struct
  let ( let+ ) = Option.bind
end

let show_registers (registers : int Lib.Compiler.RegisterMap.t) : string =
  let open Lib.Compiler.RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Lib.Ast.show term ^ " = " ^ string_of_int register)
    "" (to_seq registers)

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
      let compiler, _ =
        Lib.Compiler.compile
          (decls_queries, Lib.Compiler.initialize (), initialComputer.store)
      in
      print_endline @@ show_registers compiler.registers;
      None
