module Option = struct
  let ( let+ ) = Option.bind
end

let show_registers (registers : int Lib.RegisterAllocator.RegisterMap.t) :
    string =
  let open Lib.RegisterAllocator.RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Lib.Ast.show term ^ " = " ^ string_of_int register)
    "" (to_seq registers)
[@@warning "-32"]

let main () =
  let open Option in
  let+ content =
    In_channel.with_open_text "examples/l2.krt" (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Lib.Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries -> (
      let initialComputer = Lib.Machine.initialize () in
      let compiler, store =
        Lib.Compiler.compile
          (decls_queries, Lib.Compiler.initialize (), initialComputer.store)
      in
      print_endline @@ Lib.Machine.show_store store (Some 30);
      let open Lib.Machine in
      let stack_start = initialComputer.e_register in
      let store =
        Store.code_put (Cell.Instruction Cell.Halt) compiler.p_register store
        |> Store.stack_put (Cell.Address stack_start) stack_start
        |> Store.stack_put (Cell.Address compiler.p_register) (stack_start + 1)
        |> Store.stack_put (Cell.Address 0) (stack_start + 2)
      in

      match compiler.entry_point with
      | None -> None
      | Some entry_point -> (
          match
            Lib.Compiler.FunctorMap.find_opt entry_point.functor_name
              compiler.functor_table
          with
          | Some _ ->
              let stacked_machine =
                {
                  initialComputer with
                  store;
                  p_register = entry_point.p_register;
                }
              in
              Some (Lib.Evaluator.eval compiler.functor_table stacked_machine)
          | None -> failwith "queried using undefined predicate"))

let _ = main ()
