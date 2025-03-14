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

let halt_program
    ((compiler, ({ e_register; store; _ } as computer)) :
      Lib.Compiler.t * Lib.Machine.t) : Lib.Compiler.t * Lib.Machine.t =
  let open Lib.Machine in
  let stack_start = e_register in
  let store =
    Store.code_put (Cell.Instruction Cell.Halt) compiler.p_register store
    |> Store.stack_put (Cell.Address stack_start) stack_start
    |> Store.stack_put (Cell.Address compiler.p_register) (stack_start + 1)
    |> Store.stack_put (Cell.Address 0) (stack_start + 2)
  in
  (compiler, { computer with store })

let bimap f g (a1, a2) = (f a1, g a2)

let update_store (computer : Lib.Machine.t)
    (store : Lib.Machine.Cell.t Lib.Machine.Store.t) : Lib.Machine.t =
  { computer with store }

let _ =
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
      let compiler, computer =
        Lib.Machine.initialize ()
        |> (fun initialComputer ->
             Lib.Compiler.compile
               (decls_queries, Lib.Compiler.initialize (), initialComputer.store)
             |> bimap Fun.id (update_store initialComputer))
        |> halt_program
      in
      (* print_endline @@ Lib.Machine.show_store store (Some 30); *)
      match compiler.entry_point with
      | None -> None
      | Some entry_point ->
          Some
            (Lib.Evaluator.eval compiler.functor_table
               { computer with p_register = entry_point.p_register }))
