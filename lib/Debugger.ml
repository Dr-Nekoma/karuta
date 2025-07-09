type action =
  | ShowInternalRegisters
  | ShowXRegisters
  | ShowHeap
  | ShowStack
  | ShowCode
  | ShowFunctorTable
  | DisableDebug
  | ToggleTrace
  | Halt
  | Help
  | Step
[@@deriving enumerate]

type description = string
type shortcut = string

let action_description : action -> description = function
  | ShowInternalRegisters -> "Display internal registers"
  | ShowXRegisters -> "Display temporary (X) registers"
  | ShowHeap -> "Display heap memory region"
  | ShowStack -> "Display stack memory region"
  | ShowCode -> "Display code memory region"
  | ShowFunctorTable -> "Display functor table"
  | DisableDebug -> "Disable debug mode"
  | ToggleTrace -> "Toggle trace"
  | Halt -> "Halt execution"
  | Help -> "Display list of all debug actions"
  | Step -> "Proceed to next instruction"

let string_action : shortcut -> action option = function
  | "r" -> Some ShowInternalRegisters
  | "x" -> Some ShowXRegisters
  | "m" -> Some ShowHeap
  | "s" -> Some ShowStack
  | "c" -> Some ShowCode
  | "f" -> Some ShowFunctorTable
  | "q" -> Some DisableDebug
  | "t" -> Some ToggleTrace
  | "halt" -> Some Halt
  | "h" -> Some Help
  | "" -> Some Step
  | _ -> None

let rec run (functor_table : Compiler.functor_map)
    (stepper : Compiler.functor_map -> Machine.t -> Machine.t * bool)
    (eval : Compiler.functor_map -> Machine.t -> Machine.t)
    (computer : Machine.t) : Machine.t =
  if computer.debug then (
    let action = read_line () in
    match string_action action with
    | Some ShowInternalRegisters ->
        print_endline @@ Machine.show_internal_registers computer;
        run functor_table stepper eval computer
    | Some ShowXRegisters ->
        print_endline @@ Machine.show_x_registers computer.x_registers;
        run functor_table stepper eval computer
    | Some ShowHeap ->
        print_endline
        @@ Machine.show_store computer.store Machine.Store.heap_start
             (Machine.Store.heap_start + Machine.Store.Layout.heap_size);
        run functor_table stepper eval computer
    | Some ShowStack ->
        print_endline
        @@ Machine.show_store computer.store Machine.Store.stack_start
             (Machine.Store.stack_start + Machine.Store.Layout.stack_size);
        run functor_table stepper eval computer
    | Some ShowCode ->
        print_endline
        @@ Machine.show_store computer.store 0 Machine.Store.Layout.code_size;
        run functor_table stepper eval computer
    | Some ShowFunctorTable ->
        print_string @@ Compiler.show_functor_table functor_table;
        run functor_table stepper eval computer
    | Some DisableDebug ->
        run functor_table stepper eval { computer with debug = false }
    | Some ToggleTrace ->
        print_string "Trace ";
        print_endline @@ if computer.trace then "off" else "on";
        run functor_table stepper eval
          { computer with trace = not computer.trace }
    | Some Halt -> computer
    | Some Help -> failwith "TODO: Add help for newcomers"
    | Some Step ->
        let computer, stop = stepper functor_table computer in
        if stop then computer else run functor_table stepper eval computer
    | None ->
        print_endline "Unknown command";
        run functor_table stepper eval computer)
  else eval functor_table computer
