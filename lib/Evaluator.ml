open Machine
open Machine.Cell

let set_register (register : Cell.register) (cell : Cell.t)
    ({ store; x_registers; e_register; _ } as computer) : Machine.t =
  match register with
  | Cell.X index_of_register ->
      let open Machine.IntMap in
      let x_registers = add index_of_register cell x_registers in
      (* print_endline "setting register"; *)
      (* print_endline @@ Machine.show_x_registers x_registers; *)
      { computer with x_registers }
  | Cell.Y index_of_register -> (
      let stack_frame_size = Store.stack_get store (e_register + 2) in
      match stack_frame_size with
      | Address size ->
          if index_of_register < size then
            let store =
              Store.stack_put cell (e_register + 3 + index_of_register) store
            in
            { computer with store }
          else failwith "stack overflow"
      | _ -> failwith "invalid stack size (Not Address)")

let get_register (register : Cell.register)
    { store; x_registers; e_register; _ } : Cell.t =
  (* print_endline @@ Machine.show_x_registers x_registers; *)
  match register with
  | Cell.X index_of_register ->
      let open Machine.IntMap in
      find index_of_register x_registers
  | Cell.Y index_of_register -> (
      let stack_frame_size = Store.stack_get store (e_register + 2) in
      match stack_frame_size with
      | Address size ->
          if index_of_register < size then
            Store.stack_get store (e_register + 3 + index_of_register)
          else failwith "stack overflow"
      | _ -> failwith "invalid stack size (Not Address)")

let put_structure (register : Cell.register) (functor_label, functor_arity)
    ({ store; h_register; _ } as computer) : Machine.t =
  let structure = Machine.Cell.Structure (h_register + 1) in
  let func = Functor (functor_label, functor_arity) in
  store
  |> Store.heap_put structure h_register
  |> Store.heap_put func (h_register + 1)
  |> (fun store -> { computer with store; h_register = h_register + 2 })
  |> set_register register structure

let set_variable (register : Cell.register)
    ({ store; h_register; _ } as computer) =
  let reference = Reference h_register in
  store
  |> Store.heap_put reference h_register
  |> (fun store -> { computer with store; h_register = h_register + 1 })
  |> set_register register reference

let set_value (register : Cell.register) ({ store; h_register; _ } as computer)
    =
  let value_of_register = get_register register computer in
  store |> Store.heap_put value_of_register h_register |> fun store ->
  { computer with store; h_register = h_register + 1 }

let rec deref (a : int) store : int =
  let cell = Store.heap_get store a in
  match cell with
  | Reference value when value <> a -> deref value store
  | _ -> a

type address = int

let bind (i1 : address) (i2 : address) (mem : Machine.Cell.t Store.t) :
    Machine.Cell.t Store.t =
  Store.heap_put (Reference i2) i1 mem

let get_structure ((functor_label, functor_arity) : string * int)
    (register : Cell.register) ({ store; h_register; _ } as computer) :
    Machine.t =
  match get_register register computer with
  | Address address -> (
      let addr = deref address store in
      match Store.heap_get store addr with
      | Reference _ ->
          let structure = Structure (h_register + 1) in
          let func = Functor (functor_label, functor_arity) in
          let heap =
            store
            |> Store.heap_put structure h_register
            |> Store.heap_put func (h_register + 1)
            |> bind addr h_register
          in
          {
            computer with
            h_register = h_register + 2;
            mode = Write;
            store = heap;
          }
      | Structure a -> (
          match Store.heap_get store a with
          | Functor (label, arity)
            when label == functor_label && arity == functor_arity ->
              { computer with s_register = a + 1; mode = Read }
          | _ -> { computer with fail = true })
      | _ -> { computer with fail = true })
  | _ -> failwith "unreachable get_structure"

let unify_variable (register : Cell.register)
    ({ store; h_register; s_register; mode; _ } as computer) : Machine.t =
  match mode with
  | Read ->
      let value = Store.heap_get store s_register in
      set_register register value { computer with s_register = s_register + 1 }
  | Write ->
      let reference = Reference h_register in
      store
      |> Store.heap_put reference h_register
      |> (fun store ->
           {
             computer with
             store;
             h_register = h_register + 1;
             s_register = s_register + 1;
           })
      |> set_register register reference

let unify (a1 : address) (a2 : address) ({ store; _ } as computer) : Machine.t =
  let newComputer =
    store |> Store.pdl_push (Address a1) |> Store.pdl_push (Address a2)
    |> fun store -> { computer with store; fail = false }
  in
  let aux ({ store; fail; _ } as computer) : Machine.t =
    let mutStore = ref store in
    let mutFail = ref fail in
    while not (Store.pdl_empty !mutStore || !mutFail) do
      let generic_p1 = Store.pdl_top !mutStore in
      let (Reference p1) = generic_p1 in
      mutStore := Store.pdl_pop !mutStore;
      let (Reference p2) = Store.pdl_top !mutStore in
      mutStore := Store.pdl_pop !mutStore;
      let d1 = deref p1 !mutStore in
      let d2 = deref p2 !mutStore in
      if d1 != d2 then
        match (Store.get !mutStore d1, Store.get !mutStore d2) with
        | Reference _, _ | _, Reference _ -> mutStore := bind d1 d2 !mutStore
        | Structure v1, Structure v2 -> (
            match (Store.get !mutStore v1, Store.get !mutStore v2) with
            | Functor (s1, n1), Functor (s2, n2) ->
                if s1 == s2 && n1 == n2 then
                  for i = 1 to n1 do
                    mutStore := Store.pdl_push (Address (v1 + i)) !mutStore;
                    mutStore := Store.pdl_push (Address (v2 + i)) !mutStore
                  done
                else mutFail := true
            | _, _ -> failwith "Unreachable")
      else ()
    done;
    { computer with store = !mutStore; fail = !mutFail }
  in
  aux newComputer

let unify_value (register : Cell.register)
    ({ store; h_register; s_register; mode; _ } as computer) : Machine.t =
  match mode with
  | Read -> (
      match get_register register computer with
      | Address addr ->
          { (unify addr s_register computer) with s_register = s_register + 1 }
      | _ -> failwith "unreachable unify_value")
  | Write ->
      let value_of_register = get_register register computer in
      store |> Store.heap_put value_of_register h_register |> fun store ->
      {
        computer with
        store;
        h_register = h_register + 1;
        s_register = s_register + 1;
      }

let put_variable (x_register : Cell.register) (a_register : Cell.register)
    ({ store; h_register; _ } as computer) : Machine.t =
  let reference = Reference h_register in
  store
  |> Store.heap_put reference h_register
  |> (fun store -> { computer with store; h_register = h_register + 1 })
  |> set_register a_register reference
  |> set_register x_register reference

let put_value (x_register : Cell.register) (a_register : Cell.register) computer
    : Machine.t =
  let value = get_register x_register computer in
  set_register a_register value computer

let get_variable (x_register : Cell.register) (a_register : Cell.register)
    computer : Machine.t =
  let value = get_register a_register computer in
  set_register x_register value computer

let get_value = unify

let deallocate ({ e_register; store; _ } as computer) : Machine.t =
  let p_register = Store.stack_get store (e_register + 1) in
  let e_register = Store.stack_get store e_register in
  match (p_register, e_register) with
  | Cell.Address p_register, Cell.Address e_register ->
      { computer with p_register; e_register }
  | _ -> failwith "unreachable"

let instruction_size = 1

let allocate (n : int)
    ({ store; e_register; cp_register; p_register; _ } as computer) : Machine.t
    =
  let new_e =
    match Store.stack_get store (e_register + 2) with
    | Cell.Address n -> n + e_register + 3
    | _ -> failwith "unreachable allocate"
  in
  store
  |> Store.stack_put (Cell.Address e_register) new_e
  |> Store.stack_put (Cell.Address cp_register) (new_e + 1)
  |> Store.stack_put (Cell.Address n) (new_e + 2)
  |> fun store ->
  {
    computer with
    store;
    p_register = p_register + instruction_size;
    e_register = new_e;
  }

let call (functor' : Ast.tag * int) (functor_table : Compiler.functor_map)
    ({ p_register; _ } as computer) : Machine.t =
  let open Compiler.FunctorMap in
  {
    computer with
    cp_register = p_register + instruction_size;
    p_register = find functor' functor_table;
  }

let proceed ({ cp_register; _ } as computer) : Machine.t =
  { computer with p_register = cp_register }

let eval_step (functor_table : Compiler.functor_map)
    ({ store; p_register; _ } as computer : Machine.t) : Machine.t * bool =
  let open Machine.Cell in
  (* print_endline @@ string_of_int p_register; *)
  match Store.code_get store p_register with
  | Instruction instruction -> (
      (* print_endline @@ Machine.Cell.show_instruction instruction; *)
      match instruction with
      | GetStructure ((name, arity), register) ->
          ( {
              (get_structure (name, arity) register computer) with
              p_register = p_register + 1;
            },
            false )
      | PutStructure ((name, arity), register) ->
          ( {
              (put_structure register (name, arity) computer) with
              p_register = p_register + 1;
            },
            false )
      | PutVariable (x_register, a_register) ->
          ( {
              (put_variable x_register a_register computer) with
              p_register = p_register + 1;
            },
            false )
      | GetVariable (x_register, a_register) ->
          ( {
              (get_variable x_register a_register computer) with
              p_register = p_register + 1;
            },
            false )
      | SetVariable register ->
          ( { (set_variable register computer) with p_register = p_register + 1 },
            false )
      | SetValue register ->
          ( { (set_value register computer) with p_register = p_register + 1 },
            false )
      | UnifyVariable register ->
          ( {
              (unify_variable register computer) with
              p_register = p_register + 1;
            },
            false )
      | PutValue (x_register, a_register) ->
          ( {
              (put_value x_register a_register computer) with
              p_register = p_register + 1;
            },
            false )
      | GetValue (x_register, a_register) -> (
          match
            (get_register x_register computer, get_register a_register computer)
          with
          | Reference x_addr, Reference a_addr ->
              ( {
                  (get_value x_addr a_addr computer) with
                  p_register = p_register + 1;
                },
                false )
          | _ -> failwith "unreachable GetValue")
      | UnifyValue register ->
          ( { (unify_value register computer) with p_register = p_register + 1 },
            false )
      | Allocate n -> (allocate n computer, false)
      | Deallocate ->
          ({ (deallocate computer) with p_register = p_register + 1 }, false)
      | Call predicate -> (call predicate functor_table computer, false)
      | Proceed -> (proceed computer, false)
      | Halt -> (computer, true))
  | _ -> failwith "unreachable eval_step"

let rec eval (functor_table : Compiler.functor_map) (computer : Machine.t) :
    Machine.t =
  let computer, stop = eval_step functor_table computer in
  if stop then computer else eval functor_table computer
