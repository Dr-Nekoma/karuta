open Machine
open Machine.Cell

let put_structure (index_of_register : int) (functor_label, functor_arity)
    ({ store; registers; h_register; _ } as computer) : Machine.t =
  let structure = Machine.Cell.Structure (h_register + 1) in
  let func = Functor (functor_label, functor_arity) in
  let store =
    Store.heap_put func (h_register + 1)
    @@ Store.heap_put structure h_register store
  in
  let registers = IM.add index_of_register structure registers in
  let h_register = h_register + 2 in
  { computer with store; registers; h_register }

let set_variable (index_of_register : int)
    ({ store; registers; h_register; _ } as computer) =
  let reference = Reference h_register in
  let store = Store.heap_put reference h_register store in
  let registers = IM.add index_of_register reference registers in
  let h_register = h_register + 1 in
  { computer with store; registers; h_register }

let set_value (index_of_register : int)
    ({ store; registers; h_register; _ } as computer) =
  let value_of_register = IM.find index_of_register registers in
  let store = Store.heap_put value_of_register h_register store in
  let h_register = h_register + 1 in
  { computer with store; registers; h_register }

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
    (index_of_register : int) ({ store; h_register; _ } as computer) : Machine.t
    =
  let addr = deref index_of_register store in
  match Store.heap_get store addr with
  | Reference _ ->
      let structure = Structure (h_register + 1) in
      let func = Functor (functor_label, functor_arity) in
      let heap =
        bind addr h_register
        @@ Store.heap_put func (h_register + 1)
        @@ Store.heap_put structure h_register store
      in
      { computer with h_register = h_register + 2; mode = Write; store = heap }
  | Structure a -> (
      match Store.heap_get store a with
      | Functor (label, arity)
        when label == functor_label && arity == functor_arity ->
          { computer with s_register = a + 1; mode = Read }
      | _ -> { computer with fail = true })
  | _ -> { computer with fail = true }

let unify_variable (index_of_register : int)
    ({ store; registers; h_register; s_register; mode; _ } as computer) :
    Machine.t =
  match mode with
  | Read ->
      let value = Store.heap_get store s_register in
      let registers = IM.add index_of_register value registers in
      let s_register = s_register + 1 in
      { computer with registers; s_register }
  | Write ->
      let reference = Reference s_register in
      let store = Store.heap_put reference h_register store in
      let registers = IM.add index_of_register reference registers in
      let h_register = h_register + 1 in
      let s_register = s_register + 1 in
      { computer with store; registers; h_register; s_register }

let unify (a1 : address) (a2 : address) ({ store; _ } as computer) : Machine.t =
  let newComputer =
    (fun s -> { computer with fail = false; store = s })
    @@ Store.pdl_push (Address a2)
    @@ Store.pdl_push (Address a1) store
  in
  let aux ({ store; fail; _ } as computer) : Machine.t =
    let mutStore = ref store in
    let mutFail = ref fail in
    while not (Store.pdl_empty !mutStore || !mutFail) do
      let (Address p1) = Store.pdl_top !mutStore in
      mutStore := Store.pdl_pop !mutStore;
      let (Address p2) = Store.pdl_top !mutStore in
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

let unify_value (index_of_register : int)
    ({ store; registers; h_register; s_register; mode; _ } as computer) :
    Machine.t =
  match mode with
  | Read ->
      {
        (unify index_of_register s_register computer) with
        s_register = s_register + 1;
      }
  | Write ->
      let value_of_register = IM.find index_of_register registers in
      let store = Store.heap_put value_of_register h_register store in
      let h_register = h_register + 1 in
      let s_register = s_register + 1 in
      { computer with store; h_register; s_register }

let put_variable (index_of_x_register : int) (index_of_a_register : int)
    ({ store; registers; h_register; _ } as computer) : Machine.t =
  let reference = Reference h_register in
  let store = Store.heap_put reference h_register store in
  let registers =
    IM.add index_of_x_register reference
    @@ IM.add index_of_a_register reference registers
  in
  { computer with store; h_register = h_register + 1; registers }

let put_value (index_of_x_register : int) (index_of_a_register : int)
    ({ registers; _ } as computer) : Machine.t =
  let value = IM.find index_of_x_register registers in
  let registers = IM.add index_of_a_register value registers in
  { computer with registers }

let get_variable (index_of_x_register : int) (index_of_a_register : int)
    ({ registers; _ } as computer) : Machine.t =
  let value = IM.find index_of_a_register registers in
  let registers = IM.add index_of_x_register value registers in
  { computer with registers }

let get_value = unify
