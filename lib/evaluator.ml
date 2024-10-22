module Map = Map.Make (String)

(* L0 defines the program as a single term *)
(* In the future, upgrade this to a database with a list of terms *)
let question (_program : Ast.t) (_query : Ast.t) : Ast.t Map.t list =
  failwith "Not Implemented"

module AbstractMachine = struct
  module Cell = struct
    type t =
      | Structure of int
      | Reference of int
      | Functor of string * int
      | Empty
  end

  module Mode = struct
    type t = Read | Write
  end

  open Cell
  module IM = BatIMap

  module Store = Store.Make (struct
    let max_heap_size = 10
    let max_pdl_size = 10
  end)

  type computer = {
    store : Cell.t Store.t;
    heap_size : int;
    registers : Cell.t IM.t;
    h_register : int;
    s_register : int;
    mode : Mode.t;
    fail : bool;
  }

  let put_structure (index_of_register : int) (functor_label, functor_arity)
      ({ store; registers; h_register; _ } as computer) =
    let structure = Structure (h_register + 1) in
    let func = Functor (functor_label, functor_arity) in
    let store =
      Store.put func (h_register + 1) @@ Store.put structure h_register store
    in
    let registers = IM.add index_of_register structure registers in
    let h_register = h_register + 2 in
    { computer with store; registers; h_register }

  let set_variable (index_of_register : int)
      ({ store; registers; h_register; _ } as computer) =
    let reference = Reference h_register in
    let store = Store.put reference h_register store in
    let registers = IM.add index_of_register reference registers in
    let h_register = h_register + 1 in
    { computer with store; registers; h_register }

  let set_value (index_of_register : int)
      ({ store; registers; h_register; _ } as computer) =
    let value_of_register = IM.find index_of_register registers in
    let store = Store.put value_of_register h_register store in
    let h_register = h_register + 1 in
    { computer with store; registers; h_register }

  let rec deref (a : int) ({ store; _ } as computer) : int =
    let cell = Store.get store a in
    match cell with
    | Reference value when value <> a -> deref value computer
    | _ -> a

  let get_structure ((functor_label, functor_arity) : string * int)
      (index_of_register : int) ({ store; h_register; _ } as computer) :
      computer =
    let addr = deref index_of_register computer in
    match Store.get store addr with
    | Reference _ ->
        let reference = Reference h_register in
        let structure = Structure (h_register + 1) in
        let func = Functor (functor_label, functor_arity) in
        let heap =
          Store.put reference addr
          @@ Store.put func (h_register + 1)
          @@ Store.put structure h_register store
        in
        {
          computer with
          h_register = h_register + 2;
          mode = Write;
          store = heap;
        }
    | Structure a -> (
        match Store.get store a with
        | Functor (label, arity)
          when label == functor_label && arity == functor_arity ->
            { computer with s_register = a + 1; mode = Read }
        | _ -> { computer with fail = true })
    | _ -> { computer with fail = true }

  let unify_variable (index_of_register : int)
      ({ store; registers; h_register; s_register; mode; _ } as computer) :
      computer =
    match mode with
    | Read ->
        let value = Store.get store s_register in
        let registers = IM.add index_of_register value registers in
        let s_register = s_register + 1 in
        { computer with registers; s_register }
    | Write ->
        let reference = Reference s_register in
        let store = Store.put reference h_register store in
        let registers = IM.add index_of_register reference registers in
        let h_register = h_register + 1 in
        let s_register = s_register + 1 in
        { computer with store; registers; h_register; s_register }

  let unify (_a1 : int) _computer : computer =
    failwith "Unify is not yet implemented"

  let unify_value (index_of_register : int)
      ({ store; registers; h_register; s_register; mode; _ } as computer) :
      computer =
    match mode with
    | Read ->
        { (unify index_of_register computer) with s_register = s_register + 1 }
    | Write ->
        let value_of_register = IM.find index_of_register registers in
        let store = Store.put value_of_register h_register store in
        let h_register = h_register + 1 in
        let s_register = s_register + 1 in
        { computer with store; h_register; s_register }

  let initialize (store_size : int) (heap_size : int) : computer =
    {
      store = Store.initialize Store.empty store_size Empty;
      heap_size;
      registers = IM.empty ~eq:( = );
      h_register = 0;
      s_register = 0;
      mode = Mode.Read;
      fail = false;
    }
end
