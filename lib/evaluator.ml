module Map = Map.Make (String)

(* L0 defines the program as a single term *)
(* In the future, upgrade this to a database with a list of terms *)
let question (_program : Ast.t) (_query : Ast.t) : Ast.t Map.t list =
  failwith "Not Implemented"

module Heap = struct
  module FT = BatFingerTree

  type 'a t = 'a FT.t

  let push = FT.snoc
  let pop = FT.init
  let top = FT.last

  let put (elem : 'a) (index : int) (heap : 'a t) : 'a t =
    FT.set heap index elem

  let empty = FT.empty

  let rec initialize (heap : 'a t) (size : int) (default : 'a) =
    if size = 0 then heap else initialize (push heap default) (size - 1) default
end

module AbstractMachine = struct
  module Cell = struct
    type t =
      | Structure of int
      | Reference of int
      | Functor of string * int
      | Empty
  end

  open Cell
  module FT = BatFingerTree
  module IM = BatIMap

  type computer = {
    heap : Cell.t Heap.t;
    registers : Cell.t IM.t;
    h_register : int;
  }

  let put_structure (index_of_register : int) (functor_label, functor_arity)
      { heap; registers; h_register } =
    let structure = Structure (h_register + 1) in
    let func = Functor (functor_label, functor_arity) in
    let heap =
      Heap.put func (h_register + 1) @@ Heap.put structure h_register heap
    in
    let registers = IM.add index_of_register structure registers in
    let h_register = h_register + 2 in
    { heap; registers; h_register }

  let set_variable (index_of_register : int) { heap; registers; h_register } =
    let reference = Reference h_register in
    let heap = Heap.put reference h_register heap in
    let registers = IM.add index_of_register reference registers in
    let h_register = h_register + 1 in
    { heap; registers; h_register }

  let set_value (index_of_register : int) { heap; registers; h_register } =
    let value_of_register = IM.find index_of_register registers in
    let heap = Heap.put value_of_register h_register heap in
    let h_register = h_register + 1 in
    { heap; registers; h_register }

  let initialize (heap_size : int) : computer =
    {
      heap = Heap.initialize Heap.empty heap_size Empty;
      registers = IM.empty ~eq:( = );
      h_register = 0;
    }
end
