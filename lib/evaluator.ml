module Map = Map.Make(String)
(* L0 defines the program as a single term *)
(* In the future, upgrade this to a database with a list of terms *)
let question (_program: Ast.t) (_query: Ast.t): Ast.t Map.t list =
  failwith "Not Implemented"

module Heap = struct
  type 'a t = 'a Seq.t
  let push (heap: 'a t) (data: 'a) =
    Seq.cons data heap
  let pop (heap: 'a t) =
    if Seq.is_empty heap
    then heap
    else Seq.drop 1 heap
  let top (heap: 'a t) =
    Option.map (function (value, _) -> value) @@ Seq.uncons heap
  let put (elem: 'a) (index: int) (heap: 'a t): 'a t =
    failwith "Not implemented"
end

module AbstractMachine = struct
  module Cell = struct
    type t =
      | Structure of int
      | Reference of int
      | Functor of string * int
  end

  open Cell
  
  type computer =
    { heap: Cell.t Heap.t;
      registers: Cell.t list;
      h_register: int }
  
  (* Creates a new structure on the heap *)
  let put_structure (index_of_register: int) (functor_label, functor_arity) {heap; registers; h_register} =
    let stru = Structure (h_register + 1) in
    let func = Functor (functor_label, functor_arity) in
    let heap = Heap.put func (h_register + 1) @@ Heap.put stru h_register heap in
    let registers = List.mapi (fun i elem -> if i = index_of_register then stru else elem) registers in
    let h_register = h_register + 2
    in { heap; registers; h_register }

  let set_variable (index_of_register: int) {heap; registers; h_register} =
    let reference = Reference h_register in
    let heap = Heap.put reference h_register heap in
    let registers = List.mapi (fun i elem -> if i = index_of_register then reference else elem) registers in
    let h_register = h_register + 1
    in { heap; registers; h_register }

  let set_value (index_of_register: int) {heap; registers; h_register} =
    let value_of_register = List.nth registers index_of_register in
    let heap = Heap.put value_of_register h_register heap in
    let h_register = h_register + 1
    in { heap; registers; h_register }
end
