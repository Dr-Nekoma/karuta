module Cell = struct
  type register = X of int | Y of int

  and instruction =
    | GetStructure of ((string * int) * register)
    | PutStructure of ((string * int) * register)
    | PutVariable of (register * register)
    | GetVariable of (register * register)
    | SetVariable of register
    | SetValue of register
    | UnifyVariable of register
    | GetValue of (register * register)
    | PutValue of (register * register)
    | UnifyValue of register
    | Call of (Ast.tag * int)
    | Proceed
    | Allocate of int
    | Deallocate
    | Halt
  [@@deriving show]

  and t =
    | Structure of int
    | Reference of int
    | Functor of string * int
    | Address of int
    | Instruction of instruction
    | Empty
  [@@deriving show]
end

module Mode = struct
  type t = Read | Write
end

module IM = BatIMap

module Store = Store.Make (struct
  let code_size = 100
  let heap_size = 100
  let stack_size = 100
  let trail_pdl_size = 100
end)

module IntMap = Map.Make (Int)

type t = {
  store : Cell.t Store.t;
  x_registers : Cell.t IntMap.t;
  (* Cell.t IM.t; *)
  h_register : int;
  s_register : int;
  p_register : int;
  cp_register : int;
  e_register : int;
  mode : Mode.t;
  fail : bool;
}

let show_store (store : Cell.t Store.t) (how_many : int option) : string =
  let limit_list (l : 'a list) : 'a list =
    match how_many with
    | None -> l
    | Some x -> List.to_seq l |> Seq.take x |> List.of_seq
  in
  List.fold_left
    (fun acc elem -> acc ^ " " ^ Cell.show elem)
    ""
    (limit_list @@ Store.to_list store)

let show_x_registers (registers : Cell.t IntMap.t) : string =
  let open IntMap in
  fold
    (fun key value acc ->
      acc ^ "\n" ^ string_of_int key ^ " = " ^ Cell.show value)
    registers ""
[@@warning "-32"]

let initialize () : t =
  {
    store = Store.initialize Store.empty Store.mem_size Cell.Empty;
    x_registers = IntMap.empty;
    p_register = 0;
    cp_register = 0;
    e_register = Store.stack_start;
    h_register = Store.heap_start;
    s_register = Store.heap_start;
    mode = Mode.Read;
    fail = false;
  }
