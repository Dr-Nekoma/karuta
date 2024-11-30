module Cell = struct
  type instruction =
    | GetStructure of ((string * int) * int)
    | PutStructure of ((string * int) * int)
    | PutVariable of (int * int)
    | GetVariable of (int * int)
    | SetVariable of int
    | SetValue of int
    | UnifyVariable of int
    | GetValue of (int * int)
    | PutValue of (int * int)
    | UnifyValue of int
    | Call of int
    | Proceed

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

type t = {
  store : Cell.t Store.t;
  registers : Cell.t IM.t;
  h_register : int;
  s_register : int;
  p_register : int;
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

let initialize () : t =
  {
    store = Store.initialize Store.empty Store.mem_size Cell.Empty;
    registers = IM.empty ~eq:( = );
    p_register = 0;
    h_register = Store.heap_start;
    s_register = Store.stack_start;
    mode = Mode.Read;
    fail = false;
  }
