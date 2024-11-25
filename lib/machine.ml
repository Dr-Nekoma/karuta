module Cell = struct
  type t =
    | Structure of int
    | Reference of int
    | Functor of string * int
    | Address of int
    | Empty
  [@@deriving show]
end

module Mode = struct
  type t = Read | Write
end

module IM = BatIMap

module Store = Store.Make (struct
  let heap_size = 100
  let stack_size = 100
  let trail_pdl_size = 100
end)

type t = {
  store : Cell.t Store.t;
  registers : Cell.t IM.t;
  h_register : int;
  s_register : int;
  mode : Mode.t;
  fail : bool;
}

let show_store (store : Cell.t Store.t) : string =
  List.fold_left
    (fun acc elem -> acc ^ " " ^ Cell.show elem)
    "" (Store.to_list store)

let initialize () : t =
  {
    store = Store.initialize Store.empty Store.mem_size Cell.Empty;
    registers = IM.empty ~eq:( = );
    h_register = 0;
    s_register = 0;
    mode = Mode.Read;
    fail = false;
  }
