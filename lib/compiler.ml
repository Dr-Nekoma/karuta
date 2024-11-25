module RegisterMap = BatMap.Make (Ast)
module FT = BatFingerTree

type term_queue = Ast.t FT.t

open Machine

type t = {
  register_counter : int;
  registers : int RegisterMap.t;
  terms : term_queue;
}

let initialize () : t =
  { register_counter = 1; registers = RegisterMap.empty; terms = FT.empty }

let compile : Ast.t list * t * Cell.t Store.t -> Cell.t Store.t =
  let open BatMap in
  function
  | [], compiler, store -> store
  | d :: ds, { register_counter; registers; terms }, store -> store
