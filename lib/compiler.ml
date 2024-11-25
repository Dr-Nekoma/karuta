module RegisterMap = BatMap.Make (Ast)
module FT = BatFingerTree

type term_queue = Ast.t FT.t

open Machine

type t = { registers : int RegisterMap.t; terms : term_queue }

let initialize () : t = { registers = RegisterMap.empty; terms = FT.empty }

let rec compile : Ast.t list * t * Cell.t Store.t -> t * Cell.t Store.t =
  function
  | [], compiler, store -> (compiler, store)
  | [ d ], compiler, store -> (
      match d with
      | Query f -> register_alloc_functor f compiler store
      | Variable _ | Functor _ -> failwith "unreachable"
      | Declaration { head; body } ->
          register_alloc_declaration head body compiler store)
  | _, _, _ -> failwith "TODO"

and register_alloc_loop : t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun ({ terms; _ } as compiler) store ->
  match FT.front terms with
  | None -> (compiler, store)
  | Some (rest, d) -> (
      let new_compiler = { compiler with terms = rest } in
      match d with
      | Declaration _ -> failwith "unreachable"
      | Variable v -> register_alloc_variable v new_compiler store
      | Query f | Functor f -> register_alloc_functor f new_compiler store)

and register_alloc_declaration :
    Ast.func -> Ast.func list -> t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun head body ({ terms; _ } as compiler) store ->
  match body with
  | [] ->
      register_alloc_loop
        { compiler with terms = FT.cons terms (Ast.Functor head) }
        store
  | _ -> failwith "TODO"

and register_alloc_functor :
    Ast.func -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun ({ elements; _ } as func) { terms; registers } store ->
    let new_registers = add (Ast.Functor func) (cardinal registers) registers in
    let new_terms = FT.append terms (FT.of_list elements) in
    register_alloc_loop { registers = new_registers; terms = new_terms } store

and register_alloc_variable :
    Ast.var -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun var ({ registers; _ } as compiler) store ->
    match find_opt (Ast.Variable var) registers with
    | None ->
        let new_registers =
          add (Ast.Variable var) (cardinal registers) registers
        in
        register_alloc_loop { compiler with registers = new_registers } store
    | Some _ -> register_alloc_loop compiler store
