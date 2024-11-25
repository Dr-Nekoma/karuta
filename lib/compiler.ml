module RegisterMap = BatMap.Make (Ast)
module FT = BatFingerTree

type term_queue = Ast.t FT.t

open Machine

type t = { registers : int RegisterMap.t; terms : term_queue }

let initialize () : t = { registers = RegisterMap.empty; terms = FT.empty }

let rec compile : Ast.t list * t * Cell.t Store.t -> Cell.t Store.t = function
  | [], _, store -> store
  | [ d ], compiler, store -> (
      match d with
      | Query _ -> store (* TODO: error handling *)
      | Variable _ | Functor _ -> failwith "unreachable"
      | Declaration { head; body } ->
          let _, new_store = compile_declaration head body compiler store in
          new_store)
  | _, _, _ -> failwith "TODO"

and compile_loop : t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun ({ terms; _ } as compiler) store ->
  match FT.front terms with
  | None -> (compiler, store)
  | Some (rest, d) -> (
      let new_compiler = { compiler with terms = rest } in
      match d with
      | Query _ | Declaration _ -> failwith "unreachable"
      | Variable v -> compile_variable v new_compiler store
      | Functor f -> compile_functor f new_compiler store)

and compile_declaration :
    Ast.func -> Ast.func list -> t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun head body ({ terms; _ } as compiler) store ->
  match body with
  | [] ->
      compile_loop
        { compiler with terms = FT.cons terms (Ast.Functor head) }
        store
  | _ -> failwith "TODO"

and compile_functor : Ast.func -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun ({ elements; _ } as func) { terms; registers } store ->
    let new_registers = add (Ast.Functor func) (cardinal registers) registers in
    let new_terms = FT.append terms (FT.of_list elements) in
    compile_loop { registers = new_registers; terms = new_terms } store

and compile_variable : Ast.var -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun var ({ registers; _ } as compiler) store ->
    match find_opt (Ast.Variable var) registers with
    | None ->
        let new_registers =
          add (Ast.Variable var) (cardinal registers) registers
        in
        compile_loop { compiler with registers = new_registers } store
    | Some _ -> compile_loop compiler store
