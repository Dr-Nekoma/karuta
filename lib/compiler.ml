module RegisterMap = BatMap.Make (Ast)
module FT = BatFingerTree
module S = BatSet

type term_queue = Ast.t FT.t
type variable_set = Ast.tag S.t

open Machine

type t = {
  p_register : int;
  registers : int RegisterMap.t;
  terms : term_queue;
  variables : variable_set;
}

let initialize () : t =
  {
    p_register = 0;
    registers = RegisterMap.empty;
    terms = FT.empty;
    variables = S.empty;
  }

let rec compile : Ast.t list * t * Cell.t Store.t -> t * Cell.t Store.t =
  function
  | [], compiler, store -> (compiler, store)
  | d :: ds, compiler, store -> (
      match d with
      | Query f as query ->
          let compiler, store = register_alloc_functor f compiler store in
          let compiler, store = generate_code (compiler, store) query in
          compile (ds, compiler, store)
      | Variable _ | Functor _ -> failwith "unreachable"
      | Declaration { head; body } as declaration ->
          let compiler, store =
            register_alloc_declaration head body compiler store
          in
          let compiler, store = generate_code (compiler, store) declaration in
          compile (ds, compiler, store))

and generate_code ((({ registers; _ } as compiler), store) : t * Cell.t Store.t)
    (value : Ast.t) : t * Cell.t Store.t =
  let add_instruction
      ((({ p_register; _ } as compiler), store) : t * Cell.t Store.t)
      (instruction : Cell.instruction) : t * Cell.t Store.t =
    let store =
      Store.code_put (Cell.Instruction instruction) p_register store
    in
    ({ compiler with p_register = p_register + 1 }, store)
  in
  let emit_argument
      ((variable, value, catchall) :
        (int -> Cell.instruction)
        * (int -> Cell.instruction)
        * (int -> Cell.instruction))
      ((({ registers; variables; _ } as compiler), store) : t * Cell.t Store.t)
      (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let index_of_register = find elem registers in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None -> (S.add namev variables, variable index_of_register)
          | Some _ -> (variables, value index_of_register)
        in
        add_instruction ({ compiler with variables }, store) instruction
    | _ ->
        let instruction = catchall index_of_register in
        add_instruction (compiler, store) instruction
  in
  let emit_functor_argument =
    emit_argument
      ( (fun v -> Cell.UnifyVariable v),
        (fun v -> Cell.UnifyValue v),
        fun v -> Cell.UnifyVariable v )
  in
  let emit_toplevel_query_argument =
    emit_argument
      ( (fun v -> Cell.SetVariable v),
        (fun v -> Cell.SetValue v),
        fun v -> Cell.SetValue v )
  in
  let rec emit_query_argument
      ((({ registers; _ } as compiler), store) : t * Cell.t Store.t)
      (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let index_of_register = find elem registers in
    match elem with
    | Functor { namef; elements; arity } ->
        let instruction =
          Cell.PutStructure ((namef, arity), index_of_register)
        in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_query_argument (compiler, store) elements
    | _ -> emit_toplevel_query_argument (compiler, store) elem
  in
  let non_variable : Ast.t -> bool = function
    | Variable _ -> false
    | _ -> true
  in
  let open RegisterMap in
  match value with
  | Query ({ namef; elements; arity } as func) ->
      let compiler, store =
        List.fold_left emit_query_argument (compiler, store)
          (List.filter non_variable elements)
      in
      let index_of_register = find (Ast.Functor func) registers in
      let instruction = Cell.PutStructure ((namef, arity), index_of_register) in
      let compiler, store = add_instruction (compiler, store) instruction in
      let compiler, store =
        List.fold_left emit_toplevel_query_argument (compiler, store) elements
      in
      ({ compiler with variables = S.empty }, store)
  | Functor { namef; elements; arity } ->
      let index_register = find value registers in
      let instruction = Cell.GetStructure ((namef, arity), index_register) in
      let compiler, store = add_instruction (compiler, store) instruction in
      let compiler, store =
        List.fold_left emit_functor_argument (compiler, store) elements
      in
      let compiler, store =
        List.fold_left generate_code (compiler, store) elements
      in
      ({ compiler with variables = S.empty }, store)
  | Variable _ -> (compiler, store)
  | Declaration { head; _ } ->
      generate_code (compiler, store) (Ast.Functor head)

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
  fun ({ elements; _ } as func) ({ terms; registers; _ } as compiler) store ->
    let registers = add (Ast.Functor func) (cardinal registers) registers in
    let terms = FT.append terms (FT.of_list elements) in
    register_alloc_loop { compiler with registers; terms } store

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
