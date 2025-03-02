module VariableMap = BatMap.Make (String)

type functor_name = string * int [@@deriving ord]
type var_frequency_map = int VariableMap.t
type entry_point = { p_register : int; functor_name : functor_name }

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

type functor_map = int FunctorMap.t

module FT = BatFingerTree
module S = BatSet

type register_set = RegisterAllocator.register S.t

open Machine

type t = {
  p_register : int;
  terms : RegisterAllocator.term_queue;
  variables : RegisterAllocator.variable_set;
  scope_registers : register_set;
  functor_table : functor_map;
  entry_point : entry_point option;
}

let initialize () : t =
  {
    p_register = 0;
    terms = FT.empty;
    variables = S.empty;
    scope_registers = S.empty;
    functor_table = FunctorMap.empty;
    entry_point = None;
  }

let show_registers
    (registers : RegisterAllocator.register RegisterAllocator.RegisterMap.t) :
    string =
  let open RegisterAllocator.RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Ast.show term ^ " = "
      ^ RegisterAllocator.show_register register)
    "" (to_seq registers)
[@@warning "-32"]

let rec compile : Ast.t list * t * Cell.t Store.t -> t * Cell.t Store.t =
  function
  | [], compiler, store -> (compiler, store)
  | d :: ds, compiler, store -> (
      match d with
      | Variable _ | Functor _ -> failwith "unreachable compile"
      | (Declaration _ | Query _) as form ->
          let compiler, allocator = allocate_registers compiler form in
          let compiler, _, store =
            generate_code (compiler, allocator, store) form
          in
          compile (ds, compiler, store))

and allocate_registers ({ entry_point; p_register; _ } as compiler)
    (elem : Ast.t) : t * RegisterAllocator.t =
  match elem with
  | Variable _ | Functor _ -> failwith "not top level forms"
  | (Declaration { head = f; _ } | Query f) as form -> (
      let allocator = RegisterAllocator.allocate_toplevel form in
      match entry_point with
      | None ->
          let functor_name = (f.namef, f.arity) in
          let entry_point = Some { p_register; functor_name } in
          ({ compiler with entry_point }, allocator)
      | Some _ -> failwith "multiple queries are not supported yet")

and cell_register : RegisterAllocator.register -> Cell.register = function
  | Temporary value -> Cell.X value
  | Permanent value -> Cell.Y value

and generate_code
    (( ({ p_register; functor_table; _ } as compiler),
       ({ registers; _ } as allocator),
       store ) :
      t * RegisterAllocator.t * Cell.t Store.t) (value : Ast.t) :
    t * RegisterAllocator.t * Cell.t Store.t =
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
        (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction))
      ((({ variables; _ } as compiler), ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None -> (S.add namev variables, variable register)
          | Some _ -> (variables, value register)
        in
        let compiler, store =
          add_instruction ({ compiler with variables }, store) instruction
        in
        (compiler, allocator, store)
    | _ ->
        let instruction = catchall register in
        let compiler, store = add_instruction (compiler, store) instruction in
        (compiler, allocator, store)
  in
  let emit_functor_argument =
    emit_argument
      ( (fun v -> Cell.UnifyVariable v),
        (fun v -> Cell.UnifyValue v),
        fun v -> Cell.UnifyVariable v )
  in
  let emit_nested_fact_argument
      (( ({ terms; variables; _ } as compiler),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    let instruction = Cell.UnifyVariable register in
    let compiler, store = add_instruction (compiler, store) instruction in
    match elem with
    | Variable { namev } ->
        ({ compiler with variables = S.add namev variables }, allocator, store)
    | Functor _ as f ->
        ({ compiler with terms = FT.cons terms f }, allocator, store)
    | _ -> failwith "unreachable emit_nested_fact_argument"
  in
  let emit_queue_nested_fact_argument
      ((compiler, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable _ ->
        let instruction = Cell.UnifyVariable register in
        let compiler, store = add_instruction (compiler, store) instruction in
        (compiler, allocator, store)
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_nested_fact_argument
          (compiler, allocator, store)
          elements
    | _ -> failwith "unreachable emit_queue_nested_fact_argument"
  in
  let rec emit_queue_fact_arguments
      ((({ terms; _ } as compiler), allocator, store) :
        t * RegisterAllocator.t * Cell.t Store.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    match FT.front terms with
    | None -> (compiler, allocator, store)
    | Some (rest, d) ->
        let compiler = { compiler with terms = rest } in
        let result =
          emit_queue_nested_fact_argument (compiler, allocator, store) d
        in
        emit_queue_fact_arguments result
  in
  let emit_fact_argument
      ((({ variables; _ } as compiler), ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (index : int) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    let arg_register = Cell.X index in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None -> (S.add namev variables, Cell.UnifyVariable arg_register)
          | Some _ -> (variables, Cell.GetValue (register, arg_register))
        in
        let compiler, store =
          add_instruction ({ compiler with variables }, store) instruction
        in
        (compiler, allocator, store)
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), arg_register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_nested_fact_argument
          (compiler, allocator, store)
          elements
    | _ -> failwith "unreachable emit_fact_argument"
  in
  let emit_toplevel_query_argument =
    emit_argument
      ( (fun v -> Cell.SetVariable v),
        (fun v -> Cell.SetValue v),
        fun v -> Cell.SetValue v )
  in
  let rec emit_query_argument
      ((compiler, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Functor { namef; elements; arity } ->
        let instruction = Cell.PutStructure ((namef, arity), register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_query_argument (compiler, allocator, store) elements
    | _ -> emit_toplevel_query_argument (compiler, allocator, store) elem
  in
  let head_folder
      (( ( ({ scope_registers; _ } as compiler),
           ({ registers; _ } as allocator),
           store ),
         counter ) :
        (t * RegisterAllocator.t * Cell.t Store.t) * int) element :
      (t * RegisterAllocator.t * Cell.t Store.t) * int =
    let open RegisterAllocator.RegisterMap in
    let raw_register = find element registers in
    let register = cell_register raw_register in
    let instruction = Cell.GetVariable (register, Cell.X counter) in
    let scope_registers = S.add raw_register scope_registers in
    let compiler, store =
      add_instruction ({ compiler with scope_registers }, store) instruction
    in
    ((compiler, allocator, store), counter + 1)
  in
  let allocate_head ({ elements; _ } : Ast.func)
      ((compiler, ({ y_register; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) =
    let instruction = Cell.Allocate y_register in
    let compiler, store = add_instruction (compiler, store) instruction in
    List.fold_left head_folder ((compiler, allocator, store), 0) elements |> fst
  in
  let allocate_body (elements : Ast.func list) (compiler, allocator, store) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let allocate_clause (compiler, allocator, store)
        ({ namef; elements; arity } : Ast.func) :
        t * RegisterAllocator.t * Cell.t Store.t =
      let allocate_argument
          (( ( ({ scope_registers; _ } as compiler),
               ({ registers; _ } as allocator),
               store ),
             counter ) :
            (t * RegisterAllocator.t * Cell.t Store.t) * int)
          (individual_element : Ast.t) :
          (t * RegisterAllocator.t * Cell.t Store.t) * int =
        match individual_element with
        | Variable _ as var ->
            let open RegisterAllocator.RegisterMap in
            let raw_register = find var registers in
            let left_register = cell_register raw_register in
            let scope_registers, instruction =
              match S.find_opt raw_register scope_registers with
              | None ->
                  ( S.add raw_register scope_registers,
                    Cell.PutVariable (left_register, Cell.X counter) )
              | Some _ ->
                  ( scope_registers,
                    Cell.PutValue (left_register, Cell.X counter) )
            in
            let compiler, store =
              add_instruction
                ({ compiler with scope_registers }, store)
                instruction
            in
            ((compiler, allocator, store), counter + 1)
        | Functor func as f ->
            let open RegisterAllocator.RegisterMap in
            let (compiler, allocator, store), _ =
              ( generate_code (compiler, allocator, store) (Ast.Query func),
                counter )
            in
            let raw_register = find f registers in
            let left_register = cell_register raw_register in
            let instruction = Cell.PutValue (left_register, Cell.X counter) in
            let compiler, store =
              add_instruction (compiler, store) instruction
            in
            ((compiler, allocator, store), counter + 1)
        | _ -> failwith "unreachable allocate_body"
      in
      let instruction = Cell.Call (namef, arity) in
      let (compiler, allocator, store), _ =
        List.fold_left allocate_argument
          ((compiler, allocator, store), 0)
          elements
      in
      let compiler, store = add_instruction (compiler, store) instruction in
      (compiler, allocator, store)
    in
    let compiler, allocator, store =
      List.fold_left allocate_clause (compiler, allocator, store) elements
    in
    let compiler, store = add_instruction (compiler, store) Cell.Deallocate in
    (compiler, allocator, store)
  in
  let open RegisterAllocator.RegisterMap in
  match value with
  | Query { namef; elements; arity } ->
      let compiler, allocator, store =
        List.fold_left emit_query_argument (compiler, allocator, store) elements
      in
      let instruction = Cell.Call (namef, arity) in
      let compiler, store = add_instruction (compiler, store) instruction in
      ({ compiler with variables = S.empty }, allocator, store)
  | Functor { namef; elements; arity } ->
      let register = cell_register @@ find value registers in
      let instruction = Cell.GetStructure ((namef, arity), register) in
      let compiler, store = add_instruction (compiler, store) instruction in
      let compiler, allocator, store =
        List.fold_left emit_functor_argument
          (compiler, allocator, store)
          elements
      in
      let compiler, allocator, store =
        List.fold_left generate_code (compiler, allocator, store) elements
      in
      ({ compiler with variables = S.empty }, allocator, store)
  | Variable _ -> (compiler, allocator, store)
  | Declaration { head = { elements; namef; arity }; body = [] } ->
      let open FunctorMap in
      let functor_table = add (namef, arity) p_register functor_table in
      let compiler, allocator, store =
        Seq.fold_lefti emit_fact_argument
          ({ compiler with functor_table }, allocator, store)
          (List.to_seq elements)
      in
      let compiler, allocator, store =
        emit_queue_fact_arguments (compiler, allocator, store)
      in
      let compiler, store = add_instruction (compiler, store) Cell.Proceed in
      (compiler, allocator, store)
  | Declaration { head; body } ->
      let open FunctorMap in
      let functor_table =
        add (head.namef, head.arity) p_register functor_table
      in
      ({ compiler with functor_table }, allocator, store)
      |> allocate_head head |> allocate_body body
