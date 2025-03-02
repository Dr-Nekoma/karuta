type functor_name = string * int [@@deriving ord]

module VariableMap = BatMap.Make (String)

type var_frequency_map = int VariableMap.t

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
}

let initialize () : t =
  {
    p_register = 0;
    terms = FT.empty;
    variables = S.empty;
    scope_registers = S.empty;
    functor_table = FunctorMap.empty;
  }

let cell_register : RegisterAllocator.register -> Cell.register = function
  | Temporary value -> Cell.X value
  | Permanent value -> Cell.Y value

and add_instruction
    ((({ p_register; _ } as generator), store) : t * Cell.t Store.t)
    (instruction : Cell.instruction) : t * Cell.t Store.t =
  let store = Store.code_put (Cell.Instruction instruction) p_register store in
  ({ generator with p_register = p_register + 1 }, store)

module type Fact = sig
  val emit_argument :
    t * RegisterAllocator.t * Cell.t Store.t ->
    int ->
    Ast.t ->
    t * RegisterAllocator.t * Cell.t Store.t

  val emit_queue_arguments :
    t * RegisterAllocator.t * Cell.t Store.t ->
    t * RegisterAllocator.t * Cell.t Store.t
end

module Fact = struct
  let rec emit_nested_argument
      (( ({ terms; variables; _ } as generator),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    let instruction = Cell.UnifyVariable register in
    let generator, store = add_instruction (generator, store) instruction in
    match elem with
    | Variable { namev } ->
        ({ generator with variables = S.add namev variables }, allocator, store)
    | Functor _ as f ->
        ({ generator with terms = FT.cons terms f }, allocator, store)
    | _ -> failwith "unreachable emit_nested_fact_argument"

  and emit_queue_nested_argument
      ((generator, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable _ ->
        let instruction = Cell.UnifyVariable register in
        let generator, store = add_instruction (generator, store) instruction in
        (generator, allocator, store)
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), register) in
        let generator, store = add_instruction (generator, store) instruction in
        List.fold_left emit_nested_argument
          (generator, allocator, store)
          elements
    | _ -> failwith "unreachable emit_queue_nested_fact_argument"

  and emit_queue_arguments
      ((({ terms; _ } as generator), allocator, store) :
        t * RegisterAllocator.t * Cell.t Store.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    match FT.front terms with
    | None -> (generator, allocator, store)
    | Some (rest, d) ->
        let generator = { generator with terms = rest } in
        let result =
          emit_queue_nested_argument (generator, allocator, store) d
        in
        emit_queue_arguments result

  and emit_argument
      ((({ variables; _ } as generator), ({ registers; _ } as allocator), store) :
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
        let generator, store =
          add_instruction ({ generator with variables }, store) instruction
        in
        (generator, allocator, store)
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), arg_register) in
        let generator, store = add_instruction (generator, store) instruction in
        List.fold_left emit_nested_argument
          (generator, allocator, store)
          elements
    | _ -> failwith "unreachable emit_fact_argument"
end

module type Argument = sig
  val emit_query :
    t * RegisterAllocator.t * Cell.t Store.t ->
    Ast.t ->
    t * RegisterAllocator.t * Cell.t Store.t

  val emit_functor :
    t * RegisterAllocator.t * Cell.t Store.t ->
    Ast.t ->
    t * RegisterAllocator.t * Cell.t Store.t
end

module Argument = struct
  let rec emit_argument
      ((variable, value, catchall) :
        (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction))
      ((({ variables; _ } as generator), ({ registers; _ } as allocator), store) :
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
        let generator, store =
          add_instruction ({ generator with variables }, store) instruction
        in
        (generator, allocator, store)
    | _ ->
        let instruction = catchall register in
        let generator, store = add_instruction (generator, store) instruction in
        (generator, allocator, store)

  and emit_query
      ((generator, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let emit_toplevel_query_argument =
      emit_argument
        ( (fun v -> Cell.SetVariable v),
          (fun v -> Cell.SetValue v),
          fun v -> Cell.SetValue v )
    in
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Functor { namef; elements; arity } ->
        let instruction = Cell.PutStructure ((namef, arity), register) in
        let generator, store = add_instruction (generator, store) instruction in
        List.fold_left emit_query (generator, allocator, store) elements
    | _ -> emit_toplevel_query_argument (generator, allocator, store) elem

  let emit_functor =
    emit_argument
      ( (fun v -> Cell.UnifyVariable v),
        (fun v -> Cell.UnifyValue v),
        fun v -> Cell.UnifyVariable v )
end

let rec allocate_head ({ elements; _ } : Ast.func)
    ((generator, ({ y_register; _ } as allocator), store) :
      t * RegisterAllocator.t * Cell.t Store.t) =
  let head_folder
      (( ( ({ scope_registers; _ } as generator),
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
    let generator, store =
      add_instruction ({ generator with scope_registers }, store) instruction
    in
    ((generator, allocator, store), counter + 1)
  in
  let instruction = Cell.Allocate y_register in
  let generator, store = add_instruction (generator, store) instruction in
  List.fold_left head_folder ((generator, allocator, store), 0) elements |> fst

and allocate_body (elements : Ast.func list) (generator, allocator, store) :
    t * RegisterAllocator.t * Cell.t Store.t =
  let allocate_clause (generator, allocator, store)
      ({ namef; elements; arity } : Ast.func) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let allocate_argument
        (( ( ({ scope_registers; _ } as generator),
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
                (scope_registers, Cell.PutValue (left_register, Cell.X counter))
          in
          let generator, store =
            add_instruction
              ({ generator with scope_registers }, store)
              instruction
          in
          ((generator, allocator, store), counter + 1)
      | Functor func as f ->
          let open RegisterAllocator.RegisterMap in
          let (generator, allocator, store), _ =
            (generate (generator, allocator, store) (Ast.Query func), counter)
          in
          let raw_register = find f registers in
          let left_register = cell_register raw_register in
          let instruction = Cell.PutValue (left_register, Cell.X counter) in
          let generator, store =
            add_instruction (generator, store) instruction
          in
          ((generator, allocator, store), counter + 1)
      | _ -> failwith "unreachable allocate_body"
    in
    let instruction = Cell.Call (namef, arity) in
    let (generator, allocator, store), _ =
      List.fold_left allocate_argument
        ((generator, allocator, store), 0)
        elements
    in
    let generator, store = add_instruction (generator, store) instruction in
    (generator, allocator, store)
  in
  let generator, allocator, store =
    List.fold_left allocate_clause (generator, allocator, store) elements
  in
  let generator, store = add_instruction (generator, store) Cell.Deallocate in
  (generator, allocator, store)

and generate
    (( ({ p_register; functor_table; _ } as generator),
       ({ registers; _ } as allocator),
       store ) :
      t * RegisterAllocator.t * Cell.t Store.t) (value : Ast.t) :
    t * RegisterAllocator.t * Cell.t Store.t =
  let open RegisterAllocator.RegisterMap in
  match value with
  | Query { namef; elements; arity } ->
      let generator, allocator, store =
        List.fold_left Argument.emit_query
          (generator, allocator, store)
          elements
      in
      let instruction = Cell.Call (namef, arity) in
      let generator, store = add_instruction (generator, store) instruction in
      ({ generator with variables = S.empty }, allocator, store)
  | Functor { namef; elements; arity } ->
      let register = cell_register @@ find value registers in
      let instruction = Cell.GetStructure ((namef, arity), register) in
      let generator, store = add_instruction (generator, store) instruction in
      let generator, allocator, store =
        List.fold_left Argument.emit_functor
          (generator, allocator, store)
          elements
      in
      let generator, allocator, store =
        List.fold_left generate (generator, allocator, store) elements
      in
      ({ generator with variables = S.empty }, allocator, store)
  | Variable _ -> (generator, allocator, store)
  | Declaration { head = { elements; namef; arity }; body = [] } ->
      let open FunctorMap in
      let functor_table = add (namef, arity) p_register functor_table in
      let generator, allocator, store =
        Seq.fold_lefti Fact.emit_argument
          ({ generator with functor_table }, allocator, store)
          (List.to_seq elements)
      in
      let generator, allocator, store =
        Fact.emit_queue_arguments (generator, allocator, store)
      in
      let generator, store = add_instruction (generator, store) Cell.Proceed in
      (generator, allocator, store)
  | Declaration { head; body } ->
      let open FunctorMap in
      let functor_table =
        add (head.namef, head.arity) p_register functor_table
      in
      ({ generator with functor_table }, allocator, store)
      |> allocate_head head |> allocate_body body
