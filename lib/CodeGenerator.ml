module VariableMap = BatMap.Make (String)

type var_frequency_map = int VariableMap.t

module FT = BatFingerTree
module S = BatSet

type register_set = RegisterAllocator.register S.t

open Machine

type t = {
  p_register : int;
  terms : RegisterAllocator.term_queue;
  variables : RegisterAllocator.variable_set;
}

let initialize (begin_addr : int) : t =
  { p_register = begin_addr; terms = FT.empty; variables = S.empty }

let reset_variables
    (({ p_register; _ }, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  (initialize p_register, allocators, store)

let put_allocator (allocator : RegisterAllocator.t)
    ((generator, store) : t * Cell.t Store.t) :
    t * RegisterAllocator.t * Cell.t Store.t =
  (generator, allocator, store)

let cell_register : RegisterAllocator.register -> Cell.register = function
  | Temporary value -> Cell.X value
  | Permanent value -> Cell.Y value

and add_instruction (instruction : Cell.instruction)
    ((({ p_register; _ } as generator), store) : t * Cell.t Store.t) :
    t * Cell.t Store.t =
  (* TODO Add compiler builtins for better dev experience *)
  let real_instruction =
    match instruction with Cell.Call ("debug", 0) -> Cell.Debug | inst -> inst
  in
  let store =
    Store.code_put (Cell.Instruction real_instruction) p_register store
  in
  ({ generator with p_register = p_register + 1 }, store)

module type Fact = sig
  val emit_argument :
    t * RegisterAllocator.t * Cell.t Store.t ->
    int ->
    Ast.expr ->
    t * RegisterAllocator.t * Cell.t Store.t

  val emit_queue_arguments :
    t * RegisterAllocator.t * Cell.t Store.t ->
    t * RegisterAllocator.t * Cell.t Store.t
end

module Fact : Fact = struct
  let rec emit_nested_argument
      (( ({ terms; variables; _ } as generator),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None -> (S.add namev variables, Cell.UnifyVariable register)
          | Some _ -> (variables, Cell.UnifyValue register)
        in
        let generator, store =
          add_instruction instruction ({ generator with variables }, store)
        in
        (generator, allocator, store)
    | Functor _ as f ->
        let generator, store =
          add_instruction (Cell.UnifyValue register) (generator, store)
        in
        ({ generator with terms = FT.cons terms f }, allocator, store)

  and emit_queue_nested_argument
      ((generator, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable _ ->
        let instruction = Cell.UnifyVariable register in
        (generator, store)
        |> add_instruction instruction
        |> put_allocator allocator
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), register) in
        let generator, store = add_instruction instruction (generator, store) in
        List.fold_left emit_nested_argument
          (generator, allocator, store)
          elements

  and emit_queue_arguments
      ((({ terms; _ } as generator), allocator, store) :
        t * RegisterAllocator.t * Cell.t Store.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    match FT.front terms with
    | None -> (generator, allocator, store)
    | Some (rest, elem) ->
        elem
        |> emit_queue_nested_argument
             ({ generator with terms = rest }, allocator, store)
        |> emit_queue_arguments

  and emit_argument
      ((({ variables; _ } as generator), ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (index : int)
      (elem : Ast.expr) : t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    let arg_register = Cell.X index in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None ->
              (S.add namev variables, Cell.GetVariable (register, arg_register))
          | Some _ -> (variables, Cell.GetValue (register, arg_register))
        in
        ({ generator with variables }, store)
        |> add_instruction instruction
        |> put_allocator allocator
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), arg_register) in
        let generator, store = add_instruction instruction (generator, store) in
        List.fold_left emit_nested_argument
          (generator, allocator, store)
          elements
end

module type Argument = sig
  val emit_query :
    t * RegisterAllocator.t * Cell.t Store.t ->
    Ast.expr ->
    t * RegisterAllocator.t * Cell.t Store.t

  val emit_functor_argument :
    t * RegisterAllocator.t * Cell.t Store.t ->
    Ast.expr ->
    t * RegisterAllocator.t * Cell.t Store.t
end

module Argument : Argument = struct
  let emit_argument
      ((variable, value, catchall) :
        (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction)
        * (Cell.register -> Cell.instruction))
      ((({ variables; _ } as generator), ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
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
        ({ generator with variables }, store)
        |> add_instruction instruction
        |> put_allocator allocator
    | _ ->
        let instruction = catchall register in
        (generator, store)
        |> add_instruction instruction
        |> put_allocator allocator

  let emit_functor_argument =
    emit_argument
      ( (fun v -> Cell.SetVariable v),
        (fun v -> Cell.SetValue v),
        fun v -> Cell.SetValue v )

  let rec emit_query
      ((generator, ({ registers; _ } as allocator), store) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let emit_toplevel_query_argument = emit_functor_argument in
    let open RegisterAllocator.RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Functor { namef; elements; arity } ->
        let generator, allocator, store =
          List.fold_left emit_query (generator, allocator, store) elements
        in
        let instruction = Cell.PutStructure ((namef, arity), register) in
        let generator, store = add_instruction instruction (generator, store) in
        List.fold_left emit_functor_argument
          (generator, allocator, store)
          elements
    | _ -> emit_toplevel_query_argument (generator, allocator, store) elem
end

let rec allocate_head ({ elements; _ } : Ast.func)
    ((generator, ({ y_register; _ } as allocator), store) :
      t * RegisterAllocator.t * Cell.t Store.t) =
  let instruction = Cell.Allocate y_register in
  let generator, store = add_instruction instruction (generator, store) in
  Seq.fold_lefti Fact.emit_argument
    (generator, allocator, store)
    (List.to_seq elements)

and allocate_body (elements : Ast.func list) (generator, allocator, store) :
    t * RegisterAllocator.t * Cell.t Store.t =
  let allocate_clause (generator, allocator, store)
      ({ namef; elements; arity } : Ast.func) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let allocate_argument
        (( ( ({ variables; _ } as generator),
             ({ registers; _ } as allocator),
             store ),
           counter ) :
          (t * RegisterAllocator.t * Cell.t Store.t) * int)
        (individual_element : Ast.expr) :
        (t * RegisterAllocator.t * Cell.t Store.t) * int =
      match individual_element with
      | Variable { namev } as var ->
          let open RegisterAllocator.RegisterMap in
          let left_register = cell_register @@ find var registers in
          let variables, instruction =
            match S.find_opt namev variables with
            | None ->
                ( S.add namev variables,
                  Cell.PutVariable (left_register, Cell.X counter) )
            | Some _ ->
                (variables, Cell.PutValue (left_register, Cell.X counter))
          in
          let generator, store =
            add_instruction instruction ({ generator with variables }, store)
          in
          ((generator, allocator, store), counter + 1)
      | Functor func as f ->
          let open RegisterAllocator.RegisterMap in
          let (generator, allocator, store), _ =
            (generate_functor (generator, allocator, store) func, counter)
          in
          let left_register = cell_register @@ find f registers in
          let instruction = Cell.PutValue (left_register, Cell.X counter) in
          let generator, store =
            add_instruction instruction (generator, store)
          in
          ((generator, allocator, store), counter + 1)
    in
    let (generator, allocator, store), _ =
      List.fold_left allocate_argument
        ((generator, allocator, store), 0)
        elements
    in
    let instruction = Cell.Call (namef, arity) in
    (generator, store) |> add_instruction instruction |> put_allocator allocator
  in
  let generator, allocator, store =
    List.fold_left allocate_clause (generator, allocator, store) elements
  in
  (* TODO: We know the last instruction will be a Call because of the fold above *)
  (* TODO: Corner case being Debug instruction *)
  let previous_p_register = generator.p_register - 1 in
  let (Cell.Instruction (Cell.Call lastCallFunctor)) =
    Store.code_get store previous_p_register
  in
  (generator, store)
  |> add_instruction (Cell.Execute lastCallFunctor)
  |> (fun (generator, store) ->
       ( generator,
         Store.code_put (Cell.Instruction Cell.Deallocate) previous_p_register
           store ))
  |> put_allocator allocator

and generate_functor (generator, ({ registers; _ } as allocator), store)
    ({ elements; arity; namef } as func : Ast.func) =
  let open RegisterAllocator.RegisterMap in
  let register = cell_register @@ find (Ast.Functor func) registers in
  let instruction = Cell.PutStructure ((namef, arity), register) in
  let generator, store = add_instruction instruction (generator, store) in
  List.fold_left Argument.emit_functor_argument
    (generator, allocator, store)
    elements

and swap_allocators (allocators : RegisterAllocator.t list)
    ((generator, _, store) : t * RegisterAllocator.t * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  (generator, allocators, store)

and generate_single_declaration (decl : Ast.decl)
    ((generator, allocator :: allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  match decl with
  | { head = { elements; _ }; body = [] } ->
      let gas =
        Seq.fold_lefti Fact.emit_argument
          (generator, allocator, store)
          (List.to_seq elements)
      in
      let generator, allocator, store = Fact.emit_queue_arguments gas in
      (generator, store)
      |> add_instruction Cell.Proceed
      |> put_allocator allocator |> swap_allocators allocators
      |> reset_variables
  | { head; body } ->
      (generator, allocator, store)
      |> allocate_head head |> allocate_body body |> swap_allocators allocators
      |> reset_variables

and generate_declaration_and_patch (inst : int -> Cell.instruction)
    (decl : Ast.decl)
    ((generator, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  let address_to_patch = generator.p_register in
  (generator, store) |> add_instruction Cell.Halt (* Placeholder *)
  |> fun (generator, store) ->
  (generator, allocators, store) |> generate_single_declaration decl
  |> fun (({ p_register; _ } as generator), allocators, store) ->
  ( generator,
    allocators,
    Store.code_put (Cell.Instruction (inst p_register)) address_to_patch store
  )

and generate
    ((generator, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) (value : Ast.clause) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  match value with
  | QueryConjunction { namef; elements; arity } ->
      let (allocator :: allocators) = allocators in
      let generator, allocator, store =
        List.fold_left Argument.emit_query
          (generator, allocator, store)
          elements
      in
      let instruction = Cell.Call (namef, arity) in
      (generator, store)
      |> add_instruction instruction
      |> add_instruction Cell.Halt
      |> (fun (generator, store) ->
           ({ generator with variables = S.empty }, store))
      |> put_allocator allocator
      |> swap_allocators allocators (* TODO: deal with multiple queries *)
  | MultiDeclaration (decl, []) ->
      (generator, allocators, store) |> generate_single_declaration decl
  | MultiDeclaration (first, decls) ->
      let rec split_last (l : 'a list) : 'a list * 'a =
        match l with
        | [] -> failwith "impossible"
        | [ final ] -> ([], final)
        | first :: rest ->
            let left, final = split_last rest in
            (first :: left, final)
      in
      let middle_decls, last_decl = split_last decls in
      (generator, allocators, store)
      |> generate_declaration_and_patch (fun n -> Cell.TryMeElse n) first
      |> fun acc ->
      List.fold_left
        (Fun.flip
        @@ generate_declaration_and_patch (fun n -> Cell.RetryMeElse n))
        acc middle_decls
      |> generate_declaration_and_patch (Fun.const Cell.TrustMe) last_decl
