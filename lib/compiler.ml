module RegisterMap = BatMap.Make (Ast)
module VariableMap = BatMap.Make (String)

type functor_name = string * int [@@deriving ord]

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

module FT = BatFingerTree
module S = BatSet

type term_queue = Ast.t FT.t
type variable_set = Ast.tag S.t
type var_frequency_map = int VariableMap.t
type functor_map = int FunctorMap.t

let multiset_mappend (m1 : var_frequency_map) (m2 : var_frequency_map) :
    var_frequency_map =
  let mappend (_ : string) (v1 : int option) (v2 : int option) : int option =
    match (v1, v2) with
    | Some n1, Some n2 -> Some (n1 + n2)
    | None, Some n2 -> Some n2
    | Some n1, None -> Some n1
    | None, None -> None
  in
  VariableMap.merge mappend m1 m2

let set_to_multiset (s : string S.t) : var_frequency_map =
  let open VariableMap in
  let lambda elem acc = add elem 1 acc in
  S.fold lambda s VariableMap.empty

open Machine

type register = Temporary of int | Permanent of int [@@deriving show]
type register_set = register S.t
type entry_point = { p_register : int; functor_name : functor_name }

type t = {
  p_register : int;
  x_register : int;
  y_register : int;
  registers : register RegisterMap.t;
  terms : term_queue;
  variables : variable_set;
  scope_variables : variable_set;
  scope_registers : register_set;
  functor_table : functor_map;
  entry_point : entry_point option;
}

let initialize () : t =
  {
    p_register = 0;
    x_register = 0;
    y_register = 0;
    registers = RegisterMap.empty;
    terms = FT.empty;
    variables = S.empty;
    scope_variables = S.empty;
    scope_registers = S.empty;
    functor_table = FunctorMap.empty;
    entry_point = None;
  }

let show_registers (registers : register RegisterMap.t) : string =
  let open RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Ast.show term ^ " = " ^ show_register register)
    "" (to_seq registers)
[@@warning "-32"]

let rec reset_scope (compiler : t) (arity : int) : t =
  {
    compiler with
    scope_variables = S.empty;
    scope_registers = S.empty;
    x_register = arity;
    y_register = 0;
  }

and compile : Ast.t list * t * Cell.t Store.t -> t * Cell.t Store.t = function
  | [], compiler, store -> (compiler, store)
  | d :: ds, compiler, store -> (
      match d with
      | Query f as query -> (
          let ({ p_register; entry_point; _ } as compiler), store =
            register_alloc_query f (reset_scope compiler 0) store
          in
          match entry_point with
          | None ->
              let functor_name = (f.namef, f.arity) in
              let entry_point = Some { p_register; functor_name } in
              let compiler = { compiler with entry_point } in
              let compiler, store = generate_code (compiler, store) query in
              compile (ds, compiler, store)
          | Some _ -> failwith "multiple queries are not supported yet")
      | Variable _ | Functor _ -> failwith "unreachable compile"
      | Declaration { head; body = [] } as decl ->
          let compiler, store =
            register_alloc_functor head (reset_scope compiler 0) store
          in
          let compiler, store = generate_code (compiler, store) decl in
          compile (ds, compiler, store)
      | Declaration { head; body } as declaration ->
          let compiler, store =
            register_alloc_declaration head body
              (reset_scope compiler head.arity)
              store
          in
          let compiler, store = generate_code (compiler, store) declaration in
          compile (ds, compiler, store))

and alloc_register
    ({ registers; scope_variables; x_register; y_register; _ } as compiler)
    (elem : Ast.t) : t =
  let register, x_register, y_register =
    match elem with
    | Variable { namev } -> (
        match S.find_opt namev scope_variables with
        | None -> (Temporary x_register, x_register + 1, y_register)
        | Some _ -> (Permanent y_register, x_register, y_register + 1))
    | _ -> (Temporary x_register, x_register + 1, y_register)
  in
  let open RegisterMap in
  let registers = add elem register registers in
  { compiler with registers; x_register; y_register }

and cell_register : register -> Cell.register = function
  | Temporary value -> Cell.X value
  | Permanent value -> Cell.Y value

and generate_code
    ((({ p_register; registers; functor_table; _ } as compiler), store) :
      t * Cell.t Store.t) (value : Ast.t) : t * Cell.t Store.t =
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
      ((({ registers; variables; _ } as compiler), store) : t * Cell.t Store.t)
      (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable { namev } ->
        let variables, instruction =
          match S.find_opt namev variables with
          | None -> (S.add namev variables, variable register)
          | Some _ -> (variables, value register)
        in
        add_instruction ({ compiler with variables }, store) instruction
    | _ ->
        let instruction = catchall register in
        add_instruction (compiler, store) instruction
  in
  let emit_functor_argument =
    emit_argument
      ( (fun v -> Cell.UnifyVariable v),
        (fun v -> Cell.UnifyValue v),
        fun v -> Cell.UnifyVariable v )
  in
  let emit_nested_fact_argument
      ((({ registers; terms; _ } as compiler), store) : t * Cell.t Store.t)
      (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let register = cell_register @@ find elem registers in
    let instruction = Cell.UnifyVariable register in
    let ((compiler, store) as result) =
      add_instruction (compiler, store) instruction
    in
    match elem with
    | Variable _ -> result
    | Functor _ as f -> ({ compiler with terms = FT.cons terms f }, store)
    | _ -> failwith "unreachable emit_nested_fact_argument"
  in
  let emit_queue_nested_fact_argument
      ((({ registers; _ } as compiler), store) : t * Cell.t Store.t)
      (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let register = cell_register @@ find elem registers in
    match elem with
    | Variable _ ->
        let instruction = Cell.UnifyVariable register in
        add_instruction (compiler, store) instruction
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_nested_fact_argument (compiler, store) elements
    | _ -> failwith "unreachable emit_queue_nested_fact_argument"
  in
  let rec emit_queue_fact_arguments
      ((({ terms; _ } as compiler), store) : t * Cell.t Store.t) :
      t * Cell.t Store.t =
    match FT.front terms with
    | None -> (compiler, store)
    | Some (rest, d) ->
        let compiler = { compiler with terms = rest } in
        let result = emit_queue_nested_fact_argument (compiler, store) d in
        emit_queue_fact_arguments result
  in
  let emit_fact_argument
      ((({ registers; _ } as compiler), store) : t * Cell.t Store.t)
      (index : int) (elem : Ast.t) : t * Cell.t Store.t =
    let open RegisterMap in
    let register = cell_register @@ find elem registers in
    let arg_register = Cell.X index in
    match elem with
    | Variable _ ->
        let instruction = Cell.GetValue (register, arg_register) in
        add_instruction (compiler, store) instruction
    | Functor { namef; arity; elements } ->
        let instruction = Cell.GetStructure ((namef, arity), arg_register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_nested_fact_argument (compiler, store) elements
    | _ -> failwith "unreachable emit_fact_argument"
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
    let register = cell_register @@ find elem registers in
    match elem with
    | Functor { namef; elements; arity } ->
        let instruction = Cell.PutStructure ((namef, arity), register) in
        let compiler, store = add_instruction (compiler, store) instruction in
        List.fold_left emit_query_argument (compiler, store) elements
    | _ -> emit_toplevel_query_argument (compiler, store) elem
  in
  let head_folder
      ((({ registers; scope_registers; _ } as compiler), store), counter)
      element : (t * Cell.t Store.t) * int =
    let open RegisterMap in
    let raw_register = find element registers in
    let register = cell_register raw_register in
    let instruction = Cell.GetVariable (register, Cell.X counter) in
    let scope_registers = S.add raw_register scope_registers in
    ( add_instruction ({ compiler with scope_registers }, store) instruction,
      counter + 1 )
  in
  let allocate_head ({ elements; _ } : Ast.func)
      (({ y_register; _ } as compiler), store) : t * Cell.t Store.t =
    let instruction = Cell.Allocate y_register in
    let compiler, store = add_instruction (compiler, store) instruction in
    List.fold_left head_folder ((compiler, store), 0) elements |> fst
  in
  let allocate_body (elements : Ast.func list) (compiler, store) :
      t * Cell.t Store.t =
    let allocate_clause (compiler, store)
        ({ namef; elements; arity } : Ast.func) : t * Cell.t Store.t =
      let allocate_argument
          ((({ scope_registers; registers; _ } as compiler), store), counter)
          (individual_element : Ast.t) : (t * Cell.t Store.t) * int =
        match individual_element with
        | Variable _ as var ->
            let open RegisterMap in
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
            ( add_instruction
                ({ compiler with scope_registers }, store)
                instruction,
              counter + 1 )
        | Functor func as f ->
            let open RegisterMap in
            let (compiler, store), _ =
              (generate_code (compiler, store) (Ast.Query func), counter)
            in
            let raw_register = find f registers in
            let left_register = cell_register raw_register in
            let instruction = Cell.PutValue (left_register, Cell.X counter) in
            (add_instruction (compiler, store) instruction, counter + 1)
        | _ -> failwith "unreachable allocate_body"
      in
      let instruction = Cell.Call (namef, arity) in
      let (compiler, store), _ =
        List.fold_left allocate_argument ((compiler, store), 0) elements
      in
      add_instruction (compiler, store) instruction
    in
    let compiler, store =
      List.fold_left allocate_clause (compiler, store) elements
    in
    add_instruction (compiler, store) Cell.Deallocate
  in
  let open RegisterMap in
  match value with
  | Query { namef; elements; arity } ->
      let compiler, store =
        List.fold_left emit_query_argument (compiler, store) elements
      in
      let instruction = Cell.Call (namef, arity) in
      let compiler, store = add_instruction (compiler, store) instruction in
      ({ compiler with variables = S.empty }, store)
  | Functor { namef; elements; arity } ->
      let register = cell_register @@ find value registers in
      let instruction = Cell.GetStructure ((namef, arity), register) in
      let compiler, store = add_instruction (compiler, store) instruction in
      let compiler, store =
        List.fold_left emit_functor_argument (compiler, store) elements
      in
      let compiler, store =
        List.fold_left generate_code (compiler, store) elements
      in
      ({ compiler with variables = S.empty }, store)
  | Variable _ -> (compiler, store)
  | Declaration { head = { elements; namef; arity }; body = [] } ->
      let open FunctorMap in
      let functor_table = add (namef, arity) p_register functor_table in
      let compiler, store =
        Seq.fold_lefti emit_fact_argument
          ({ compiler with functor_table }, store)
          (List.to_seq elements)
      in
      let compiler, store = emit_queue_fact_arguments (compiler, store) in
      add_instruction (compiler, store) Cell.Proceed
  | Declaration { head; body } ->
      let open FunctorMap in
      let functor_table =
        add (head.namef, head.arity) p_register functor_table
      in
      ({ compiler with functor_table }, store)
      |> allocate_head head |> allocate_body body

and register_alloc_loop : t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun ({ terms; _ } as compiler) store ->
  match FT.front terms with
  | None -> (compiler, store)
  | Some (rest, d) -> (
      let new_compiler = { compiler with terms = rest } in
      match d with
      | Declaration _ -> failwith "unreachable register_alloc_loop"
      | Variable v -> register_alloc_variable v new_compiler store
      | Query _ -> failwith "there's no such thing as a nested query"
      | Functor f -> register_alloc_functor f new_compiler store)

and extract_variables (elem : Ast.t) : string S.t =
  match elem with
  | Declaration _ | Query _ -> failwith "unreachable extract_variables"
  | Functor { elements; _ } ->
      let initial_set = S.empty in
      List.fold_left
        (fun acc e -> S.union acc @@ extract_variables e)
        initial_set elements
  | Variable { namev } -> S.add namev S.empty

and register_alloc_declaration :
    Ast.func -> Ast.func list -> t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun ({ elements; _ } as head) body ({ terms; _ } as compiler) store ->
  match body with
  | [] ->
      register_alloc_loop
        { compiler with terms = FT.cons terms (Ast.Functor head) }
        store
  | clauses ->
      let scope_variables =
        let extracted_head_variables =
          set_to_multiset
          @@ List.fold_left S.union S.empty
          @@ List.map extract_variables
               [ Ast.Functor head; Ast.Functor (List.hd clauses) ]
        in
        let folder acc e =
          multiset_mappend acc @@ set_to_multiset @@ extract_variables
          @@ Ast.Functor e
        in
        List.fold_left folder extracted_head_variables (List.tl clauses)
        |> VariableMap.filterv (fun v -> v != 1)
        |> VariableMap.keys |> S.of_enum
      in
      let ( ++ ) = FT.append in
      let terms =
        let open Ast in
        terms
        ++ (FT.of_list
           @@ List.concat_map (fun { elements; _ } -> elements) clauses)
        ++ FT.of_list elements
      in
      register_alloc_loop { compiler with scope_variables; terms } store

and register_alloc_functor :
    Ast.func -> t -> Cell.t Store.t -> t * Cell.t Store.t =
 fun ({ elements; _ } as func) ({ terms; _ } as compiler) store ->
  let compiler = alloc_register compiler (Ast.Functor func) in
  let terms = FT.append terms (FT.of_list elements) in
  register_alloc_loop { compiler with terms } store

and register_alloc_query : Ast.func -> t -> Cell.t Store.t -> t * Cell.t Store.t
    =
 fun { elements; _ } ({ terms; _ } as compiler) store ->
  let terms = FT.append terms (FT.of_list elements) in
  register_alloc_loop { compiler with terms } store

and register_alloc_variable :
    Ast.var -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun var ({ registers; _ } as compiler) store ->
    match find_opt (Ast.Variable var) registers with
    | None ->
        let compiler = alloc_register compiler (Ast.Variable var) in
        register_alloc_loop compiler store
    | Some _ -> register_alloc_loop compiler store
