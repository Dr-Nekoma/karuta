module RegisterMap = BatMap.Make (Ast)
module VariableMap = BatMap.Make (String)
module FT = BatFingerTree
module S = BatSet

type term_queue = Ast.t FT.t
type variable_set = Ast.tag S.t
type var_frequency_map = int VariableMap.t

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

type t = {
  p_register : int;
  x_register : int;
  y_register : int;
  registers : register RegisterMap.t;
  terms : term_queue;
  variables : variable_set;
  scope_variables : variable_set;
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
  }

let show_registers (registers : register RegisterMap.t) : string =
  let open RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Ast.show term ^ " = " ^ show_register register)
    "" (to_seq registers)
[@@warning "-32"]

let rec reset_scope (compiler : t) : t =
  { compiler with scope_variables = S.empty; x_register = 0; y_register = 0 }

and compile : Ast.t list * t * Cell.t Store.t -> t * Cell.t Store.t = function
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
            register_alloc_declaration head body (reset_scope compiler) store
          in
          print_endline @@ show_registers compiler.registers;
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
      let register = cell_register @@ find (Ast.Functor func) registers in
      let instruction = Cell.PutStructure ((namef, arity), register) in
      let compiler, store = add_instruction (compiler, store) instruction in
      let compiler, store =
        List.fold_left emit_toplevel_query_argument (compiler, store) elements
      in
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
  | Declaration { head; _ } ->
      (* TODO: generate code for bodies of declarations *)
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

and extract_variables (elem : Ast.t) : string S.t =
  match elem with
  | Declaration _ | Query _ -> failwith "unreachable"
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

and register_alloc_variable :
    Ast.var -> t -> Cell.t Store.t -> t * Cell.t Store.t =
  let open RegisterMap in
  fun var ({ registers; _ } as compiler) store ->
    match find_opt (Ast.Variable var) registers with
    | None ->
        let compiler = alloc_register compiler (Ast.Variable var) in
        register_alloc_loop compiler store
    | Some _ -> register_alloc_loop compiler store
