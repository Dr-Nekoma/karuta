type entry_point = {
  p_register : int;
  functor_name : CodeGenerator.functor_name;
}

type t = { entry_point : entry_point option }

let initialize () : t = { entry_point = None }

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

let rec allocate_registers
    (({ entry_point }, ({ p_register; _ } as generator)) : t * CodeGenerator.t)
    (elem : Ast.t) : t * CodeGenerator.t * RegisterAllocator.t =
  match elem with
  | Variable _ | Functor _ -> failwith "not top level forms"
  | (Declaration { head = f; _ } | Query f) as form -> (
      let allocator = RegisterAllocator.allocate_toplevel form in
      match entry_point with
      | None ->
          let functor_name = (f.namef, f.arity) in
          let entry_point = Some { p_register; functor_name } in
          ({ entry_point }, generator, allocator)
      | Some _ -> failwith "multiple queries are not supported yet")

and compile :
    Ast.t list * t * CodeGenerator.t * Machine.Cell.t Machine.Store.t ->
    t * CodeGenerator.t * Machine.Cell.t Machine.Store.t = function
  | [], compiler, generator, store -> (compiler, generator, store)
  | d :: ds, compiler, generator, store -> (
      match d with
      | Variable _ | Functor _ -> failwith "unreachable compile"
      | (Declaration _ | Query _) as form ->
          let compiler, generator, allocator =
            allocate_registers (compiler, generator) form
          in
          let generator, _, store =
            CodeGenerator.generate (generator, allocator, store) form
          in
          compile (ds, compiler, generator, store))
