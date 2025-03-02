type entry_point = {
  p_register : int;
  functor_name : CodeGenerator.functor_name;
}

type t = { entry_point : entry_point option; code_generator : CodeGenerator.t }

let initialize () : t =
  { entry_point = None; code_generator = CodeGenerator.initialize () }

let rec allocate_registers ({ entry_point; code_generator } : t) (elem : Ast.t)
    : t * RegisterAllocator.t =
  match elem with
  | Variable _ | Functor _ -> failwith "not top level forms"
  | (Declaration { head = f; _ } | Query f) as form -> (
      let allocator = RegisterAllocator.allocate_toplevel form in
      match entry_point with
      | None ->
          let functor_name = (f.namef, f.arity) in
          let entry_point =
            Some { p_register = code_generator.p_register; functor_name }
          in
          ({ entry_point; code_generator }, allocator)
      | Some _ -> failwith "multiple queries are not supported yet")

and compile :
    Ast.t list * t * Machine.Cell.t Machine.Store.t ->
    t * Machine.Cell.t Machine.Store.t = function
  | [], compiler, store -> (compiler, store)
  | d :: ds, compiler, store -> (
      match d with
      | Variable _ | Functor _ -> failwith "unreachable compile"
      | (Declaration _ | Query _) as form ->
          let compiler, allocator = allocate_registers compiler form in
          let code_generator, _, store =
            CodeGenerator.generate
              (compiler.code_generator, allocator, store)
              form
          in
          compile (ds, { compiler with code_generator }, store))
