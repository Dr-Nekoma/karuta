type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

type functor_map = int FunctorMap.t

type t = {
  entry_point : entry_point option;
  p_register : int;
  functor_table : functor_map;
}

let initialize () : t =
  { entry_point = None; p_register = 0; functor_table = FunctorMap.empty }

let rec allocate_registers (elem : Ast.t) : RegisterAllocator.t =
  match elem with
  | Variable _ | Functor _ -> failwith "not top level forms"
  | (Declaration _ | Query _) as form ->
      RegisterAllocator.allocate_toplevel form

and compile :
    Ast.t list * t * Machine.Cell.t Machine.Store.t ->
    t * Machine.Cell.t Machine.Store.t = function
  | [], compiler, store -> (compiler, store)
  | d :: ds, ({ entry_point; p_register; functor_table } as compiler), store
    -> (
      match d with
      | Variable _ | Functor _ -> failwith "unreachable compile"
      | Declaration { head; _ } as form ->
          let open FunctorMap in
          ( allocate_registers form |> fun allocator ->
            CodeGenerator.generate
              (CodeGenerator.initialize p_register, allocator, store)
              form )
          |> fun (code_generator, _, store) ->
          compile
            ( ds,
              {
                compiler with
                p_register = code_generator.p_register;
                functor_table =
                  add (head.namef, head.arity) p_register functor_table;
              },
              store )
      | Query _ as form ->
          let entry_point =
            match entry_point with
            | None -> Some { p_register }
            | Some _ -> failwith "multiple queries are not supported yet"
          in
          ( allocate_registers form |> fun allocator ->
            CodeGenerator.generate
              (CodeGenerator.initialize p_register, allocator, store)
              form )
          |> fun (code_generator, _, store) ->
          compile
            ( ds,
              {
                compiler with
                entry_point;
                p_register = code_generator.p_register;
              },
              store ))
