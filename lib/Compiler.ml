type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

type functor_map = int FunctorMap.t

module SigmaMap = BatMap.Make (struct
  type t = string [@@deriving ord]
end)

type comptime_entity =
  | Module of {
      signature : Ast.func option;
      body : Ast.clause list;
      environment : sigma_map;
    }
  | Signature of Ast.func

and sigma_map = comptime_entity SigmaMap.t

let show_functor_table (functors : functor_map) : string =
  let open FunctorMap in
  BatSeq.fold_left
    (fun acc ((label, arity), address) ->
      acc ^ label ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)

let show_query_variables (variables : Machine.query_map) : string =
  BatSeq.fold_left
    (fun acc (name, cell) -> acc ^ name ^ " = " ^ Machine.Cell.show cell ^ "\n")
    "" (BatMap.to_seq variables)

type t = {
  entry_point : entry_point option;
  p_register : int;
  functor_table : functor_map;
  sigma_table : sigma_map;
  module_prefix : string list;
}

let initialize () : t =
  {
    entry_point = None;
    p_register = 0;
    functor_table = FunctorMap.empty;
    sigma_table = SigmaMap.empty;
    module_prefix = [];
  }

let rec allocate_registers (elem : Ast.clause) : RegisterAllocator.t list =
  match elem with
  | (MultiDeclaration _ | Query _) as form ->
      RegisterAllocator.allocate_toplevel form

and compile :
    Ast.clause list * t * Machine.Cell.t Machine.Store.t ->
    t * Machine.Cell.t Machine.Store.t = function
  | [], compiler, store -> (compiler, store)
  | ( d :: ds,
      ({ entry_point; p_register; functor_table; sigma_table; module_prefix } as
       compiler),
      store ) -> (
      match d with
      | Directive ({ namef = "comment"; _ }, _) -> compile (ds, compiler, store)
      | Directive
          ( {
              namef = "signature";
              arity = 2;
              elements =
                [
                  Ast.Functor { namef; arity = 0; elements = [] };
                  (* TODO: check if body is an atom or a proper list *)
                  Ast.Functor body;
                ];
              _;
            },
            _ ) -> (
          let open SigmaMap in
          match find_opt namef sigma_table with
          | Some _ ->
              failwith @@ "Name collision when defining signature " ^ namef
          | None ->
              compile
                ( ds,
                  {
                    compiler with
                    sigma_table = add namef (Signature body) sigma_table;
                  },
                  store ))
      | Directive
          ( {
              namef = "module";
              arity;
              elements =
                Ast.Functor { namef; arity = 0; elements = [] } :: module_tail;
            },
            body )
        when (arity = 1 && module_tail = [])
             || arity = 2 && module_tail <> []
                && Ast.Expr.is_functor (List.hd module_tail) -> (
          let open SigmaMap in
          match find_opt namef sigma_table with
          | Some _ -> failwith @@ "Name collision when defining module " ^ namef
          | None ->
              compile
                ( ds,
                  {
                    compiler with
                    module_prefix = namef :: module_prefix;
                    sigma_table =
                      add namef
                        (let signature =
                           if arity = 1 then None
                           else
                             Some
                               (match List.hd module_tail with
                               | Ast.Functor f -> f
                               | _ -> failwith "unreachable")
                         in
                         (* TODOs:
                            1. Add a pass to find all defined names before compilation.
                            2. Recursively compile the body.
                         *)
                         Module { signature; environment = sigma_table; body })
                        sigma_table;
                  },
                  store ))
      | MultiDeclaration ({ head; _ }, _) as form ->
          let open FunctorMap in
          ( allocate_registers form |> fun allocators ->
            CodeGenerator.generate
              (CodeGenerator.initialize p_register, allocators, store)
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
            | Some _ ->
                failwith
                  "Multiple queries are only supported using the comma syntax"
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
