open Machine

type ast = Functor of string * ast list [@@deriving show]

let rec inspect (store : Cell.t Store.t) (register : Cell.t) : string =
  match register with
  | Structure address -> (
      match Store.get store address with
      | Functor (name, arity) ->
          let open Batteries in
          let folder acc elem =
            let inner = inspect store @@ Store.get store (address + elem + 1) in
            if acc = "" then inner else acc ^ "," ^ inner
          in
          let middle = List.fold_left folder "" (List.of_enum (0 --^ arity)) in
          if arity = 0 then name else name ^ "[" ^ middle ^ "]"
      | _ -> failwith "Nonsense via structure")
  | Reference address -> (
      let next = Store.get store @@ Evaluator.deref address store in
      match next with
      | Reference _ -> failwith "TODO: take care of free variables"
      | _ -> inspect store next)
  | Empty | Functor _ | ArgCount _ | Instruction _ | Address _ ->
      failwith "unreachable inspect"

let rec to_ast (store : Cell.t Store.t) (register : Cell.t) : ast =
  match register with
  | Structure addr -> (
      match Store.get store addr with
      | Functor (name, arity) ->
          let rec collect i acc =
            if i = arity then List.rev acc
            else
              let child = Store.get store (addr + i + 1) in
              collect (i + 1) (to_ast store child :: acc)
          in
          Functor (name, collect 0 [])
      | _ -> failwith "Expected Functor at structure address")
  | Reference addr ->
      to_ast store (Store.get store (Evaluator.deref addr store))
  | _ -> failwith "Cannot convert non-structured term to AST"

let query_ast_args ({ x_registers; args; store; _ } : Machine.t) : ast list =
  match args with
  | None -> []
  | Some how_many ->
      let open Machine.IntMap in
      let find_register idx = find idx x_registers in
      List.init how_many (fun i -> to_ast store (find_register i))

let query_args ({ store; query_variables; _ } : Machine.t) : string =
  let folder acc (variable, cell) =
    acc ^ variable ^ " = " ^ inspect store cell ^ "\n"
  in
  BatSeq.fold_left folder "" (BatMap.to_seq query_variables)
