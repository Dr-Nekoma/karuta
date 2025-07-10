open Machine

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
      failwith "unrechable inspect"

let query_args ({ x_registers; args; store; _ } : Machine.t) : string =
  match args with
  | None -> ""
  | Some how_many ->
      let open Machine.IntMap in
      let find_register idx = find idx x_registers in
      let open Batteries in
      let folder acc elem = acc ^ " | " ^ inspect store (find_register elem) in
      List.fold_left folder "" (List.of_enum (0 --^ how_many))
