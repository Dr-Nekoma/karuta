open Sexplib0.Sexp_conv
open Machine

type ast = Functor of { name : string; children : ast list }
[@@deriving show, sexp]

type output = ast list [@@deriving show, sexp]

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
          Functor { name; children = collect 0 [] }
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
