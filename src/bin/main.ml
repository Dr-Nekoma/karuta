module AST = struct
  type tag = string
  type t =
    | Variable of { name: tag }
    | Functor of { name: tag; elements: t list; arity: int }
end

module Evaluation = struct

  module Map = Map.Make(String)
  (* L0 defines the program as a single term *)
  (* In the future, upgrade this to a database with a list of terms *)
  let question (_program: AST.t) (_query: AST.t): AST.t Map.t list =
    failwith "Not Implemented"
end

module Main = struct
    let () =
        print_endline "Hello World! :)"
end
