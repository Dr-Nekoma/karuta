module FuncMap = BatMap.Make(Ast.func)

type t = {
    register_counter: int;
    structs: FuncMap.t;
  }


let initialize (): t =
  {
    register_counter = 1;
    structs = FuncMap.empty ~eq:( = );
  }

open BatMap
let compile = function
  | [] compiler computer queries -> (queries, computer)
  | [d::ds] ({register_counter; structs}) computer queries ->
     let newComputer =
       match d with
       | Functor {namef; elements; arity} ->
          let c1 = Evaluator.get_structure (namef, arity) register_counter computer in
          (queries, )
       | Variable {namev} ->
             
          
     (* in newComputer   *)

     (*   | Functor {namef; elements; arity} -> *)
     (*      match find_opt namef structs with *)
     (*      | None -> *)
     (*         {(Evaluator.put_structure register_counter (namef, arity) computer) with *)
     (*           register_counter = register_counter + 1} *)
     (*      | Some index -> Evaluator.set_value index computer *)
     (*   | Variable {namev} -> *)
  
  
