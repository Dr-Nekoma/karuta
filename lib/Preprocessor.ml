let from_declaration (clause : Ast.t) : Ast.decl =
  match clause with
  | Declaration decl -> decl
  | _ -> failwith "unreachable from_declaration"

let show_clauses (clauses : Ast.t list) : string =
  List.fold_left (fun acc term -> acc ^ "\n" ^ Ast.show term) "" clauses
[@@warning "-32"]

let group_clauses (clauses : Ast.t list) : Ast.t list =
  let compare_func (f1 : Ast.func) (f2 : Ast.func) : int =
    if f1.namef == f2.namef && f1.arity == f2.arity then 0 else 1
  in
  let compare_clauses (c1 : Ast.t) (c2 : Ast.t) : int =
    match (c1, c2) with
    | Declaration { head = h1; _ }, Declaration { head = h2; _ } ->
        compare_func h1 h2
    | _, _ -> 1
  in
  let multi_mapper (group : Ast.t list) : Ast.t =
    match group with
    | [ x ] -> x
    | Declaration first :: _ as many ->
        Ast.MultiDeclaration
          (first.head.namef, first.head.arity, List.map from_declaration many)
    | _ -> failwith "unreachable group"
  in
  let open Batteries in
  print_endline @@ show_clauses clauses;
  let new_clauses =
    clauses |> List.group compare_clauses |> List.map multi_mapper
  in
  print_endline "New";
  print_endline @@ show_clauses new_clauses;
  new_clauses
