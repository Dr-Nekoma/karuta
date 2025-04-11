type tag = string [@@deriving show, ord]

type expr = Variable of var | Functor of func [@@deriving show, ord]

and clause =
  | Declaration of decl
  | MultiDeclaration of (tag * int * decl list)
  | Query of func
[@@deriving show, ord]

and var = { namev : tag } [@@deriving show, ord]

and func = { namef : tag; elements : expr list; arity : int }
[@@deriving show, ord]

and decl = { head : func; body : func list } [@@deriving show, ord]

type exprs = expr list [@@deriving show, ord]

module Clause = struct
  type t = clause [@@deriving show, ord]
end

module Expr = struct
  type t = expr [@@deriving show, ord]
end
