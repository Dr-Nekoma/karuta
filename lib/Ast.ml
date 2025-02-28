type tag = string [@@deriving show, ord]

type t =
  | Variable of var
  | Functor of func
  | Declaration of decl
  | Query of func
[@@deriving show, ord]

and var = { namev : tag } [@@deriving show, ord]

and func = { namef : tag; elements : t list; arity : int }
[@@deriving show, ord]

and decl = { head : func; body : func list } [@@deriving show, ord]

type ts = t list [@@deriving show, ord]
