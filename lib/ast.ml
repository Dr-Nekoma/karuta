type tag = string
[@@deriving show]

type t =
  | Variable of var
  | Functor of func
  | Declaration of decl
  [@@deriving show]
and var = { namev : tag }
  [@@deriving show]
and func = { namef : tag; elements : t list; arity : int }
  [@@deriving show]
and decl = { head : func; body : func list}
  [@@deriving show]

type ts = t list
  [@@deriving show]

