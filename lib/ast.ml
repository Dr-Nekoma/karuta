type tag = string
[@@deriving show]

type t =
  | Variable of { name : tag }
  | Functor of { name : tag; elements : t list; arity : int }
  [@@deriving show]
