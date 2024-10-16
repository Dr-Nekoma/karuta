let say_hi () = print_endline "Hello, Karuta!"

type tag = string
type t =
  | Variable of { name: tag }
  | Functor of { name: tag; elements: t list; arity: int }

