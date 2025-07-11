open Sexplib0.Sexp_conv

type ast = Functor of {name: string; children: ast list}
[@@deriving show, sexp]

type output = ast list
[@@deriving show, sexp]
