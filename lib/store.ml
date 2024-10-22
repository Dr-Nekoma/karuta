module type Layout = sig
  val max_heap_size : int
  val max_pdl_size : int
end

module type Memory = sig
  type 'a t

  (* Store Operations *)
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a
  val top : 'a t -> 'a
  val get : 'a t -> int -> 'a (* TODO: Make this get safe *)
  val put : 'a -> int -> 'a t -> 'a t
  val empty : 'a t
  val initialize : 'a t -> int -> 'a -> 'a t

  (* Heap Operations *)
  val heap_get : 'a t -> int -> 'a
  val heap_put : 'a -> int -> 'a t -> 'a t

  (* PDL Operations *)
  val pdl_get : 'a t -> int -> 'a
  val pdl_put : 'a -> int -> 'a t -> 'a t
end

module Make (Layout : Layout) = struct
  module FT = BatFingerTree

  type 'a t = 'a FT.t

  let push = FT.snoc
  let pop = FT.init
  let top = FT.last
  let get = FT.get

  let put (elem : 'a) (index : int) (heap : 'a t) : 'a t =
    FT.set heap index elem

  let empty = FT.empty

  let rec initialize (heap : 'a t) (size : int) (default : 'a) =
    if size = 0 then heap else initialize (push heap default) (size - 1) default

  let heap_get (mem : 'a t) (index : int) : 'a =
    if index >= Layout.max_heap_size then failwith "Heap Max Size Reached!"
    else FT.get mem index

  let heap_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index >= Layout.max_heap_size then failwith "Heap Max Size Reached!"
    else FT.set mem elem index

  let pdl_get (mem : 'a t) (index : int) : 'a =
    if index >= Layout.max_pdl_size then failwith "PDL Max Size Reached!"
    else FT.get mem index

  let pdl_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index >= Layout.max_pdl_size then failwith "PDL Max Size Reached!"
    else FT.set mem elem index
end
