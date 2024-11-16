module type Layout = sig
  val heap_start : int
  val pdl_start : int
  val max_heap_size : int
  val max_pdl_size : int
end

module type Memory = sig
  type 'a t

  (* Store Operations *)
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a
  val top : 'a t -> 'a
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

  let put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index <= 0 then failwith "Tried reaching illegal memory region! Ceiling reached: x!"
    else FT.set mem index elem

  let limited_get (floor: int) (size: int) (mem: 'a t) (index: int): 'a =
    if index >= (floor + size) || index < floor
    then failwith "Tried reaching illegal memory region! Ceiling or floor reached: x!"
    else get mem index
      
  let limited_put (floor: int) (size: int) (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index >= (floor + size) || index < floor
    then failwith "Tried reaching illegal memory region! Ceiling or floor reached: x!"
    else put elem index mem       

  let empty = FT.empty

  let rec initialize (heap : 'a t) (size : int) (default : 'a) =
    if size = 0 then heap else initialize (push heap default) (size - 1) default

  let heap_get (mem: 'a t) (index: int): 'a = limited_get Layout.heap_start Layout.max_heap_size mem index

  let heap_put (elem: 'a) (index: int) (mem: 'a t): 'a t = limited_put Layout.heap_start Layout.max_heap_size elem index mem

  let pdl_get (mem: 'a t) (index: int): 'a = limited_get Layout.pdl_start Layout.max_pdl_size mem index

  let pdl_put (elem: 'a) (index: int) (mem: 'a t): 'a t = limited_put Layout.pdl_start Layout.max_pdl_size elem index mem

end
