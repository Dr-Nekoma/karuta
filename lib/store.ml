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
    FT.set mem index elem

  let limited_get (ceiling: int) (mem: 'a t) (index: int): 'a =
    if index >= ceiling
    then failwith "Tried reaching illegal memory region! Ceiling reached: x!"
    else get mem index
      
  let limited_put (ceiling: int) (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index >= ceiling
    then failwith "Tried reaching illegal memory region! Ceiling reached: x!"
    else put elem index mem       

  let empty = FT.empty

  let rec initialize (mem : 'a t) (size : int) (default : 'a) =
    if size = 0 then mem else initialize (push mem default) (size - 1) default

  let heap_get = limited_get Layout.max_heap_size 

  let heap_put = limited_put Layout.max_heap_size 

  let pdl_get = limited_get Layout.max_pdl_size

  let pdl_put = limited_put Layout.max_pdl_size

end
