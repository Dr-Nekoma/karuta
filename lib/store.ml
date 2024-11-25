module type Layout = sig
  val heap_size : int
  val stack_size : int
  val trail_pdl_size : int
end

module type Memory = sig
  type 'a t

  (* Store Operations *)
  val to_list : 'a t -> 'a list
  val empty : 'a t
  val initialize : 'a t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a

  (* Heap Operations *)
  val heap_get : 'a t -> int -> 'a
  val heap_put : 'a -> int -> 'a t -> 'a t

  (* PDL Operations *)
  val pdl_push : 'a -> 'a t -> 'a t
  val pdl_pop : 'a t -> 'a
  val pdl_top : 'a t -> 'a
  val pdl_empty : 'a t -> bool
end

module Make (Layout : Layout) = struct
  module FT = BatFingerTree

  type 'a t = 'a FT.t

  let to_list = FT.to_list
  let get = FT.get
  let set = FT.set
  let mem_size = Layout.heap_size + Layout.stack_size + Layout.trail_pdl_size

  let put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    if index <= 0 then
      failwith "Tried reaching illegal memory region! Ceiling reached: x!"
    else set mem index elem

  let limited_get (floor : int) (size : int) (mem : 'a t) (index : int) : 'a =
    if index >= floor + size || index < floor then
      failwith
        "Tried reaching illegal memory region! Ceiling or floor reached: x!"
    else get mem index

  let limited_put (floor : int) (size : int) (elem : 'a) (index : int)
      (mem : 'a t) : 'a t =
    if index >= floor + size || index < floor then
      failwith
        "Tried reaching illegal memory region! Ceiling or floor reached: x!"
    else put elem index mem

  let empty = FT.empty
  let push = FT.snoc

  let rec initialize (heap : 'a t) (size : int) (default : 'a) =
    if size = 0 then heap else initialize (push heap default) (size - 1) default

  let heap_start = 0

  let heap_get (mem : 'a t) (index : int) : 'a =
    limited_get heap_start Layout.heap_size mem index

  let heap_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    limited_put heap_start Layout.heap_size elem index mem

  let pdl_tracker = ref (mem_size - 1)
  let stack_start = heap_start + Layout.heap_size
  let trail_start = stack_start + Layout.stack_size

  (* TODO: Add top of trail as an argument to be checked*)
  let pdl_push (elem : 'a) (mem : 'a t) : 'a t =
    if !pdl_tracker < trail_start then failwith "Stack is full!"
    else pdl_tracker := !pdl_tracker - 1;
    put elem (!pdl_tracker + 1) mem

  let pdl_empty (_ : 'a t) : bool = !pdl_tracker == mem_size - 1

  let pdl_pop (mem : 'a t) : 'a t =
    if pdl_empty mem then failwith "Stack is empty!"
    else pdl_tracker := !pdl_tracker + 1;
    mem

  let pdl_top (mem : 'a t) : 'a =
    if pdl_empty mem then failwith "Stack is empty!" else get mem !pdl_tracker
end
