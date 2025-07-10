module type Layout = sig
  val code_size : int
  val heap_size : int
  val stack_size : int
  val pdl_size : int
  val trail_size : int
end

module type Memory = sig
  type 'a t

  module Layout : Layout

  (* Store Operations *)
  val window : int -> int -> 'a t -> 'a t option
  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val to_list : 'a t -> 'a list
  val empty : 'a t
  val initialize : 'a t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val put : 'a -> int -> 'a t -> 'a t
  val heap_start : int
  val stack_start : int
  val pdl_start : int
  val trail_start : int
  val mem_size : int

  (* Code Operations *)
  val code_get : 'a t -> int -> 'a
  val code_put : 'a -> int -> 'a t -> 'a t

  (* Heap Operations *)
  val heap_get : 'a t -> int -> 'a
  val heap_put : 'a -> int -> 'a t -> 'a t

  (* Stack Operations *)
  val stack_get : 'a t -> int -> 'a
  val stack_put : 'a -> int -> 'a t -> 'a t

  (* Trail Operations *)
  val trail_get : 'a t -> int -> 'a
  val trail_put : 'a -> int -> 'a t -> 'a t

  (* PDL Operations *)
  val pdl_push : 'a -> 'a t -> 'a t
  val pdl_pop : 'a t -> 'a * 'a t
  val pdl_empty : 'a t -> bool
end

module Make (Layout : Layout) : Memory = struct
  module FT = BatFingerTree
  module Layout = Layout

  type 'a t = 'a FT.t

  let window (start : int) (end_ : int) (mem : 'a t) : 'a t option =
    (* TODO: Actually do exception handling, don't duplicate the bounds check *)
    if start < 0 || FT.size mem = 0 then None
    else if end_ < start || FT.size mem < end_ then None
    else
      let _, tail = FT.split_at mem start in
      let middle, _ = FT.split_at tail (end_ - start) in
      Some middle

  let fold_left = FT.fold_left
  let to_list = FT.to_list
  let get = FT.get
  let set = FT.set

  let mem_size =
    Layout.code_size + Layout.heap_size + Layout.stack_size + Layout.pdl_size
    + Layout.trail_size

  let put (elem : 'a) (index : int) (mem : 'a t) : 'a t = set mem index elem

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

  let code_get (mem : 'a t) (index : int) : 'a =
    limited_get 0 Layout.code_size mem index

  let code_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    limited_put 0 Layout.code_size elem index mem

  let heap_start = Layout.code_size
  let stack_start = heap_start + Layout.heap_size
  let pdl_start = stack_start + Layout.stack_size
  let trail_start = pdl_start + Layout.pdl_size
  let pdl_tracker = ref pdl_start

  let heap_get (mem : 'a t) (index : int) : 'a =
    limited_get heap_start Layout.heap_size mem index

  let heap_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    limited_put heap_start Layout.heap_size elem index mem

  let stack_get (mem : 'a t) (index : int) : 'a =
    limited_get stack_start Layout.stack_size mem index

  let stack_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    limited_put stack_start Layout.stack_size elem index mem

  let trail_get (mem : 'a t) (index : int) : 'a =
    limited_get trail_start Layout.trail_size mem index

  let trail_put (elem : 'a) (index : int) (mem : 'a t) : 'a t =
    limited_put trail_start Layout.trail_size elem index mem

  let pdl_push (elem : 'a) (mem : 'a t) : 'a t =
    if trail_start <= !pdl_tracker then failwith "Stack is full!"
    else pdl_tracker := !pdl_tracker + 1;
    put elem (!pdl_tracker - 1) mem

  let pdl_empty (_ : 'a t) : bool = !pdl_tracker = pdl_start

  let pdl_pop (mem : 'a t) : 'a * 'a t =
    if pdl_empty mem then failwith "Stack is empty!"
    else
      let element = get mem (!pdl_tracker - 1) in
      pdl_tracker := !pdl_tracker - 1;
      (element, mem)
end
