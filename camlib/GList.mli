type 'a t

val create: unit -> 'a t
val append: 'a t -> 'a -> unit
val append_list: 'a t -> 'a list -> unit
val finish: 'a t -> 'a list
val finish_as_array: 'a t -> 'a array
val length: 'a t -> int
val clear: 'a t -> unit
val iter: ('a -> unit) -> 'a t -> unit
val iteri: (int -> 'a -> unit) -> 'a t -> unit
val first: 'a t -> 'a
