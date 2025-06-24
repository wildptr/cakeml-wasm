type 'a fseq

val length: 'a fseq -> int
val prepend: 'a -> 'a fseq -> 'a fseq
val append: 'a -> 'a fseq -> 'a fseq
val snoc: 'a fseq -> 'a -> 'a fseq
val join: 'a fseq -> 'a fseq -> 'a fseq
val empty: 'a fseq
val is_empty: 'a fseq -> bool
val singleton: 'a -> 'a fseq
val to_array: 'a fseq -> 'a array
val from_array: 'a array -> 'a fseq
val from_array_unsafe: 'a array -> 'a fseq
val from_list: 'a list -> 'a fseq
val to_list: 'a fseq -> 'a list
val map: ('a -> 'b) -> 'a fseq -> 'b fseq
val mapi: (int -> 'a -> 'b) -> 'a fseq -> 'b fseq
val iter: ('a -> unit) -> 'a fseq -> unit
val iteri: (int -> 'a -> unit) -> 'a fseq -> unit
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b fseq -> 'a
val fold_right: ('a -> 'b -> 'b) -> 'a fseq -> 'b -> 'b
val concat: 'a fseq fseq -> 'a fseq
val concat_list: 'a fseq list -> 'a fseq
val first: 'a fseq -> 'a
val flat_map: ('a -> 'b fseq) -> 'a fseq -> 'b fseq
