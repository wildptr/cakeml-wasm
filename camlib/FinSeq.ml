
type 'a fseq =
  | Empty
  | Single of 'a
  | Cons of 'a * 'a fseq * int
  | Snoc of 'a fseq * 'a * int
  | Joined of 'a fseq * 'a fseq * int(*length*)
  | List of 'a list * int
  | Array of 'a array

let length = function
  | Empty -> 0
  | Single _ -> 1
  | Cons (_, _, n) | Snoc (_, _, n) | Joined (_, _, n) | List (_, n) -> n
  | Array a -> Array.length a

let prepend x = function
  | Empty -> Single x
  | Single y -> List ([x;y], 2)
  | List (l, n) -> List (x::l, 1+n)
  | s -> Cons (x, s, 1+length s)

let append x = function
  | Empty -> Single x
  | s -> Snoc (s, x, length s+1)

let snoc s x = append x s

let join a b =
  match a, b with
  | _, Empty -> a
  | Empty, _ -> b
  | _, Single b -> Snoc (a, b, length a+1)
  | Single a, _ -> Cons (a, b, 1+length b)
  | _ -> Joined (a, b, length a + length b)

let empty = Empty
let singleton x = Single x

let iteri f seq =
  let rec aux i rest = function
    | Empty -> fin i rest
    | Single x -> f i x; fin (i+1) rest
    | Cons (x, s, _) -> f i x; aux (i+1) rest s
    | Snoc (s, x, _) ->
      aux i (Single x :: rest) s
    | Joined (a, b, _) ->
      aux i (b::rest) a
    | List (l, n) ->
      List.iteri (fun j x -> f (i+j) x) l;
      fin (i+n) rest
    | Array a ->
      Array.iteri (fun j x -> f (i+j) x) a;
      let n = Array.length a in
      fin (i+n) rest
  and fin i = function
    | [] -> ()
    | s::rest ->
      aux i rest s
  in
  aux 0 [] seq

let to_array = function
  | Empty -> [||]
  | Single x -> [|x|]
  | Array a -> Array.copy a
  | s ->
    let n = length s in
    let arr = Array.make n (Obj.magic ()) in
    iteri (Array.set arr) s;
    arr

let map f s =
  let rec aux ac rest = function
    | Empty -> fin ac rest
    | Single x -> fin (snoc ac (f x)) rest
    | Cons (x, s, _) ->
      aux (snoc ac (f x)) rest s
    | Snoc (s, x, _) ->
      aux ac (Single x :: rest) s
    | Joined (a, b, _) ->
      aux ac (b::rest) a
    | List (l, n) ->
      fin (join ac (List (List.map f l, n))) rest
    | Array a ->
      fin (join ac (Array (Array.map f a))) rest
  and fin ac = function
    | [] -> ac
    | s::rest -> aux ac rest s
  in
  aux Empty [] s

let mapi f s =
  let rec aux i ac rest = function
    | Empty -> fin i ac rest
    | Single x -> fin (i+1) (snoc ac (f i x)) rest
    | Cons (x, s, _) ->
      aux (i+1) (snoc ac (f i x)) rest s
    | Snoc (s, x, _) ->
      aux i ac (Single x :: rest) s
    | Joined (a, b, _) ->
      aux i ac (b::rest) a
    | List (l, n) ->
      fin (i+n) (join ac (List (List.mapi (fun j x -> f (i+j) x) l, n))) rest
    | Array a ->
      let n = Array.length a in
      fin (i+n) (join ac (Array (Array.mapi (fun j x -> f (i+j) x) a))) rest
  and fin i ac = function
    | [] -> ac
    | s::rest -> aux i ac rest s
  in
  aux 0 Empty [] s

let fold_left f init s =
  let rec aux ac rest = function
    | Empty -> fin ac rest
    | Single x -> fin (f ac x) rest
    | Cons (x, s, _) ->
      aux (f ac x) rest s
    | Snoc (s, x, _) ->
      aux ac (Single x :: rest) s
    | Joined (a, b, _) ->
      aux ac (b::rest) a
    | List (l, _) ->
      fin (List.fold_left f ac l) rest
    | Array a ->
      fin (Array.fold_left f ac a) rest
  and fin ac = function
    | [] -> ac
    | s::rest -> aux ac rest s
  in
  aux init [] s

let iter f seq =
  let rec aux rest = function
    | Empty -> fin rest
    | Single x -> f x; fin rest
    | Cons (x, s, _) -> f x; aux rest s
    | Snoc (s, x, _) ->
      aux (Single x :: rest) s
    | Joined (a, b, _) ->
      aux (b::rest) a
    | List (l, _) ->
      List.iter f l;
      fin rest
    | Array a ->
      Array.iter f a;
      fin rest
  and fin = function
    | [] -> ()
    | s::rest ->
      aux rest s
  in
  aux [] seq

let fold_right f s init =
  let rec aux ac rest = function
    | Empty -> fin ac rest
    | Single x -> fin (f x ac) rest
    | Cons (x, s, _) ->
      aux ac (Single x :: rest) s
    | Snoc (s, x, _) ->
      aux (f x ac) rest s
    | Joined (a, b, _) ->
      aux ac (a::rest) b
    | List (l, _) ->
      fin (List.fold_right f l ac) rest
    | Array a ->
      fin (Array.fold_right f a ac) rest
  and fin ac = function
    | [] -> ac
    | s::rest -> aux ac rest s
  in
  aux init [] s

let from_array arr =
  (*Array.fold_left (Fun.flip append) empty arr*)
  if arr=[||] then Empty
  else Array (Array.copy arr)

let from_array_unsafe arr =
  if arr=[||] then Empty
  else Array arr

let from_list list =
  (*List.fold_left (Fun.flip append) empty list*)
  if list=[] then Empty
  else List (list, List.length list)

let to_list = function
  | Empty -> []
  | Single x -> [x]
  | List (l, _) -> l
  | s ->
    fold_right List.cons s []

let concat seq_seq =
  fold_left join empty seq_seq

let concat_list seq_list =
  List.fold_left join empty seq_list

let is_empty seq =
  seq = Empty

let rec first = function
  | Empty -> failwith "empty sequence"
  | Single x | Cons (x, _, _) -> x
  | Snoc (a, _, _) | Joined (a, _, _) -> first a
  | List (l, _) -> List.hd l
  | Array a -> a.(0)

let flat_map f seq =
  let a = GList.create() in
  seq |> iter (fun x -> f x |> iter (GList.append a));
  let n = GList.length a in
  List (GList.finish a, n)
(*
  concat (map f seq)
*)
