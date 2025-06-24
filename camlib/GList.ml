type 'a mut_list = Nil | Cons of { hd: 'a; mutable tl: 'a mut_list }

type 'a t =
{
  mutable last_cons: 'a mut_list;
  mutable list: 'a mut_list;
  mutable length: int
}

let create () =
  let g = { list = Nil; last_cons = Nil; length = 0 } in
  (g.last_cons <- Obj.magic g; g)

let append g x =
  match g.last_cons with
  | Nil -> assert false
  | Cons cons ->
    let new_cons = Cons { hd=x; tl=Nil } in
    cons.tl <- new_cons;
    g.last_cons <- new_cons;
    g.length <- g.length + 1

let append_list g xx =
  List.iter (append g) xx

let finish: 'a t -> 'a list =
  fun {list;_} -> Obj.magic list

let finish_as_array g =
  let n = g.length in
  let a = Array.make n (Obj.magic()) in
  let rec iter i = function
    | Nil -> ()
    | Cons {hd; tl} ->
      a.(i)<-hd;
      iter (i+1) tl
  in
  (iter 0 g.list; a)

let length g = g.length

let clear g =
  g.list <- Nil;
  g.last_cons <- Obj.magic g;
  g.length <- 0

let iter f g =
  let rec iter = function
    | Nil -> ()
    | Cons {hd; tl} ->
      f hd; iter tl
  in
  iter g.list

let iteri f g =
  let rec iter i = function
    | Nil -> ()
    | Cons {hd; tl} ->
      f i hd;
      iter (i+1) tl
  in
  iter 0 g.list

let first g =
  match g.list with
  | Nil -> failwith __FUNCTION__
  | Cons {hd;_} -> hd
