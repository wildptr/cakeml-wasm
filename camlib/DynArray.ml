type 'a t =
{
  mutable len: int;
  mutable arr: 'a array
}

let length {len;_} = len

let get {len; arr} i =
  if i>=len then invalid_arg __FUNCTION__;
  arr.(i)

let add da x =
  let len = da.len in
  if len = Array.length da.arr then
  (
    let new_len = Array.length da.arr * 2 in
    let new_arr = Array.make new_len (Obj.magic()) in
    Array.blit da.arr 0 new_arr 0 len;
    da.arr <- new_arr
  );
  da.len <- len+1;
  Array.unsafe_set da.arr len x

let to_list da =
  let a = GList.create() in
  for i=0 to da.len-1 do
    Array.unsafe_get da.arr i |> GList.append a
  done;
  GList.finish a

let of_array src_arr =
  let len = Array.length src_arr in
  let arr = Array.make (max 8 len) (Obj.magic()) in
  Array.blit src_arr 0 arr 0 len;
  {len; arr}

let for_all f {len; arr} =
  let rec aux i =
    if i<len then f arr.(i) && aux (i+1)
    else true
  in aux 0

let create() =
  let arr = Array.make 8 (Obj.magic()) in
  {len=0; arr}

let to_array {len; arr} =
  Array.sub arr 0 len

let iter f {len; arr} =
  for i=0 to len-1 do f arr.(i) done
