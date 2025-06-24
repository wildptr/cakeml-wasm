let inplace_map f arr =
  let n = Array.length arr in
  for i=0 to n-1 do
    arr.(i) <- f arr.(i)
  done

let join a b =
  let alen = Array.length a in
  let result = Array.make (alen + Array.length b) (Obj.magic ()) in
  Array.blit a 0 result 0 alen;
  Array.blit b 0 result alen (Array.length b);
  result

let filter_mapi_list f arr =
  let acc = GList.create () in
  for i=0 to Array.length arr - 1 do
    match f i arr.(i) with
    | Some y ->
      GList.append acc y
    | None -> ()
  done;
  GList.finish acc

let map_list f arr =
  let acc = GList.create () in
  arr |> Array.iter (fun x -> GList.append acc (f x));
  GList.finish acc

let filter_map_list f arr =
  let acc = GList.create () in
  arr |> Array.iter begin fun x ->
    match f x with
    | Some y ->
      GList.append acc y
    | None -> ()
  end;
  GList.finish acc

let filter_map f arr =
  filter_map_list f arr |> Array.of_list

let filter_mapi f arr =
  filter_mapi_list f arr |> Array.of_list

let filter f arr =
  let acc = GList.create () in
  for i=0 to Array.length arr - 1 do
    let x = arr.(i) in
    if f x then
      GList.append acc x
  done;
  GList.finish_as_array acc

let filteri f arr =
  let acc = GList.create () in
  for i=0 to Array.length arr - 1 do
    let x = arr.(i) in
    if f i x then
      GList.append acc x
  done;
  GList.finish acc |> Array.of_list

let for_alli f arr =
  let n = Array.length arr in
  let rec test i =
    i=n || f i arr.(i) && test (i+1)
  in
  test 0

(* map preserving physical identity *)
let map' f arr =
  let n = Array.length arr in
  let rec iter i =
    if i<n then (
      let x = arr.(i) in
      let x' = f x in
      if x == x' then iter (i+1)
      else (
        let arr' = Array.copy arr in
        arr'.(i) <- x';
        for j=i+1 to n-1 do
          arr'.(j) <- f arr.(j)
        done;
        arr'
      )
    )
    else arr
  in
  iter 0

let fold f arr a =
  Array.fold_left (Fun.flip f) a arr

let foldi f arr a =
  let n = Array.length arr in
  let rec fold i a =
    if i=n then a
    else fold (i+1) (f i arr.(i) a)
  in
  fold 0 a

let forall_in_range f arr start end_ =
  let rec test i =
    i>=end_ || f arr.(i) && test (i+1)
  in
  test start

let exists_in_range f arr start end_ =
  let rec test i =
    i<end_ && (f arr.(i) || test (i+1))
  in
  test start

let index key arr =
  let n = Array.length arr in
  let rec find i =
    if i<n then
      if fst arr.(i) = key then i
      else find (i+1)
    else raise Not_found
  in find 0

let assoc key arr =
  let n = Array.length arr in
  let rec find i =
    if i<n then
      let x=arr.(i) in
      if fst x = key then snd x
      else find (i+1)
    else raise Not_found
  in find 0

let assoc_opt key arr =
  let n = Array.length arr in
  let rec find i =
    if i<n then
      let x=arr.(i) in
      if fst x = key then Some (snd x)
      else find (i+1)
    else None
  in find 0

let find f arr =
  let n = Array.length arr in
  let rec find i =
    if i<n then
      let x=arr.(i) in
      if f x then x else find (i+1)
    else raise Not_found
  in find 0

let count f arr =
  Array.fold_left (fun n x -> if f x then n+1 else n) 0 arr

let to_list_rev arr =
  Array.fold_left (fun a x -> x::a) [] arr

let argmax f arr =
  let n = Array.length arr in
  if n=0 then invalid_arg "RichArray.argmax";
  let rec loop max maxi i =
    if i<n then
      let y = f arr.(i) in
      if y>max then loop y i (i+1)
      else loop max maxi (i+1)
    else maxi
  in
  loop (f arr.(0)) 0 1

let fold_map f a arr =
  let n = Array.length arr in
  let arr' = Array.make n (Obj.magic()) in
  let rec loop i a =
    if i=n then a
    else (
      let a', y = f a arr.(i) in
      arr'.(i) <- y;
      loop (i+1) a'
    )
  in
  let a' = loop 0 a in
  (a', arr')
