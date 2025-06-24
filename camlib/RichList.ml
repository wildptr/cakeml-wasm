let index_opt key list =
  let rec find i = function
    | [] -> None
    | name::rest ->
      if name=key then Some i
      else find (i+1) rest
  in find 0 list

let index key list =
  let rec find i = function
    | [] -> raise Not_found
    | name::rest ->
      if name=key then i
      else find (i+1) rest
  in find 0 list

let index_assoc_opt key list =
  let rec find i = function
    | [] -> None
    | (name, x)::rest ->
      if name=key then Some (i, x)
      else find (i+1) rest
  in find 0 list

let associ key list =
  let rec find i = function
    | [] -> raise Not_found
    | (name, x)::rest ->
      if name=key then (i, x)
      else find (i+1) rest
  in find 0 list

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::rest -> last rest

let filter_mapi f list =
  let rec aux i = function
    | [] -> []
    | x::rest ->
      let rest' = aux (i+1) rest in
      match f i x with
      | Some y -> y::rest'
      | None -> rest'
  in
  aux 0 list

let take n list =
  let rec aux acc n list =
    if n<=0 then FinSeq.to_list acc
    else match list with
      | [] -> FinSeq.to_list acc
      | x::rest ->
        aux (FinSeq.append x acc) (n-1) rest
  in
  aux FinSeq.empty n list

let rec drop n list =
  if n<=0 then list
  else match list with
    | []->[]
    | _::rest -> drop (n-1) rest

let split_at n list =
  let rec aux acc n list =
    if n<=0 then (FinSeq.to_list acc, list)
    else match list with
      | [] -> (FinSeq.to_list acc, [])
      | x::rest ->
        aux (FinSeq.append x acc) (n-1) rest
  in
  aux FinSeq.empty n list

let rec fold f list a =
  match list with
  | [] -> a
  | x::rest -> fold f rest (f x a)

let rec range a b =
  if a >= b then []
  else a :: range (a+1) b
