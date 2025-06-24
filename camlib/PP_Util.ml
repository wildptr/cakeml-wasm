
let fprintf = Printf.fprintf

let pp_array_sep sep pp out a =
  let n = Array.length a in
  if n>0 then (
    pp out a.(0);
    for i=1 to n-1 do
      fprintf out "%s%a" sep pp a.(i)
    done
  )

let pp_list_sep sep pp out = function
  | [] -> ()
  | x::rest ->
    pp out x;
    List.iter (fprintf out "%s%a" sep pp) rest

let pp_array pp out a =
  pp_array_sep ", " pp out a

let pp_list pp out a =
  pp_list_sep ", " pp out a

let pp_int_array out a =
  pp_array (fun out -> fprintf out "%d") out a

let pp_int_list out a =
  pp_array (fun out -> fprintf out "%d") out a
