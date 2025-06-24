let fprintf = Printf.fprintf

let pp_array_sep sep pp f a =
  let n = Array.length a in
  if n>0 then (
    pp f a.(0);
    for i=1 to n-1 do
      fprintf f "%s%a" sep pp a.(i)
    done
  )

let pp_list_sep sep pp f = function
  | [] -> ()
  | x::rest ->
    pp f x;
    List.iter (fun x -> fprintf f "%s%a" sep pp x) rest

let pp_array pp f a =
  pp_array_sep ", " pp f a

let pp_list pp f a =
  pp_list_sep ", " pp f a

let output_int f x =
  output_string f (string_of_int x)

let pp_int_array f a =
  pp_array output_int f a

let pp_int_list f a =
  pp_list output_int f a
