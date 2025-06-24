open Util

type exp =
  | Int of int
  | String of string
  | Constr of string * exp array
  | Tuple of exp array
  | List of exp array

let const name = Constr(name,[||])
let tuple lst = Tuple(to_array lst)
let constr name args = Constr (name, to_array args)
let list lst = List (to_array lst)

(* *)

let rec pp_exp out = function
  | Int n ->
    fprintf out "%d" n
  | String s ->
    let b = Buffer.create (String.length s+2) in
    Buffer.add_char b '"';
    String.iter begin fun c ->
      if c>=' ' && c<='~' then
        Buffer.add_char b c
      else match c with
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | _ ->
        sprintf "\\x%02x" (int_of_char c) |> Buffer.add_string b
    end s;
    Buffer.add_char b '"';
    Buffer.output_buffer out b

  | Constr (name, args) ->
    output_string out name;
    if args<>[||] then fprintf out "(%a)" pp_exp_array args
  | Tuple arr ->
    fprintf out "(%a)" pp_exp_array arr
  | List arr ->
    fprintf out "[%a]" pp_exp_array arr

and pp_exp_array out args =
  if args<>[||] then (
    pp_exp out args.(0);
    for i=1 to length args-1 do
      output_char out ',';
      pp_exp out args.(i)
    done
  )

let rec pp_exp_max_depth lev out e =
  if lev>0 then
    match e with
    | Int _ | String _ -> pp_exp out e
    | Constr (name, args) ->
      output_string out name;
      if args<>[||] then fprintf out "(%a)" (pp_exp_array_max_depth (lev-1)) args
    | Tuple arr ->
      fprintf out "(%a)" (pp_exp_array_max_depth (lev-1)) arr
    | List arr ->
      fprintf out "[%a]" (pp_exp_array_max_depth (lev-1)) arr
  else
    output_string out "..."

and pp_exp_array_max_depth lev out args =
  if lev=0 then
    output_string out "..."
  else
  if args<>[||] then (
    pp_exp_max_depth lev out args.(0);
    for i=1 to length args-1 do
      output_char out ',';
      pp_exp_max_depth lev out args.(i)
    done
  )
