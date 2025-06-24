
type layout_body =
  | Text of string
  | Join of layout * layout
  | List of layout list * layout(*separator*)
  | Array of layout array * layout
  | Enclosed of int * (string * string) * layout
  | Indented of int * layout
  | AutoIndented of int * layout
  | Empty
  | Break

and layout = Layout of
{
  body: layout_body;
  br: bool
}

let contains_break (Layout rep) = rep.br

let empty = Layout{body=Empty; br=false}
let break = Layout{body=Break; br=true}
let text x = Layout{body=Text x; br=false}
let join x y = Layout{body=Join(x,y); br=contains_break x||contains_break y}
let list x = Layout{body=List(x,empty); br=List.exists contains_break x}
let array x =
  (* CAUTION: don't modify [x] *)
  Layout{body=Array(x,empty); br=Array.exists contains_break x}
let enclosed n delim x =
  Layout{body=Enclosed (n, delim, x); br=contains_break x}
let indented n x =
  Layout{body=Indented (n, x); br=true}
let auto_indented n x =
  Layout{body=AutoIndented (n, x); br=contains_break x}
let space = text " "
let newline = text "\n"
let semi = text ";"

let write_indented ch lev layout =
  let write_pend lev = function
    | Empty -> ()
    | Text x -> output_string ch x
    | Break ->
      output_char ch '\n';
      output_string ch (String.make lev ' ')
    | _ -> ()
  in
  (* pend:  *)
  let rec write lev (Layout layout) (pend: layout_body) =
    match layout.body with
    | Text x ->
      write_pend lev pend;
      output_string ch x;
      Empty

    | Join (x, y) ->
      pend |> write lev x |> write lev y

    | List ([], _) -> pend
    | List (x::rest, sep) ->
      let pend = write lev x pend in
      let f pend x =
        pend |> write lev sep |> write lev x
      in
      List.fold_left f pend rest

    | Array ([||], _) -> pend
    | Array (l, sep) ->
      let pend = write lev l.(0) pend in
      let n = Array.length l in
      let rec loop i pend =
        if i<n then
          pend |> write lev sep |> write lev l.(i) |> loop (i+1)
        else pend
      in
      loop 1 pend

    | Empty -> pend

    | Break ->
      (* pend ignored *)
      output_char ch '\n';
      output_string ch (String.make lev ' ');
      Empty

    | Enclosed (n, (start, end_), body) ->
      write_pend lev pend;
      output_string ch start;
      let _ = write (lev+n) body Empty in
      output_string ch end_;
      Empty

    | Indented (n, body) ->
      (* pend ignored *)
      output_char ch '\n';
      let lev' = lev+n in
      output_string ch (String.make lev' ' ');
      write lev' body Empty

    | AutoIndented (n, body) ->
      (* pend ignored *)
      if contains_break body then (
        output_char ch '\n';
        let lev' = lev+n in
        output_string ch (String.make lev' ' ');
        let _ = write lev' body Empty in
        Break
      )
      else (
        output_char ch ' ';
        let _ = write lev body Empty in
        Text " "
      )
  in
  write lev layout Empty |>ignore

let write ch layout =
  write_indented ch 0 layout

let writeln ch layout =
  write ch layout; output_char ch '\n'

let sep_list' sep l =
  let br =
    contains_break sep || List.exists contains_break l
  in
  Layout{body=List(l,sep); br}

let sep_array' sep l =
  let br =
    contains_break sep || Array.exists contains_break l
  in
  Layout{body=Array(l,sep); br}

let sep_list sep l =
  sep_list' (text sep) l

let sep_array sep l =
  sep_array' (text sep) l

let parenthesized x =
  enclosed 1 ("(",")") x

let tuple_syntax arr =
  match Array.length arr with
  | 0 -> text "()"
  | 1 -> arr.(0)
  | _ -> sep_array ", " arr |> parenthesized
(*
let pp_layout f layout =
  let open Format in
  let rec pp f = function
    | Text s -> pp_print_string f s
    | Join (a, b) ->
      pp f a; pp f b
    | List list ->
      List.iter (pp f) list
    | Array arr ->
      Array.iter (pp f) arr
    | Enclosed (n, (start, end_), body) ->
      pp_open_vbox f n;
      fprintf f "%s%a%s" start pp body end_;
      pp_close_box f ()
    | Indented (n, body) ->
      let space = String.make n ' ' in
      fprintf f "@ %s@[<v>%a@]" space pp body
    | Empty -> ()
    | Break -> fprintf f "@ "
  in
  fprintf f "@[<v>%a@]" pp layout
*)
