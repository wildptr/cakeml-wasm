open Util

type sexp =
  | Atom of string
  | List of sexp list
  | String of string

let rec pp_sexp out = function
  | Atom x -> output_string out x
  | List lst ->
    output_char out '(';
    begin match lst with
    | [] -> ()
    | first::rest ->
      pp_sexp out first;
      List.iter (fprintf out " %a" pp_sexp) rest
    end;
    output_char out ')'
  | String s ->
    Lex.output_quoted_string out s


let rec sexp_unirep = function
  | Atom s -> Unirep.Constr(s,[||])
  | List lst -> Unirep.list (map sexp_unirep lst)
  | String s -> Unirep.String s

let is_punct = function
  | '('|')'|'['|']'|'{'|'}' -> true
  | ';' -> true
  | _ -> false

let lex_sexp text i : int(*token*) * int(*start*) * int(*end*) =
  let len = String.length text in

  let rec until_next_line i =
    if i<len then
      if text.[i]='\n' then i
      else until_next_line (i+1)
    else i
  in
  let rec skip_space i =
    if i<len then
      let c = text.[i] in
      if CChar.isspace c then
        skip_space (i+1)
      else if c=';' then
        until_next_line (i+1) |> skip_space
      else i
    else i
  in
  let rec scan_word i =
    if i<len && let c=text.[i] in not (CChar.isspace c || is_punct c) then
      scan_word (i+1)
    else i
  in

  let i = skip_space i in
  if i=len then (0,i,i) else
  let c1 = text.[i] in
  if is_punct c1 then
    (Char.code c1, i, i+1)
  else if c1='"' then
    let j = Lex.scan_string text i in
    (130, i, j)
  else (
    let j = scan_word i in
    (129, i, j)
  )

exception Syntax of int * string

let syntax_error i s = raise (Syntax (i,s))


let rec _parse_sexp text cur i j : sexp * (int*int*int) =
  match cur with
  | 40(*LPAREN*) ->
    let cur,i,j = lex_sexp text j in
    let a = GList.create() in
    let rec loop cur i j =
      match cur with
      | 0 -> (cur,i,j)
      | 41(*RPAREN*) ->
        lex_sexp text j
      | _ ->
        let el, (cur,i,j) = _parse_sexp text cur i j in
        GList.append a el;
        loop cur i j
    in
    let s = loop cur i j in
    (List (GList.finish a), s)

  | 91(*'['*) ->
    let cur,i,j = lex_sexp text j in
    let a = GList.create() in
    let rec loop cur i j =
      match cur with
      | 0 -> (cur,i,j)
      | 93(*']'*) ->
        lex_sexp text j
      | _ ->
        let el, (cur,i,j) = _parse_sexp text cur i j in
        GList.append a el;
        loop cur i j
    in
    let s = loop cur i j in
    (List (GList.finish a), s)

  | 129(*ID*) ->
    (Atom (String.sub text i (j-i)), lex_sexp text j)

  | 130(*STR*) ->
    let s = String.sub text i (j-i) |> Lex.parse_string in
    (String s, lex_sexp text j)

  | _ -> syntax_error i (String.sub text i (j-i))


let parse_sexp text =
  let cur,i,j = lex_sexp text 0 in
  _parse_sexp text cur i j |>fst

let parse_sexp_list text =
  let cur,i,j = lex_sexp text 0 in
  let a = GList.create() in
  let rec loop cur i j =
    match cur with
    | 0 -> ()
    | 40 | 91 | 129(*ID*) | 130(*STR*) ->
      let e,(cur,i,j) = _parse_sexp text cur i j in
      GList.append a e;
      loop cur i j
    | _ -> raise (Syntax (i, String.sub text i (j-i)))
  in
  loop cur i j;
  GList.finish a
