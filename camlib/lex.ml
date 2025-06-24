
let get = String.unsafe_get

(* input.[pos] = '"' *)
let scan_string input pos =
  let delim = input.[pos] in
  let len = String.length input in
  let rec loop i =
    if i<len then
      let c = get input i in
      match c with
      | '\\' ->
        if i+1<len then
          match get input (i+1) with
          | '\\'|'"'|'\'' -> loop (i+2)
          | _ -> loop (i+1)
        else len
      | '\n' -> (* unterminated string literal *)
        i+1
      | _ ->
        if c=delim then i+1
        else loop (i+1)
    else i
  in
  loop (pos+1)

let parse_char_at s i =
  let c = s.[i] in
  if c = '\\' then
    match s.[i+1] with
    | 'a' -> ('\007', i+2)
    | 'b' -> ('\008', i+2)
    | 't' -> ('\009', i+2)
    | 'n' -> ('\010', i+2)
    | 'v' -> ('\011', i+2)
    | 'f' -> ('\012', i+2)
    | 'r' -> ('\013', i+2)
    | '"' -> ('\034', i+2)
    | '\''-> ('\039', i+2)
    | '\\'-> ('\092', i+2)
(*
    | '0'..'9' ->
      (int_of_string (String.sub s (i+1) 3) |>Char.chr, i+4)
*)
    | 'x' ->
      (int_of_string ("0"^String.sub s (i+1) 3) |>Char.chr, i+4)
    | c -> (c, i+2)
  else (c, i+1)

let parse_string s =
  let buf = Buffer.create (String.length s) in
  let delim = s.[0] in
  let rec parse i =
    if s.[i] = delim then ()
    else (
      let c, j = parse_char_at s i in
      Buffer.add_char buf c;
      parse j
    )
  in
  parse 1;
  Buffer.contents buf

let output_quoted_string out s =
  output_char out '"';
  s |> String.iter begin fun c ->
    match c with
    | '"' | '\\' ->
      Printf.fprintf out "\\%c" c
    | _ ->
    if c>=' ' && c<='~' then
      output_char out c
    else (
      Printf.fprintf out "\\x%02x" (Char.code c)
    )
  end;
  output_char out '"'

let output_single_quoted_string out s =
  output_char out '\'';
  s |> String.iter begin fun c ->
    match c with
    | '\'' | '\\' ->
      Printf.fprintf out "\\%c" c
    | _ ->
    if c>=' ' && c<='~' then
      output_char out c
    else (
      Printf.fprintf out "\\x%02x" (Char.code c)
    )
  end;
  output_char out '\''

let read_integer s i =
  let len = String.length s in
  let rec loop i a =
    if i<len then
      let c = s.[i] in
      if CChar.isdigit c then
        loop (i+1) (a*10+(int_of_char(c)-48))
      else (a,i)
    else (a,i)
  in loop i 0
