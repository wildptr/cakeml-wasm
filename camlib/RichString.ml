open CChar

(* => x[s/c] *)
let replace_all c s x =
  String.split_on_char c x |> String.concat s

let drop n s =
  String.sub s n (String.length s-n)

let replace_prefix old new_ s =
  if String.starts_with old s then
    let pfx_len = String.length old in
    new_ ^ String.sub s pfx_len (String.length s - pfx_len)
  else s

let tokenize s =
  let len = String.length s in
  let rec skip_space i =
    if i<len && isspace s.[i] then skip_space (i+1) else i
  in

  let a = GList.create() in

  let rec loop i =
    let g i =
      let rec scan i =
        if i<len && not (isspace s.[i]) then scan (i+1) else i
      in
      let j = scan (i+1) in
      GList.append a (String.sub s i (j-i));
      loop j
    in
    let i = skip_space i in
    if i<len then
    match s.[i] with
    | '\''|'"' as q ->
      begin match String.index_from_opt s (i+1) q with
      | None -> (* unterminated *)
        g i
      | Some j ->
        GList.append a (String.sub s (i+1) (j-(i+1)));
        loop (j+1)
      end
    | '#' -> () (* comment *)
    | _ -> g i
  in
  loop 0;

  GList.finish_as_array a

let from_char_list list =
  List.map (String.make 1) list |> String.concat ""

let contains_at s i p =
  let plen = String.length p in
  let rec f j =
    j=plen || String.unsafe_get s (i+j) = String.unsafe_get p j && f (j+1)
  in
  i+plen <= String.length s && f 0

let flat_map f s =
  let a = GList.create() in
  String.iter (fun c -> GList.append a (f c)) s;
  GList.finish a |> String.concat""

let split s =
  let len = String.length s in
  if len=0 then [] else
  begin
    let rec find_word_end i =
      if i<len && not (CChar.isspace (String.unsafe_get s i)) then
        find_word_end (i+1)
      else i
    in
    let rec find_space_end i =
      if i<len && CChar.isspace (String.unsafe_get s i) then
        find_space_end (i+1)
      else i
    in
    let a = GList.create() in
    let rec loop word_start =
      let word_end = find_word_end word_start in
      GList.append a (String.sub s word_start (word_end-word_start));
      if word_end < len then loop (find_space_end word_end)
    in
    loop 0;
    GList.finish a
  end
