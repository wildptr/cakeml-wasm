
exception Encoding

let get s i = String.unsafe_get s i |> int_of_char

(* returns (j, code) *)
let decode s i =
  let len = String.length s in
  let c = int_of_char s.[i] in
  if c<0x80 then (i+1, c)
  else if c<0xc0 then raise Encoding
  else if c<0xd0 then
    if len<i+2 then raise Encoding
    else (i+2, (c land 31) lsl 6 lor (get s (i+1) land 63))
  else if c<0xf0 then
    if len<i+3 then raise Encoding
    else (i+3, (c land 15) lsl 12 lor (get s (i+1) land 63) lor (get s (i+2) land 63))
  else (
    if len<i+4 then raise Encoding
    else (i+4, (c land 7) lsl 18 lor (get s (i+1) land 63) lor (get s (i+2) land 63) lor (get s (i+3) land 63))
  )
