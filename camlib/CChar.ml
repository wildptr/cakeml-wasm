let isspace = function
  | ' '|'\t'|'\n'|'\r'|'\011'|'\012'  -> true
  | _ -> false

let isupper = function
  | 'A'..'Z' -> true
  | _ -> false

let islower = function
  | 'a'..'z' -> true
  | _ -> false

let isalpha c =
  let c = int_of_char c lor 32 in
  c>=97(*'a'*)&&c<=122(*'z'*)

let isdigit = function
  | '0'..'9' -> true
  | _ -> false

let isxdigit = function
  | '0'..'9' -> true
  | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

let isalnum c = isdigit c || isalpha c

let is_printable c =
  let c = Char.code c in
  c>=32 && c<=126
