open Sexp

let int_atom n = Atom (string_of_int n)

let i32_const n =
  List [Atom"i32.const"; int_atom n]

let i64_const n =
  List [Atom"i64.const"; int_atom n]

let set_global i =
  List [Atom"global.set"; int_atom i]

let get_global i =
  List [Atom"global.get"; int_atom i]

let wasm_name s =
  Atom("$"^s)

let mk_if (then_part: sexp list) (else_part: sexp list) : sexp =
  let rest =
    if else_part = [] then []
    else [List(Atom"else"::else_part)]
  in
  List (Atom"if" :: List(Atom"then"::then_part) :: rest)

let make_func (name, export_name, body) =
  let lst = List[Atom"result"; Atom"i32"] :: body in
  let lst =
    if export_name="" then lst
    else List[Atom"export"; String export_name] :: lst
  in
  List (Atom"func" :: wasm_name name :: lst)

let is_header = function
  | Atom _ -> true
  | List(Atom("export"|"type"|"param"|"result")::_) -> true
  | _ -> false
let do_indent = ref false

let rec pp_wasm lev out = function
  | Atom x -> output_string out x
  | String s -> Lex.output_quoted_string out s
  | List lst ->
    output_char out '(';
    begin match lst with
    | [] -> ()
    | first::rest ->
      pp_sexp out first;
      let rec f = function
        | [] -> ()
        | x::rest ->
          if is_header x then
          (
            output_char out ' ';
            pp_sexp out x;
            f rest
          )
          else
          (
            List.iter begin fun x ->
              output_char out '\n';
              if !do_indent then
                output_string out (String.make ((lev+1)*2) ' ');
              pp_wasm (lev+1) out x
            end (x::rest)
          )
      in
      f rest
    end;
    output_char out ')';
