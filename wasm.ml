open Sexp
open Util

type value_type = I32 | I64 | F32 | F64
type func_type = value_type array * value_type array

type memarg = Memarg of {offset:int}

type br_label =
  | I of int (* unnamed, de Bruijn index *)
  | L of string (* named *)

type named_func_type = string * func_type

let default_nft = ("",([||],[||]))

type inst =
  | Block of string * named_func_type * inst list(*block*)
  | Loop  of string * named_func_type * inst list(*block*)
  | Regular of string
  | If of named_func_type * inst list(*block*) * inst list(*block*)
  | Br of br_label
  | BrIf of br_label
  | BrTable of br_label array * br_label
  | Call of string
  | CallIndirect of named_func_type
  | ReturnCall of string
  | ReturnCallIndirect of named_func_type
  | Return
  | Unreachable
  (* *)
  | LocalGet of int
  | LocalSet of int
  | LocalTee of int
  | GlobalGet of int
  | GlobalSet of int
  | I32Const of int
  | I64Const of int64
  | Memory of string * memarg

(* s-expression syntax *)

let int_atom n = Atom (string_of_int n)

let br_label_to_sexp = function
  | I i -> int_atom i
  | L l -> Atom l

let ap hd tl = List (Atom hd :: tl)

let ft_sexp (name, _) =
  if name="" then []
  else [ap "type" [Atom name]]

let rec inst_to_sexp = function
  | Block (l, ft, b) ->
    ap "block"(Atom l :: ft_sexp ft @ map inst_to_sexp b)
  | Loop  (l, ft, b) ->
    ap "loop" (Atom l :: ft_sexp ft @ map inst_to_sexp b)
  | Regular op -> ap op []
  | If (ft, b1, b2) ->
    ap "if" [
      Atom (fst ft);
      ap "then" (map inst_to_sexp b1);
      ap "else" (map inst_to_sexp b2)
    ]
  | Br l ->
    ap "br" [br_label_to_sexp l]
  | BrIf l ->
    ap "br_if" [br_label_to_sexp l]
  | BrTable (ll, l) ->
    ap "br_table" (map br_label_to_sexp (to_list ll) @ [br_label_to_sexp l])
  | Call fn ->
    ap "call" [Atom fn]
  | CallIndirect ft ->
    ap "call_indirect" (ft_sexp ft)
  | ReturnCall fn ->
    ap "return_call" [Atom fn]
  | ReturnCallIndirect ft ->
    ap "return_call_indirect" (ft_sexp ft)
  | Return ->
    ap "return" []
  | Unreachable ->
    ap "unreachable" []
  | LocalGet i ->
    ap "local.get" [int_atom i]
  | LocalSet i ->
    ap "local.set" [int_atom i]
  | LocalTee i ->
    ap "local.tee" [int_atom i]
  | GlobalGet i ->
    ap "global.get" [int_atom i]
  | GlobalSet i ->
    ap "global.set" [int_atom i]
  | I32Const n ->
    ap "i32.const" [int_atom n]
  | I64Const n ->
    ap "i64.const" [Atom (Int64.to_string n)]
  | Memory (op, Memarg{offset}) ->
    ap op (if offset=0 then [] else [Atom("offset="^string_of_int offset)])

let i32_const n = I32Const n
let i64_const n = I64Const (Int64.of_int n)
let set_global i = GlobalSet i
let get_global i = GlobalGet i

let wasm_name s = "$"^s

(*
let mk_if (then_part: sexp list) (else_part: sexp list) : sexp =
  let rest =
    if else_part = [] then []
    else [List(Atom"else"::else_part)]
  in
  List (Atom"if" :: List(Atom"then"::then_part) :: rest)
*)

let make_func (name, export_name, body) =
  let lst = List[Atom"result"; Atom"i32"] :: body in
  let lst =
    if export_name="" then lst
    else List[Atom"export"; String export_name] :: lst
  in
  List (Atom"func" :: Atom (wasm_name name) :: lst)

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
