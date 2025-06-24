open Prog
open Sexp
open Util
open Wasm

let use_tail_calls = ref false

(* func name => id *)
let func_table: int STable.t
  = STable.create 0x1000
(*
(* [(group, group_local_index)] *)
let func_info: (int*int) array ref
  = ref [||]
*)

let numeric_label_id label =
(*
  let rec find i =
    if i>0 && CChar.isdigit label.[i-1] then find (i-1)
    else i
  in
  let len = String.length label in
  let i = find len in
  String.sub label i (len-i) |> int_of_string
*)
  STable.find func_table label

(*
let analyze_tail_calls (func_list: (string * prog) array) =
  let num_func = length func_list in
  let parent = Array.init num_func Fun.id in
  let group_size = Array.make num_func 1 in

  let rec find x =
    if parent.(x) = x then x
    else (
      let p = find parent.(x) in
      parent.(x) <- p; p
    )
  in
  let union x y =
    let x = find x in
    let y = find y in
    if x <> y then
    begin
      let size_x = group_size.(x) in
      let size_y = group_size.(y) in
      if size_x > size_y then
      (
        (* y is smaller *)
        parent.(y) <- x;
        group_size.(x) <- size_x + size_y
      )
      else (
        (* x is smaller *)
        parent.(y) <- x;
        group_size.(x) <- size_x + size_y
      )
    end
  in

  func_list |> Array.iteri begin fun func_index (func_name, func_body) ->
    let rec f = function
      | Seq pp -> f pp.(length pp-1)
      | If (_,p1,p2) ->
        f p1; f p2
      | FancyCall (Loc (l,_), Skip, Skip) ->
        union func_index (numeric_label_id l)
      | _->()
    in
    f func_body
  end;

  let ctr = Array.make num_func 0 in
  Array.init num_func begin fun i ->
    let group = find i in
    let group_local_index = ctr.(group) in
    ctr.(group) <- group_local_index + 1;
    (group, group_local_index)
  end
*)


let same_reg a b =
  match a, b with
  | Reg i, Reg j when i=j -> true
  | _->false

let dest_addr addr : exp * int =
  match addr with
  | Op (Bin Add, [|base; Const offset|]) when offset>=0 ->
    (base, offset)
  | _ -> (addr, 0)


let rec comp_exp e c : sexp list =
  match e with
  | Const n -> i64_const n :: c
  | Reg i -> get_global i :: c
  | Loc (l, _) -> i64_const (numeric_label_id l*2) :: c
  | Op (op, ss) -> comp_op (op, ss) c

and comp_op (op, ss) c =
  let extern func_name =
    (List[Atom"call"; wasm_name func_name] :: c)
    |> Array.fold_right comp_exp ss
  in
  match op with
  | Bin op ->
    let g() =
      let op_name =
        match op with
        | Add -> "add" | Sub -> "sub"
        | And -> "and" | Or -> "or" | Xor -> "xor"
      in
      comp_exp ss.(0) (comp_exp ss.(1) (List[Atom("i64."^op_name)]::c))
    in
    begin match op with
    | Or | And ->
      if same_reg ss.(0) ss.(1) then comp_exp ss.(0) c
      else g()
    | _->g()
    end

  | Shift op ->
    let op_name =
      match op with
      | Lsl -> "shl" | Lsr -> "shr_u" | Asr -> "shr_s" | Ror -> "rotr"
    in
    comp_exp ss.(0) (comp_exp ss.(1) (List[Atom("i64."^op_name)]::c))

  | Rel op ->
    let g c = comp_exp ss.(0) (comp_exp ss.(1) c) in
    let f op_name = g (List[Atom("i64."^op_name)]::c) in
    begin match op with
    | Equal -> f "eq" | NotEqual -> f "ne"
    | Less -> f "lt_s" | NotLess -> f "ge_s"
    | Lower -> f "lt_u" | NotLower -> f "ge_u"
    | NotTest ->
      g (
        List[Atom"i64.and"] ::
        i64_const 0 ::
        List[Atom"i64.ne"] ::
        c
      )
    | Test ->
      g (
        List[Atom"i64.and"] ::
        List[Atom"i64.eqz"] ::
        c
      )
    end

  | Load size ->
    let op_name =
      if size>=8 then "i64.load"
      else sprintf "i64.load%d_u" (size*8)
    in
    let base, offset = dest_addr ss.(0) in
    let op_sexp =
      if offset=0 then
        List[Atom op_name]
      else
        List[Atom op_name; Atom("offset="^string_of_int offset)]
    in
    comp_exp base (List[Atom"i32.wrap_i64"]::op_sexp::c)

  | Store size ->
    let op_name =
      if size>=8 then "i64.store"
      else sprintf "i64.store%d" (size*8)
    in
    let base, offset = dest_addr ss.(0) in
    let op_sexp =
      if offset=0 then
        List[Atom op_name]
      else
        List[Atom op_name; Atom("offset="^string_of_int offset)]
    in
    comp_exp base (List[Atom"i32.wrap_i64"] :: comp_exp ss.(1) (op_sexp::c))

  | AddCarry -> extern "add_carry"
  | AddOverflow -> extern "add_overflow"
  | SubOverflow -> extern "sub_overflow"
  | LongMul -> extern "long_mul"
  | LongDiv -> extern "long_div"

let comp_cond e c =
  let c' =
    match e with
    | Op (op, _) ->
      begin match op with
      | Bin _ | Shift _ | Load _ ->
        i64_const 0 ::
        List[Atom"i64.ne"] ::
        c
      | Rel _ -> c
      | _ -> assert false
      end
    | _ ->
      i64_const 0 ::
      List[Atom"i64.ne"] ::
      c
  in
  comp_exp e c'

let comp_call tail dest lr ret exn c =
  let g() =
    List[Atom"global.set"; int_atom lr; i64_const 0] ::
    if !use_tail_calls then
      begin match dest with
      | Loc (l,_) ->
        List [Atom"call"; wasm_name l] ::
        mk_if exn ret :: c
      | _ -> (* indirect call *)
        List [Atom"i32.wrap_i64"] :: i32_const 1 :: List[Atom"i32.shr_u"] ::
        List [Atom"call_indirect"; List[Atom"type"; Atom"$Ftype"]] ::
        mk_if exn ret :: c
        |> comp_exp dest
      end
    else (
      begin match dest with
      | Loc (l,_) ->
        List [Atom"call"; Atom"$cakeml"; i32_const (STable.find func_table l)] ::
        mk_if exn ret :: c
      | _ -> (* indirect call *)
        List [Atom"i32.wrap_i64"] :: i32_const 1 :: List[Atom"i32.shr_u"] ::
        List [Atom"call"; Atom"$cakeml"] ::
        mk_if exn ret :: c
        |> comp_exp dest
      end
    )
  in
  if tail then (* make this a tail-call *)
    if !use_tail_calls then
      begin match dest with
      | Loc (l,_) ->
        [List[Atom"return_call"; wasm_name l]]
      | _ -> (* indirect call *)
        [
          List [Atom"i32.wrap_i64"]; i32_const 1; List[Atom"i32.shr_u"];
          List[Atom"return_call_indirect"; List[Atom"type"; Atom"$Ftype"]]
        ]
        |> comp_exp dest
      end
    else begin
      match dest with
      | Loc (l,_) ->
        let i = STable.find func_table l in
        [List[Atom"br"; Atom"$DISPATCH"; i32_const i]]
      | _ -> g()
    end
  else g()

let rec comp tail p c : sexp list =
  match p with
  | Jump (JReturn, _) ->
    [i32_const 0; List[Atom"return"]]

  | Jump (JRaise, _) ->
    [i32_const 1; List[Atom"return"]]

  | Jump (JRawCall, _) -> (*XXX*)
    [Atom"(;raw_call;)"]

  | TailCall dest ->
    comp_call true dest 0 [] [] c

  | FancyCall (dest, lr, ret_handler, exn_handler) ->
    let ret = comp tail ret_handler [] in
    let exn = comp tail exn_handler [] in
    comp_call tail dest lr ret exn c

  | Skip -> c

  | Assign (t, e) ->
    comp_exp e (set_global t :: c)

  | OpInst (op, tt, ss) ->
    (* last t on top of stack *)
    RichArray.fold (fun t c -> set_global t :: c) tt c
    |> comp_op (op, ss)

  | Halt _ ->
    [ i64_const 2; List[Atom"call"; Atom"$rt_exit"]; List[Atom"unreachable"] ]

  | Seq pp ->
    let rec comp_list pp c =
      match pp with
      | [] -> c
      | [p] -> comp tail p c
      | p::pp -> comp false p (comp_list pp c)
    in
    comp_list (to_list pp) c
    (*Array.fold_right comp pp c*)

  | If (cond, p1, p2) ->
    mk_if (comp tail p1 []) (comp tail p2 []) :: c
    |> comp_cond cond

  | While (cond, p1) ->
    let body =
      comp_cond cond [mk_if (comp false p1 [List[Atom"br"; int_atom 1]]) []]
    in
    List(Atom"loop"::body) :: c

  | FFI (entry, ss) ->
    List[Atom"call"; wasm_name ("ffi_"^entry)] :: c
    |> Array.fold_right comp_exp ss

  | Unknown s -> List[Atom";"; s; Atom";"] :: c

let comp_func body =
  comp true body [List[Atom"return"; i32_const 0]]

let compile_program_without_tail_calls functions =
(*
  let my_func_info = analyze_tail_calls functions in
  func_info := my_func_info;
  let groups: (string * sexp list) GList.t array =
    Array.init (length functions) (fun _ -> GList.create()) in
  functions |> Array.iteri begin fun i (func_name, func_body) ->
    let group, _ = my_func_info.(i) in
    GList.append groups.(group) (func_name, comp_func func_body)
  end;
*)
  functions |> Array.iteri begin fun i (func_name, _) ->
    STable.add func_table func_name i
  end;
  let compiled_functions = amap (apsnd comp_func) functions in
  begin
    let nb = length functions in
    if nb>0 then
    let dispatch_loop_body =
      let br_table =
        let a = GList.create() in
        GList.append a (Atom"br_table");
        compiled_functions |> Array.iter begin fun (func_name, _) ->
          GList.append a (wasm_name func_name)
        end;
        GList.append a (Atom"$DEFAULT");
        List (GList.finish a)
      in
      let a =
        Array.fold_right begin fun (func_name, func_body) a ->
          List(Atom"block" :: wasm_name func_name :: List[Atom"type"; Atom"$Btype"] :: a) :: func_body
        end compiled_functions [br_table]
      in
      List(Atom"block" :: Atom"$DEFAULT" :: List[Atom"type"; Atom"$Btype"] :: a)
    in
    let func_body =
      [
        List[Atom"local.get"; Atom"$entry"];
        List[Atom"loop"; Atom"$DISPATCH"; List[Atom"type"; Atom"$Btype"]; dispatch_loop_body];
        i32_const 0;
        List[Atom"return"]
      ]
    in
    List (
      Atom"func" ::
      Atom"$cakeml" ::
      List[Atom"param"; wasm_name "entry"; Atom"i32"] ::
      List[Atom"result"; Atom"i32"] ::
      func_body
    )
    |> printf "\n%a\n" (Wasm.pp_wasm 0)
  end
