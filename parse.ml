open Sexp
open Prog
open Util

exception Unknown_Form

let parse_operand = function
  | List[Atom"Reg"; Atom r] ->
    Reg (int_of_string r)
  | List[Atom"Imm"; Atom n] ->
    Const (Int64.of_string n |> Int64.to_int)
  | _ -> raise Unknown_Form

let parse_addr = function
  | List[Atom"Addr"; Atom base; Atom offset] ->
    let base = Reg (int_of_string base) in
    let offset = Int64.of_string offset |> Int64.to_int in
    if offset=0 then base else Op (Bin Add, [|base; Const offset|])
  | _ -> raise Unknown_Form

let parse_cond = function
  | List[Atom op; Atom s1; s2] ->
    let op = match op with
      | "Equal" -> Equal
      | "NotEqual" -> NotEqual
      | "Lower" -> Lower
      | "NotLower" -> NotLower
      | "Less" -> Less
      | "NotLess" -> NotLess
      | "Test" -> Test
      | "NotTest" -> NotTest
      | _ -> raise Unknown_Form
    in
    let s1 = Reg (int_of_string s1) in
    let s2 = parse_operand s2 in
    Op (Rel op, [|s1; s2|])
  | _ -> raise Unknown_Form

let rec parse_inst sexp =
  try
  match sexp with
  | Atom a ->
    begin match a with
    | "skip" -> Skip
    | _ -> raise Unknown_Form
    end
  | String _ -> raise Unknown_Form
  | List lst ->
    begin match lst with
    | [Atom"Arith"; List arith] ->
      begin match arith with
      | [Atom"Binop"; Atom binop; Atom t; Atom s1; s2] ->
        let binop = match binop with
          | "Add" -> Add
          | "Sub" -> Sub
          | "Or" -> Or
          | "And" -> And
          | "Xor" -> Xor
          | _ -> raise Unknown_Form
        in
        let t = int_of_string t in
        let s1 = Reg (int_of_string s1) in
        let s2 = parse_operand s2 in
        Assign (t, Op (Bin binop, [|s1; s2|]))

      | [Atom"Shift"; Atom shift; Atom t; Atom s1; Atom s2] ->
        let shift = match shift with
          | "Lsl" -> Lsl
          | "Lsr" -> Lsr
          | "Asr" -> Asr
          | "Ror" -> Ror
          | _ -> raise Unknown_Form
        in
        let t = int_of_string t in
        let s1 = Reg (int_of_string s1) in
        let s2 = Const (int_of_string s2) in
        Assign (t, Op (Shift shift, [|s1; s2|]))

      | [Atom"AddCarry"; Atom r1; Atom r2; Atom r3; Atom r4] ->
        let r1 = int_of_string r1 in
        let r2 = int_of_string r2 in
        let r3 = int_of_string r3 in
        let r4 = int_of_string r4 in
        OpInst (AddCarry, [|r1; r4|], [|Reg r2; Reg r3; Reg r4|])

      | [Atom"AddOverflow"; Atom t1; Atom s1; Atom s2; Atom t2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = Reg (int_of_string s1) in
        let s2 = Reg (int_of_string s2) in
        OpInst (AddOverflow, [|t1; t2|], [|s1; s2|])

      | [Atom"SubOverflow"; Atom t1; Atom s1; Atom s2; Atom t2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = Reg (int_of_string s1) in
        let s2 = Reg (int_of_string s2) in
        OpInst (SubOverflow, [|t1; t2|], [|s1; s2|])

      | [Atom"LongMul"; Atom t1; Atom t2; Atom s1; Atom s2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = Reg (int_of_string s1) in
        let s2 = Reg (int_of_string s2) in
        OpInst (LongMul, [|t1; t2|], [|s1; s2|])

      | [Atom"LongDiv"; Atom t1; Atom t2; Atom s1; Atom s2; Atom s3] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = Reg (int_of_string s1) in
        let s2 = Reg (int_of_string s2) in
        let s3 = Reg (int_of_string s3) in
        OpInst (LongDiv, [|t1; t2|], [|s1; s2; s3|])

      | _ -> raise Unknown_Form
      end

    | [Atom"Const"; Atom t; Atom s] ->
      let t = int_of_string t in
      let s = Int64.of_string s |> Int64.to_int in
      Assign (t, Const s)

    | [Atom"Mem"; Atom op; Atom reg; addr] ->
      let reg = int_of_string reg in
      let addr = parse_addr addr in
      let load op = OpInst (op, [|reg|], [|addr|])
      and store op = OpInst (op, [||], [|addr; Reg reg|]) in
      begin match op with
      | "Load" -> load (Load 8)
      | "Load8" -> load (Load 1)
      | "Load32" -> load (Load 4)
      | "Store" -> store (Store 8)
      | "Store8" -> store (Store 1)
      | "Store32" -> store (Store 4)
      | _ -> raise Unknown_Form
      end

    | Atom"seq"::rest ->
      Seq (map parse_inst rest |> to_array)

    | [Atom"if"; cond; p1; p2] ->
      let cond = parse_cond cond in
      If (cond, parse_inst p1, parse_inst p2)

    | [Atom"while"; cond; p1] ->
      let cond = parse_cond cond in
      While (cond, parse_inst p1)

    | [Atom"call"; ret_h; dest; exn_h] ->
      let dest = match dest with
        | List[Atom"direct"; Atom l] -> Loc (l,0)
        | List[Atom"reg"; Atom i] -> Reg (int_of_string i)
        | _ -> raise Unknown_Form
      in
      begin match ret_h with
      | Atom"tail" ->
        TailCall dest
      | List[Atom"returning"; Atom lr; _; _; p] ->
        let lr = int_of_string lr in
        let ret_h = parse_inst p in
        let exn_h = match exn_h with
          | Atom"no_handler" -> Skip
          | List[Atom"handler"; _; _; p] ->
            parse_inst p
          | _ -> raise Unknown_Form
        in
        FancyCall (dest, lr, ret_h, exn_h)
        | _ -> raise Unknown_Form
      end

    | [Atom "halt"; Atom s] ->
      let s = Reg (int_of_string s) in
      Halt s

    | [Atom "loc_value"; Atom t; Atom l; Atom m] ->
      let t = int_of_string t in
      let m = int_of_string m in
      Assign (t, Loc (l, m))

    | Atom"return" :: Atom s :: _ ->
      let s = int_of_string s in
      Jump (JReturn, Reg s)

    | Atom"raise" :: Atom s :: _ ->
      let s = int_of_string s in
      Jump (JRaise, Reg s)

    | [Atom"raw_call"; Atom l] ->
      Jump (JRawCall, Loc(l,1))

    | Atom"ffi" :: Atom entry :: Atom s1 :: Atom s2 :: Atom s3 :: Atom s4 :: _ ->
      let s1 = Reg (int_of_string s1) in
      let s2 = Reg (int_of_string s2) in
      let s3 = Reg (int_of_string s3) in
      let s4 = Reg (int_of_string s4) in
      FFI (entry, [|s1;s2;s3;s4|])

    | _ -> raise Unknown_Form
    end
  with Unknown_Form -> Unknown sexp

let parse_toplevel sexp =
  match sexp with
  | List[Atom"func"; Atom name; body] ->
    Some (name, parse_inst body)
  | _ ->
    None
