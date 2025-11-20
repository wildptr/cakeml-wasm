open Util

type binop = Add | Sub | And | Or | Xor

type relop = Equal | NotEqual | Lower | NotLower | Less | NotLess | Test | NotTest

type shift_op = Lsl | Lsr | Asr | Ror

type op =
  | Bin of binop
  | Shift of shift_op
  | Rel of relop
  | AddCarry (*2;3*)
  | AddOverflow (*2;2*)
  | SubOverflow (*2;2*)
  | LongMul (*2;2*)
  | LongDiv (*2;3*)
  | Load of int(*size*) (* addr *)
  | Store of int(*size*) (* addr, data *)

(*
type exp =
  | Const of int
  | Reg of int
  | Binop of binop * exp * exp
  | Relop of relop * exp * exp
*)

type exp =
  | Const of int
  | Reg of int
  | Loc of string*int
  | Op of op * exp array

type jump_kind = JRawCall | JReturn | JRaise

type prog =
  | Skip
  | Assign of int * exp
  | OpInst of op * int array * exp array
  | Halt of exp
  | Seq of prog array
  | If of exp * prog * prog
  | While of exp * prog
  | Unknown of Sexp.sexp
  (* *)
  | FancyCall of exp(*target*) * int(*link register*) * prog(*return handler*) * prog option(*exception handler*)
  | TailCall of exp(*target*)
  | Jump of jump_kind * exp
  | FFI of string * exp array

let pp_var out i =
  fprintf out "$%d" i

let rec pp_exp out = function
  | Const n ->
    printf "%d" n
  | Reg i ->
    printf "$%d" i
  | Loc (l,m) ->
    printf "@%s.%d" l m
  | Op (op, ss) ->
    pp_op out (op ,ss)

and pp_op out (op, ss) =
  let infix op_sym =
    fprintf out "%a%s%a" pp_exp ss.(0) op_sym pp_exp ss.(1)
  and prefix op_name =
    fprintf out "%s(%a)" op_name (PrintfUtil.pp_array pp_exp) ss
  in
  match op with
  | Bin op ->
    let op_sym = match op with
      | Add -> "+"
      | Sub -> "-"
      | Or -> "|"
      | And -> "&"
      | Xor -> "^"
    in
    fprintf out "%a%s%a" pp_exp ss.(0) op_sym pp_exp ss.(1)

  | Shift op ->
    begin match op with
    | Lsl -> infix "<<"
    | Lsr -> infix ">>"
    | Asr -> infix "±>>"
    | Ror -> prefix "ror"
    end

  | Rel op ->
    begin match op with
    | Equal -> infix "=="
    | NotEqual -> infix "!="
    | Less -> infix "±<"
    | NotLess -> infix "±>="
    | Lower -> infix "<"
    | NotLower -> infix ">="
    | NotTest -> infix "&"
    | Test -> fprintf out "!(%a&%a)" pp_exp ss.(0) pp_exp ss.(1)
    end

  | AddCarry -> prefix "add_carry"
  | AddOverflow -> prefix "add_overflow"
  | SubOverflow -> prefix "sub_overflow"
  | LongMul -> prefix "long_mul"
  | LongDiv -> prefix "long_div"

  | Load size ->
    fprintf out "load.%d(%a)" size pp_exp ss.(0)

  | Store size ->
    fprintf out "store.%d(%a, %a)" size pp_exp ss.(0) pp_exp ss.(1)

let indent n out =
  String.make (n*2) ' ' |> output_string out

let rec print_prog lev ind out p =
  if ind then indent lev out;
  match p with
  | Skip -> output_string out ";\n"
  | Assign (t, s) ->
    fprintf out "$%d = %a;\n" t pp_exp s
  | OpInst (op, tt, ss) ->
    if tt<>[||] then
      fprintf out "%a = " (PrintfUtil.pp_array pp_var) tt;
    printf "%a;\n" pp_op (op, ss)
  | Seq pp ->
    output_string out "{\n";
    pp |> Array.iter (print_prog (lev+1) true out);
    indent lev out;
    output_string out "}\n"

  | If (e, p1, p2) ->
    fprintf out "if (%a) " pp_exp e;
    print_prog (lev) false out p1;
    if p2<>Skip then
    (
      indent lev out;
      output_string out "else ";
      print_prog (lev) false out p2
    )

  | While (e, p1) ->
    fprintf out "while (%a) " pp_exp e;
    print_prog (lev) false out p1

  | Jump (_, s) ->
    fprintf out "goto %a;\n" pp_exp s

  | Halt s ->
    fprintf out "halt %a;\n" pp_exp s

  | FancyCall (dest, _, ret_handler, exn_handler) ->
    fprintf out "fancy_call (%a) " pp_exp dest;
    print_prog (lev) false out ret_handler;
    begin match exn_handler with
    | None -> ()
    | Some exn_handler ->
      indent lev out;
      output_string out "except ";
      print_prog (lev) false out exn_handler;
    end

  | TailCall dest ->
    fprintf out "tail_call %a;\n" pp_exp dest

  | FFI (entry, args) ->
    fprintf out "ffi.%s(%a);\n" entry (PrintfUtil.pp_array pp_exp) args

  | Unknown sexp ->
    fprintf out "#%a\n" Sexp.pp_sexp sexp

(* ~/cakeml-wordLang_multiret/compiler/backend/stackLangScript.sml *)
(*
  prog = Skip
       | Inst ('a inst)
       | Get num store_name
       | Set store_name num
       | OpCurrHeap binop num num
       | Call ((stackLang$prog # num # num # num) option)
              (* return-handler code, link reg, labels l1,l2*)
              (num + num) (* target of call *)
              ((stackLang$prog # num # num) option)
              (* handler: exception-handler code, labels l1,l2*)
       | Seq stackLang$prog stackLang$prog
       | If cmp num ('a reg_imm) stackLang$prog stackLang$prog
       | While cmp num ('a reg_imm) stackLang$prog
       | JumpLower num num num (* reg, reg, target name *)
       | Alloc num
       | StoreConsts num num (num option) (* reg, reg, stub name to call *)
       | Raise num
       | Return num
       | FFI string num num num num num (* FFI index, conf_ptr, conf_len,
                                           array_ptr, array_len, ret_addr *)
       | Tick
       | LocValue num num num   (* assign v1 := Loc v2 v3 *)
       | Install num num num num num (* code buffer start, length of new code,
                                      data buffer start, length of new data, ret_addr *)
       | ShMemOp memop num ('a addr) (* share memory operation, register, addr to load/store *)
       | CodeBufferWrite num num (* code buffer address, byte to write *)
       | DataBufferWrite num num (* data buffer address, word to write *)
       (* new in stackLang, compared to wordLang, below *)
       | RawCall num            (* tail-call into body of function (past stack alloc) *)
       | StackAlloc num         (* allocate n slots on the stack *)
       | StackFree num          (* free n slots on the stack *)
       | StackStore num num     (* offset, fast *)
       | StackStoreAny num num  (* reg contains offset, slow, used by GC *)
       | StackLoad num num      (* offset, fast *)
       | StackLoadAny num num   (* reg contains offset, slow, used by GC *)
       | StackGetSize num       (* used when installing exc handler *)
       | StackSetSize num       (* used by implementation of raise *)
       | BitmapLoad num num     (* load word from read-only region *)
       | Halt num
*)
(* Semantics in: *)
(* ~/cakeml-wordLang_multiret/compiler/backend/semantics/stackSemScript.sml *)

(* ~/cakeml-wordLang_multiret/compiler/backend/semantics/labSemScript.sml *)

(* ~/cakeml-wordLang_multiret/compiler/encoders/asm/asmScript.sml *)
(* ~/cakeml-wordLang_multiret/compiler/encoders/asm/asmSemScript.sml *)

(*
  inst = Skip
       | Const reg ('a word)
       | Arith ('a arith)
       | Mem memop reg ('a addr)
       | FP fp
*)
