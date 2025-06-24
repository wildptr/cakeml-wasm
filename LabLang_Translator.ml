(* Translate LabLang into x86_64 (NASM syntax) *)

open Sexp
open Util

exception Unknown_Form
exception Arch_Constraint

let asm_name s =
  let a = GList.create() in
  GList.append a "_";
  let i = if s.[0]='$' then 1 else 0 in
  for i=i to String.length s-1 do
    let c = String.unsafe_get s i in
    GList.append a
    (
      if CChar.isalnum c || c='_' || c='.' || c='@' then
        String.make 1 c
      else
        sprintf "?%02x" (int_of_char c)
    )
  done;
  GList.finish a |> String.concat ""

let make_label l m =
  sprintf "%s.%s" (asm_name l) m

let reg_names = [|
  "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi";
  "r8" ; "r9" ; "r10"; "r11"; "r12"; "r13"; "r14"; "r15";
|]

let byte_reg_names = [|
  "al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil";
  "r8b"; "r9b"; "r10b"; "r11b"; "r12b"; "r13b"; "r14b"; "r15b";
|]

let dword_reg_names = [|
  "eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi";
  "r8d"; "r9d"; "r10d"; "r11d"; "r12d"; "r13d"; "r14d"; "r15d";
|]
(*
let trans_operand = function
  | List[Atom"Reg"; Atom r] ->
    print_string reg_names.(int_of_string r)
  | List[Atom"Imm"; Atom n] ->
    print_string n
  | _ -> raise Unknown_Form
*)

type operand = Reg of int | Imm of string

let parse_operand = function
  | List[Atom"Reg"; Atom r] -> Reg (int_of_string r)
  | List[Atom"Imm"; Atom n] -> Imm n
  | _ -> raise Unknown_Form

let parse_addr = function
  | List[Atom"Addr"; Atom base; Atom offset] ->
    let base = int_of_string base in
    let offset = Int64.of_string offset |> Int64.to_int in
    (base, offset)
  | _ -> raise Unknown_Form

let print_operand out = function
  | Reg i ->
    output_string out reg_names.(i)
  | Imm n ->
    output_string out n

let print_offset out offset =
  if offset<>0 then
    fprintf out "%+d" offset

let trans_cond = function
  | List[Atom op; Atom s1; s2] ->
    let s1 = int_of_string s1 in
    let s2 = parse_operand s2 in
    let op, cc = match op with
      | "Equal" -> ("cmp","e")
      | "NotEqual" -> ("cmp","ne")
      | "Lower" -> ("cmp","b")
      | "NotLower" -> ("cmp","nb")
      | "Less" -> ("cmp","l")
      | "NotLess" -> ("cmp","nl")
      | "Test" -> ("test","z")
      | "NotTest" -> ("test","nz")
      | _ -> raise Unknown_Form
    in
    printf "%s %s,%a\n" op reg_names.(s1) print_operand s2;
    cc
  | _ -> raise Unknown_Form

let trans_inst sexp =
  begin try
  match sexp with
  | List[Atom"asm"; List asm] ->
    begin match asm with
    | [Atom"Arith"; List arith] ->
      begin match arith with
      | [Atom"Binop"; Atom binop; Atom t; Atom s1; s2] ->
        let binop = match binop with
          | "Add" -> "add"
          | "Sub" -> "sub"
          | "Or" -> "or"
          | "And" -> "and"
          | "Xor" -> "xor"
          | _ -> raise Unknown_Form
        in
        let t = int_of_string t in
        let s1 = int_of_string s1 in
        let s2 = parse_operand s2 in
        if binop="or" && Reg s1=s2 then
          printf "mov %s,%s\n" reg_names.(t) reg_names.(s1)
        else (
          if t<>s1 then
            raise Arch_Constraint;
          printf "%s %s,%a\n" binop reg_names.(t) print_operand s2
        )

      | [Atom"Shift"; Atom shift; Atom t; Atom s1; Atom s2] ->
        let shift = match shift with
          | "Lsl" -> "shl"
          | "Lsr" -> "shr"
          | "Asr" -> "sar"
          | "Ror" -> "ror"
          | _ -> raise Unknown_Form
        in
        let t = int_of_string t in
        let s1 = int_of_string s1 in
        if t<>s1 then
          raise Arch_Constraint;
        printf "%s %s,%s\n" shift reg_names.(t) s2

      | [Atom"AddCarry"; Atom r1; Atom r2; Atom r3; Atom r4] ->
        (* sum, a, b, carry *)
        let r1 = int_of_string r1 in
        let r2 = int_of_string r2 in
        let r3 = int_of_string r3 in
        let r4 = int_of_string r4 in
        if r1<>r2 then
          raise Arch_Constraint;
        let r4_name = reg_names.(r4) in
        let byte_r4_name = byte_reg_names.(r4) in
        printf "test %s,%s\n" r4_name r4_name;
        printf "setnz %s\n" byte_r4_name;
        printf "rcr %s,1\n" byte_r4_name;
        printf "adc %s,%s\n" reg_names.(r1) reg_names.(r3);
        printf "setc %s\n" byte_r4_name;
        printf "movzx %s,%s\n" dword_reg_names.(r4) byte_r4_name

      | [Atom"AddOverflow"; Atom t1; Atom s1; Atom s2; Atom t2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = int_of_string s1 in
        let s2 = int_of_string s2 in
        if t1<>s1 then
          raise Arch_Constraint;
        let byte_t2_name = byte_reg_names.(t2) in
        printf "add %s,%s\n" reg_names.(t1) reg_names.(s2);
        printf "seto %s\n" byte_t2_name;
        printf "movzx %s,%s\n" dword_reg_names.(t2) byte_t2_name

      | [Atom"SubOverflow"; Atom t1; Atom s1; Atom s2; Atom t2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = int_of_string s1 in
        let s2 = int_of_string s2 in
        if t1<>s1 then
          raise Arch_Constraint;
        let byte_t2_name = byte_reg_names.(t2) in
        printf "sub %s,%s\n" reg_names.(t1) reg_names.(s2);
        printf "seto %s\n" byte_t2_name;
        printf "movzx %s,%s\n" dword_reg_names.(t2) byte_t2_name

      | [Atom"LongMul"; Atom t1; Atom t2; Atom s1; Atom s2] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = int_of_string s1 in
        let s2 = int_of_string s2 in
        if not (t1=2 && t2=0 && s1=0) then
          raise Arch_Constraint;
        printf "mul %s\n" reg_names.(s2)

      | [Atom"LongDiv"; Atom t1; Atom t2; Atom s1; Atom s2; Atom s3] ->
        let t1 = int_of_string t1 in
        let t2 = int_of_string t2 in
        let s1 = int_of_string s1 in
        let s2 = int_of_string s2 in
        let s3 = int_of_string s3 in
        if not (t1=0 && t2=2 && s1=2 && s2=0) then
          raise Arch_Constraint;
        printf "div %s\n" reg_names.(s3)

      | _ -> raise Unknown_Form
      end

    | [Atom"Const"; Atom t; Atom s] ->
      let t = int_of_string t in
      printf "mov %s,%s\n" reg_names.(t) s

    | [Atom"Mem"; Atom op; Atom reg; addr] ->
      let reg = int_of_string reg in
      let base, offset = parse_addr addr in
      begin match op with
      | "Load" ->
        printf "mov %s,[%s%a]\n"
          reg_names.(reg) reg_names.(base) print_offset offset
      | "Load8" ->
        printf "movzx %s,byte[%s%a]\n"
          reg_names.(reg) reg_names.(base) print_offset offset
      | "Load32" ->
        printf "mov %s,[%s%a]\n"
          dword_reg_names.(reg) reg_names.(base) print_offset offset
      | "Store" ->
        printf "mov [%s%a],%s\n"
          reg_names.(base) print_offset offset reg_names.(reg)
      | "Store8" ->
        printf "mov [%s%a],%s\n"
          reg_names.(base) print_offset offset byte_reg_names.(reg)
      | "Store32" ->
        printf "mov [%s%a],%s\n"
          reg_names.(base) print_offset offset dword_reg_names.(reg)
      | _ -> raise Unknown_Form
      end

    | [Atom"JumpReg>"; Atom s] ->
      let s = int_of_string s in
      printf "jmp %s\n" reg_names.(s)

    | _ -> raise Unknown_Form
    end (* (asm ...) *)

  | List[Atom"label"; Atom l; Atom m] ->
    print_endline "align 2";
    printf "%s:\n" (make_label l m)

  | List[Atom"call_FFI"; Atom ffi_name] ->
    printf "call ffi_%s\n" ffi_name

  | Atom"halt" ->
    print_endline "jmp exit"

  | List[Atom "loc_value"; Atom t; Atom l; Atom m] ->
    let t = int_of_string t in
    printf "mov %s,%s\n" reg_names.(t) (make_label l m)

  | List[Atom"jump"; Atom l; Atom m] ->
    printf "jmp %s\n" (make_label l m)

  | List[Atom"jump_cmp"; cond; Atom l; Atom m] ->
    let cc = trans_cond cond in
    printf "j%s %s\n" cc (make_label l m)

  | _ -> raise Unknown_Form

  with Unknown_Form ->
    printf "int3 ;#%a\n" pp_sexp sexp
  end


let trans_func = function
  | List (Atom func_name :: rest) ->
    let func_asm_name = make_label func_name "0" in
    printf "\nalign 2\nglobal %s\n%s:\n" func_asm_name func_asm_name;
    List.iter trans_inst rest

  | _ -> raise Unknown_Form
