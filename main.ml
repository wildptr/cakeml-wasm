open Util

let memory_size = 0x200_0000
let stack_size = 0x10_0000

let argspec =
[
  ("-wasm-tail-call", Arg.Set Comp.use_tail_calls, "use wasm tail calls");
]

let process_file filepath =
  let text = In_channel.with_open_bin filepath In_channel.input_all in
  let definitions = Sexp.parse_sexp_list text in

  let functions =
    let a = GList.create() in
    definitions |> List.iter begin fun sexp ->
      match StackLang_Parser.parse_toplevel sexp with
      | Some (func_name, func_body) ->
        GList.append a (func_name, func_body)
      | None->()
    end;
    GList.finish_as_array a
  in

  let num_support_func = 16 in

  let ffi_functions = [
    "write";
    "read";
    "open_in";
    "close";
    "get_arg_count";
    "get_arg_length";
    "get_arg";
  ] in

  print_endline "(module";

  (* Imports *)
  [
(*
    "(import \"host\" \"add_carry\" (func $add_carry (param i64 i64 i64) (result i64 i64)))\n";
    "(import \"host\" \"add_overflow\" (func $add_overflow (param i64 i64) (result i64 i64)))\n";
    "(import \"host\" \"sub_overflow\" (func $sub_overflow (param i64 i64) (result i64 i64)))\n";
    "(import \"host\" \"long_mul\" (func $long_mul (param i64 i64) (result i64 i64)))\n";
*)
    "(import \"host\" \"long_div\" (func $long_div (param i64 i64 i64) (result i64 i64)))\n";
    "(import \"host\" \"rt_exit\" (func $rt_exit (param i64)))\n";
    "(import \"host\" \"rt_tee\" (func $rt_tee (param i64) (result i64)))\n";
  ]
  |> List.iter print_string;
  ffi_functions |> List.iter begin fun f ->
    printf "(import \"host\" \"ffi_%s\" (func $ffi_%s (param i64 i64 i64 i64)))\n" f f
  end;

  (* Globals *)
  print_newline();
  printf "(global (mut i64) (i64.const 0))\n";
  (* 4th arg reg (1,RCX): end of stack *)
  printf "(global (mut i64) (i64.const %d))\n" memory_size;
  (* 3rd arg reg (2,RDX): end of heap = start of stack *)
  printf "(global (mut i64) (i64.const %d))\n" (memory_size - stack_size);
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  (* 2nd arg reg (6,RSI): start of heap *)
  printf "(global (mut i64) (i64.const `$data_size`))\n";
  (* 1st arg reg (7,RDI): start of program (n/a in wasm) *)
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";
  printf "(global (mut i64) (i64.const 0))\n";

  (* Memory *)
  print_newline();
  printf "(memory (export \"memory\") %d)\n" (memory_size lsr 16);

  (* Table *)
  if !Comp.use_tail_calls then
  begin
    printf "(table %d funcref)\n" (length functions);
    functions |> Array.iteri begin fun i (func_name, _) ->
      printf "(elem (i32.const %d) $%s)\n" i func_name;
      STable.add Comp.func_table func_name i
    end
  end;

  (* Types *)
  print_endline "(type $Ftype (func (result i32)))";
  print_endline "(type $Btype (func (param i32)))";

  (* *)

  print_newline();
  print_endline "`$support`"; (* placehoder for support routines *)

  if !Comp.use_tail_calls then
    functions |> Array.iteri begin fun i (func_name, func_body) ->
      printf "\n;; %s (func #%d)\n" func_name (num_support_func + i);
      print_endline "(;";
      StackLang.print_prog 0 false stdout func_body;
      print_endline ";)";
      let wasm: Wasm.inst list = Comp.comp_func func_body in
      let func_export_name =
        (* HACK *)
        if String.ends_with "@0" func_name then "wa_start" else ""
      in
      printf "%a\n" (Wasm.pp_wasm 0) (Wasm.make_func (func_name, func_export_name, map Wasm.inst_to_sexp wasm))
    end
  else (
    Comp.compile_program_without_tail_calls functions;
    (* entry point *)
    print_endline "(func (export \"wa_start\") (result i32) (call $cakeml (i32.const 0)))"
  );

  print_endline ")"


let () =
  Printexc.record_backtrace true;

  let input_list = GList.create() in
  Arg.parse argspec (GList.append input_list) "";
  GList.iter process_file input_list
