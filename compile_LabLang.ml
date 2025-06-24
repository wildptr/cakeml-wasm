
let process_file filepath =
  let text = In_channel.with_open_bin filepath In_channel.input_all in
  let definitions = Sexp.parse_sexp_list text in
  List.iter LabLang_Translator.trans_func definitions

let () =
  Printexc.record_backtrace true;

  let input_list = GList.create() in
  Arg.parse [] (GList.append input_list) "";
  print_endline "[bits 64]";
  print_endline "extern ffi_write";
  print_endline "extern exit";
  print_endline "section .text";
  GList.iter process_file input_list
(*
  print_endline "align 2";
  print_endline "global cake_codebuffer_begin";
  print_endline "cake_codebuffer_begin:";
  print_endline "global cake_codebuffer_end";
  print_endline "cake_codebuffer_end:"
*)
