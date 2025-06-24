open Sexp
open Util

(* ~/cakeml-master/semantics/astScript.sml *)

exception Syntax of string

let print_sexp_string out = function
  | String s -> output_string out s
  | _ -> output_char out '_'

(* (a, b, ...) *)
let trans_list f out = function
  | List (first::rest) ->
    output_char out '(';
    f out first;
    rest |> List.iter (fun x -> output_char out ','; f out x);
    output_char out ')'
  | _->()

let rec trans_id out = function
  | List (Atom"Short"::s::_) ->
    print_sexp_string out s
  | List (Atom"Long"::s::rest::_) ->
    fprintf out "%a." print_sexp_string s;
    trans_id out rest
  | _ -> raise (Syntax "id")

let trans_lit out = function
  | Atom lit ->
    output_string out lit
  | String s ->
    Lex.output_quoted_string out s
  | List (Atom"word8" :: s :: _) ->
    fprintf out "'\\%a'" print_sexp_string s
  | List (Atom"word64" :: s :: _) ->
    fprintf out "%aL" print_sexp_string s
  | List (Atom"char" :: String s :: _) ->
    Lex.output_single_quoted_string out s
  | List (Atom h :: _) ->
    raise (Syntax ("lit: "^h))
  | _ -> raise (Syntax "lit")

(* Types *)

let rec trans_type out = function
  | List (Atom "Atvar" :: s :: _) ->
    print_sexp_string out s

  | List (Atom "Atapp" :: type_args :: type_con :: _) ->
    fprintf out "%a%a" (trans_list trans_type) type_args trans_id type_con

  | List (Atom "Attup" :: elem_types :: _) ->
    begin match elem_types with
    | List elem_types ->
      fprintf out "(%a)" (PrintfUtil.pp_list_sep "*" trans_type) elem_types
    | _ ->
      output_string out "unit"
    end

  | List (Atom "Atfun" :: dom :: ran :: _) ->
    fprintf out "(%a->%a)" trans_type dom trans_type ran

  | _ -> raise (Syntax "type")

(* Patterns *)

let is_empty_list = function
  | List(_::_) -> false
  | _ -> true

let rec trans_pat out = function
  | String x ->
    output_string out x

  | List (Atom"Pany" :: _) ->
    output_char out '_'

  | List (Atom"Pcon" :: con :: args :: _) ->
    begin match con with
    | Atom"NONE" ->
      if is_empty_list args then output_string out "()"
    | List(Atom"SOME" :: id :: _) ->
      trans_id out id
    | _ -> raise (Syntax "pat(Pcon)")
    end;
    trans_list trans_pat out args

  | List (Atom"Plit" :: lit :: _) ->
    trans_lit out lit

  | _ -> raise (Syntax "pat")

(* Expressions *)

let indent out lev =
  for _=1 to lev do
    output_string out "  "
  done

let lbr out lev =
  output_char out '\n';
  indent out lev

let rec trans_exp lev out = function
  | List (Atom"Lit" :: lit :: _) ->
    trans_lit out lit

  | List (Atom"Var" :: id :: _) ->
    trans_id out id

  | List (Atom"App" :: op :: args :: _) ->
    output_char out '(';
    begin match args with
    | List args ->
      begin match op with
      | Atom"Opapp" ->
        PrintfUtil.pp_list_sep " " (trans_exp (lev+1)) out args
      | _ ->
        fprintf out "#%a" pp_sexp op;
        args |> List.iter (fprintf out " %a" (trans_exp (lev+1)))
      end
    | _->()
    end;
    output_char out ')'

  | List (Atom"Con" :: con :: args :: _) ->
    begin match con with
    | Atom"NONE" ->
      if is_empty_list args then output_string out "()"
    | List(Atom"SOME" :: id :: _) ->
      trans_id out id
    | _ -> raise (Syntax "exp(Pcon)")
    end;
    trans_list (trans_exp (lev+1)) out args

  | List (Atom"Fun" :: par_id :: fun_body :: _) ->
    fprintf out "(fun %a ->%a%a%a)" trans_pat par_id lbr (lev+1)
      (trans_exp (lev+1)) fun_body lbr lev

  | List (Atom"Mat" :: s :: cases :: _) ->
    fprintf out "(match %a with\n%a%a)" (trans_exp (lev+1)) s
      (trans_match_clauses lev) cases
      indent lev

  | List (Atom"If" :: s1 :: s2 :: s3 :: _) ->
(*
  (if _ then
    _
  else
    _
  )
*)
    fprintf out "(if %a then%a%a%aelse%a%a%a)" (trans_exp (lev+1)) s1 lbr (lev+1)
      (trans_exp (lev+1)) s2 lbr lev
      lbr (lev+1)
      (trans_exp (lev+1)) s3 lbr lev

  | List (Atom"Let" :: lhs_id :: rhs :: body :: _) ->
    fprintf out "let _ =%a%a%ain%a%a" lbr (lev+1)
      (trans_exp (lev+1)) rhs lbr lev
      lbr lev
      (trans_exp lev) body

  | List (Atom"Letrec" :: _ :: body :: _) ->
    fprintf out "let rec in%a%a"
      lbr lev
      (trans_exp lev) body

  | List (Atom"Raise" :: s :: _) ->
    fprintf out "(raise %a)" (trans_exp (lev+1)) s

  | List (Atom"Handle" :: s :: cases :: _) ->
    fprintf out "(try%a%a%awith\n%a%a)" lbr (lev+1)
      (trans_exp (lev+1)) s lbr lev
      (trans_match_clauses (lev+1)) cases
      indent lev

  | List (Atom"Log" :: op :: s1 :: s2 :: _) ->
    let op_sym =
      match op with
      | Atom"And" -> "&&"
      | Atom"Or"  -> "||"
      | _ -> raise (Syntax "exp(Log)")
    in
    fprintf out "(%a %s %a)" (trans_exp (lev+1)) s1 op_sym (trans_exp (lev+1)) s2

  | s ->
    fprintf out "#%a" pp_sexp s

and trans_match_clauses lev out lst =
  match lst with
  | List lst ->
    lst |> List.iter begin function
      | List (lhs :: rhs_body) ->
        fprintf out "%a| %a ->%a%a\n" indent lev trans_pat lhs lbr (lev+1)
          (trans_exp (lev+1)) (List rhs_body)
      | _->()
    end
  | _ -> ()

(* Declarations *)

let trans_type_dec out = function
  | _loc :: List defs :: _ ->
    begin match defs with
    | [] -> ()
    | first::rest ->
      let f head = function
      | List (type_pars :: type_name :: constrs) ->
        fprintf out "%s %a%a =\n" head (trans_list print_sexp_string) type_pars
          print_sexp_string type_name;
        constrs |> List.iter begin function
        | List (c_name :: c_arg_types) ->
          fprintf out "  | %a" print_sexp_string c_name;
          begin match c_arg_types with
          | [] -> ()
          | first::rest ->
            fprintf out " of (%a)" trans_type first;
            rest |> List.iter (fprintf out " * (%a)" trans_type)
          end;
          output_char out '\n'
        | _->()
        end
      | _ -> ()
      in
      f "type" first;
      List.iter (f "and") rest
    end

  | _ -> raise (Syntax "Dtype")

let trans_let_dec out = function
  | _loc :: lhs :: rhs :: _ ->
    fprintf out "let %a =\n  %a\n" trans_pat lhs
      (trans_exp 1) rhs

  | _ -> raise (Syntax "Dlet")

let () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  match parse_sexp text with
  | List decs ->
    decs |> List.iter begin function
      | List (Atom head :: body) ->
        begin match head with
        | "Dtype" -> trans_type_dec stdout body
        | "Dlet" -> trans_let_dec stdout body
        | _->()
        end
      | _ -> ()
    end
  | _->()

