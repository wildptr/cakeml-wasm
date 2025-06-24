type exp =
  | Int of int
  | String of string
  | Constr of string * exp array
  | Tuple of exp array
  | List of exp array

val const: string -> exp
val tuple: exp list -> exp
val constr: string -> exp list -> exp
val list: exp list -> exp

val pp_exp: out_channel -> exp -> unit
val pp_exp_max_depth: int -> out_channel -> exp -> unit
