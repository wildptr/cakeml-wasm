
let fold_option f opt a =
  match opt with
  | Some x -> f x a
  | None -> a
