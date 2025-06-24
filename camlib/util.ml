(* List operations *)
let foldl = List.fold_left
let foldr f init list = List.fold_right f list init
let cons a b = a::b
let rev_append = List.rev_append
let nth = List.nth
let zip = List.combine
let map = List.map

(* Array operations *)
let to_list = Array.to_list
let to_array = Array.of_list
let length = Array.length
let amap = Array.map
let afoldl = Array.fold_left
let azip = Array.combine

(* Finite sequence operations *)
type 'a fseq = 'a FinSeq.fseq
let single = FinSeq.singleton
let (++) = FinSeq.join
let (+-) a b = FinSeq.append b a
let (-+) = FinSeq.prepend
let fseq = FinSeq.from_list
let empty = FinSeq.empty

[@@@pml "infix 120 ++ +- -+"]

(* General *)
let apfst f (a, b) = (f a, b)
let apsnd f (a, b) = (a, f b)
let apfst_snd (f, g) (a, b) = (f a, g b)

module SSet = Set.Make(String)
module SMap = Map.Make(String)

module ISet = Set.Make(Int)
module IMap = Map.Make(Int)

module StringH = struct
  type t = string
  let equal = String.equal
  let hash s =
    let n = String.length s in
    let rec f a i =
      if i<n then
        let c = Char.code s.[i] in
        f (a*19+c) (i+1)
      else a
    in
    f 0 0
end

module STable = Hashtbl.Make(StringH)

(* Formatting *)

let sprintf = Printf.sprintf
let printf  = Printf.printf
let fprintf = Printf.fprintf
let eprintf = Printf.eprintf

(* Misc *)

let tick ctr =
  let i = !ctr in (ctr:=i+1; i)
