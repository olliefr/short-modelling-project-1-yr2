
open Core.Std

(* Many thanks to Malcolm Matalka on OCaml list for this *)
module Subroutes = Map.Make(struct
  type t = (int list * int) with compare, sexp
end)

type t = (int * int list) Subroutes.t
let empty = Subroutes.empty
let to_list = Subroutes.to_alist
let add t k v = Subroutes.add t ~key:k ~data:v
let find = Subroutes.find

(*
type t = ((int list * int) * (int * int list)) list
    
let empty = []

let to_list x = x

let add t k v = List.Assoc.add t k v

let find t k = List.Assoc.find t k
*)
