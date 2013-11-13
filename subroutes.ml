
open Core.Std

type t = ((int list * int) * (int * int list)) list

let empty = []

let to_list x = x

let add t k v = List.Assoc.add t k v

let find t k = List.Assoc.find t k
