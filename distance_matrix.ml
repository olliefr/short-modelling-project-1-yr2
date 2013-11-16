
open Core.Std

module M = Map.Make(struct
  type t = (int * int) with compare, sexp
end)

type t = {
  names : string list;
  destinations : int M.t
}

let empty = {
  names = [];
  destinations = M.empty
}

(* Assumption: there are no unconnected nodes *)
let length ({names; _} : t) =
  List.length names

let lookup_or_insert names key =
  match List.findi names ~f:(fun _ x -> x = key) with
    | None -> let n = List.length names in
	      (n, names @ [key]) (* FIXME sloow *)
    | Some (pos, name) -> (pos, names)

let add {names; destinations} origin destination distance =
  let o_number, names = lookup_or_insert names origin in
  let d_number, names = lookup_or_insert names destination in
  let destinations = M.add destinations ~key:(o_number, d_number) ~data:distance in
  {names; destinations}

(* FIXME return option? *)
let cost {destinations; _} origin destination =
  match M.find destinations (origin, destination) with
    | None -> failwith (sprintf "%i to %i" origin destination)
    | Some distance -> distance

(* FIXME return option? *)
let name {names; _} location =
  List.nth_exn names location

let to_alist t = M.to_alist t
