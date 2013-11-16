
open Core.Std

(** A data structure to store the distances between the nodes 
    and the names of the nodes. The nodes are numbered from 0. *)
type t

(** Empty distance matrix *)
val empty : t

(** The number of locations in the matrix *)
val length : t -> int

(** Add an edge to the distance matrix *)
val add : t -> string -> string -> int -> t

(** The cost of moving from node j to node k *)
val cost : t -> int -> int -> int

(** Return the name of the node with given number *)
val name : t -> int -> string

(** Return a complete associative list of all values *)
(* FIXME how to get the types right? *)
(*
val to_alist : t -> ((int * int) * int) list
*)
