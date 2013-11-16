
open Core.Std

(** A data structure to store solved subproblems for 
    dynamic programming (Bellman 1961) approach to solving TSP *)
type t

(** The empty data structure *)
val empty : t

(** Store a solution to a subproblem.
    FIXME why expose Key.t? why cannot leave (int list * int)? *)
val add : t -> (int list * int) -> (int * int list) -> t

(** Find a solution to a subproblem 
    FIXME why expose Key.t? why cannot leave (int list * int)? *)
val find : t -> (int list * int) -> (int * int list) option

(** Convert the data structure to an association list.
    Useful for printing but much too slow for anything else *)
val to_list : t -> ((int list * int) * (int * int list)) list
