(* Dynamical Programming in OCaml
   -- Ollie Frolovs <o.frolovs@gmail.com>
*)
open Core.Std

(* Attribution: http://ocaml.org/tutorials/99problems.html *)
let extract k list =
  let rec aux k acc emit = function
    | [] -> acc
    | h :: t ->
      if k = 1 then aux k (emit [h] acc) emit t else
        let new_emit x = emit (h :: x) in
        aux k (aux (k-1) acc new_emit t) emit t
  in
  let emit x acc = x :: acc in
  aux k [] emit list

(* TODO read the distances from a file *)
(*
  ("Bristol",   [("Cambridge", 269); ("London", 193);    ("Oxford", 118)]);
  ("Cambridge", [("Bristol", 269);   ("London", 117);    ("Oxford", 158)]);
  ("London",    [("Bristol", 193);   ("Cambridge", 117); ("Oxford",  98)]);
  ("Oxford",    [("Bristol", 118);   ("Cambridge", 158); ("London",  98)])
*)
let locations = [
  "Bristol";
  "Cambridge";
  "London";
  "Oxford"
]

let distance_matrix = [
  [  0;  269;  193;  118];
  [269;    0;  117;  158];
  [193;  117;    0;   98];
  [118;  158;   98;    0]
]

let distance_matrix = [
[0;	12;	6;	8;	20;	5;	18;	1;	17;	12];
[12;	0;	11;	4;	11;	1;	15;	12;	14;	3];
[6;	11;	0;	11;	2;	11;	9;	5;	9;	2];
[8;	4;	11;	0;	8;	4;	9;	7;	20;	16];
[20;	11;	2;	8;	0;	8;	13;	3;	11;	2];
[5;	1;	11;	4;	8;	0;	12;	15;	6;	19];
[18;	15;	9;	9;	13;	12;	0;	13;	9;	14];
[1;	12;	5;	7;	3;	15;	13;	0;	20;	10];
[17;	14;	9;	20;	11;	6;	9;	20;	0;	4];
[12;	3;	2;	16;	2;	19;	14;	10;	4;	0]
]


let cost k j = List.nth_exn (List.nth_exn distance_matrix j) k

(* size of the problem *)
let n = List.length distance_matrix

(* 
Map; (S : int set, k : int) -> (distance : int, path : int list)
*)

(* FIXME using Map is more efficient *)
(* FIXME try simple assoc list first *)
(*
let subroutes = [
  ([0; 1], 1), (269, [0; 1])
]
*)

(* the base cases where final destination j=0,
   aka all paths of length one starting from 
   the vertex 0 *)
let subroutes = 
(*
  let t1 = List.Assoc.add [] ([0; 1], 1) (5, [0; 1]) in
  let t2 = List.Assoc.add t1 ([0; 2], 2) (7, [0; 2]) in
  let t3 = List.Assoc.add t2 ([0; 3], 3) (9, [0; 3]) in
  t3
*)
(* FIXME how to create assoc list from indices? *)
  List.tl_exn (
    List.mapi (List.hd_exn distance_matrix)
      ~f:(fun i e -> (([0; i], i), (e, [0; i])))
  )

let compute_distance subroutes s j k = 
  let s' = List.filter s (fun x -> x <> j) in
  match List.Assoc.find subroutes (0 :: s', k) with
      None -> failwith 
	(sprintf "([%s], k=%i)" 
	   (String.concat (List.map s ~f:(fun x -> sprintf "%i" x)) ~sep:"; ") 
	   k) (* FIXME this is awful style *)
    | Some (distance, _) -> ((0::s, k), (distance + (cost k j), 0::s))

let subproblem_size = List.range 2 n

let minimum_path paths =
  List.hd_exn (
    List.sort 
      paths 
      ~cmp:(fun ((_,_), (dist1,_)) ((_,_), (dist2,_)) -> 
	if dist1 = dist2 then 0
	else if dist1 < dist2 then -1
	else 1
      )
  )
      
let solve_subproblem_for_given_s_and_j subroutes (s, j) =
  let ks = List.filter s (fun x -> x <> j) in
  let paths = List.map ks ~f:(compute_distance subroutes s j) in
  let min_path = minimum_path paths in
  List.Assoc.add subroutes (fst min_path) (snd min_path)

let solve_subproblem_for_given_s subroutes s = 
  let js = extract 1 s in
  List.fold_left
    (List.map js ~f:(fun j -> (s, List.hd_exn j)))
    ~init:subroutes
    ~f:solve_subproblem_for_given_s_and_j

let solve_subproblem subroutes m = 
  let s = extract m (List.range 2 n) in
  List.fold_left
    s
    ~init:subroutes
    ~f:solve_subproblem_for_given_s

let result =
  let n = List.length distance_matrix in  (* problem size *)
  let ms = List.range 2 n in              (* list of all subproblems *)
  List.fold_left
    ms
    ~init:subroutes
    ~f:solve_subproblem

let final_hop = 
  List.filter
    (List.Assoc.map 
       result 
       ~f:(fun (distance,route) -> (route, cost (List.last_exn route) 0)))
    (fun (_, (route,_)) -> List.length route = n - 1)
