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
(*
let locations = [
  "Bristol";
  "Cambridge";
  "London";
  "Oxford"
]
*)
(*
let distance_matrix = [
  [  0;  269;  193;  118];
  [269;    0;  117;  158];
  [193;  117;    0;   98];
  [118;  158;   98;    0]
]
*)

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

(*
let distance_matrix = [
[0;	26;	34;	51;	16;	12;	59;	33;	7;	11;	38;	31;	14;	46;	52;	12;	20;	25;	51;	47];
[26;	0;	55;	49;	8;	41;	42;	36;	58;	11;	53;	23;	36;	16;	57;	57;	28;	20;	36;	20];
[34;	55;	0;	25;	20;	7;	53;	26;	43;	30;	30;	30;	13;	41;	22;	28;	17;	17;	47;	42];
[51;	49;	25;	0;	60;	30;	35;	36;	55;	23;	39;	53;	42;	35;	35;	50;	12;	48;	8;	16];
[16;	8;	20;	60;	0;	59;	38;	23;	8;	25;	48;	25;	56;	52;	50;	58;	30;	14;	35;	41];
[12;	41;	7;	30;	59;	0;	26;	59;	17;	7;	49;	28;	32;	23;	23;	34;	11;	56;	52;	50];
[59;	42;	53;	35;	38;	26;	0;	23;	33;	23;	24;	45;	47;	25;	50;	26;	17;	11;	7;	60];
[33;	36;	26;	36;	23;	59;	23;	0;	14;	42;	50;	21;	42;	39;	32;	52;	36;	29;	19;	43];
[7;	58;	43;	55;	8;	17;	33;	14;	0;	47;	24;	55;	34;	53;	38;	50;	44;	31;	25;	20];
[11;	11;	30;	23;	25;	7;	23;	42;	47;	0;	53;	57;	15;	11;	58;	49;	12;	50;	29;	28];
[38;	53;	30;	39;	48;	49;	24;	50;	24;	53;	0;	32;	40;	51;	30;	42;	8;	9;	13;	49];
[31;	23;	30;	53;	25;	28;	45;	21;	55;	57;	32;	0;	53;	10;	23;	39;	53;	12;	20;	30];
[14;	36;	13;	42;	56;	32;	47;	42;	34;	15;	40;	53;	0;	29;	26;	45;	58;	24;	30;	32];
[46;	16;	41;	35;	52;	23;	25;	39;	53;	11;	51;	10;	29;	0;	44;	34;	23;	11;	27;	28];
[52;	57;	22;	35;	50;	23;	50;	32;	38;	58;	30;	23;	26;	44;	0;	29;	51;	49;	38;	41];
[12;	57;	28;	50;	58;	34;	26;	52;	50;	49;	42;	39;	45;	34;	29;	0;	38;	43;	25;	14];
[20;	28;	17;	12;	30;	11;	17;	36;	44;	12;	8;	53;	58;	23;	51;	38;	0;	23;	24;	60];
[25;	20;	17;	48;	14;	56;	11;	29;	31;	50;	9;	12;	24;	11;	49;	43;	23;	0;	31;	29];
[51;	36;	47;	8;	35;	52;	7;	19;	25;	29;	13;	20;	30;	27;	38;	25;	24;	31;	0;	16];
[47;	20;	42;	16;	41;	50;	60;	43;	20;	28;	49;	30;	32;	28;	41;	14;	60;	29;	16;	0]
]

*)

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
	(sprintf "([%s], k=%i), j=%i" 
	   (String.concat (List.map s ~f:(fun x -> sprintf "%i" x)) ~sep:"; ") 
	   k j) (* FIXME this is awful style *)
    | Some (distance, route) -> ((0::s, j), (distance + (cost k j), route@[j]))

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
  List.fold_left
    (List.map s ~f:(fun j -> (s, j)))
    ~init:subroutes
    ~f:solve_subproblem_for_given_s_and_j

let solve_subproblem subroutes m = 
  let s = extract m (List.range 1 n) in
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

let long_routes_only = 
  List.filter result 
    (fun ((_,_), (_, route)) -> List.length route = n)
    
let complete_routes = List.map long_routes_only
  ~f:(fun ((_,_), (distance, route)) -> 
    (distance + (cost (List.last_exn route) 0), route @ [0]))

let best_routes = 
  List.sort complete_routes
    ~cmp:(fun (dist1, _) (dist2, _) -> 
      if dist1 = dist2 then 0
      else if dist1 < dist2 then -1
      else 1
    )

(* FIXME is there a better way to print a list of numbers joined by space? *)
let () = List.iter best_routes ~f:(fun (distance, route) -> printf "%i: %s\n" 
  distance (String.concat ~sep:" " (List.map route ~f:(fun x -> sprintf "%i" (x+1)))))
