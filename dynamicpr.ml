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

(*
module Routes = Comparator.Make(struct
  type t = (int list * int) * (int * int list)
  let sexp_of_t t = Sexp.Atom "N/A" 
  let t_of_sexp = 
  let compare (dist1,_) (dist2,_) = if dist1 > dist2 then 1 else if dist1 < dist2 then -1 else 0
end)
*)

(* TODO read the distances from a file *)

(*
let locations = [
  "Bristol";
  "Cambridge";
  "London";
  "Oxford"
]
*)

let distance_matrix = [
  [  0;  269;  193;  118];
  [269;    0;  117;  158];
  [193;  117;    0;   98];
  [118;  158;   98;    0]
]

(* n = 10 *)
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

(* n = 20 *)
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

(* n = 14 *)
(*
let distance_matrix = [
[0;	17;	24;	24;	60;	52;	36;	55;	54;	10;	8;	53;	36;	37];
[17;	0;	44;	21;	20;	13;	7;	9;	16;	22;	36;	41;	54;	36];
[24;	44;	0;	52;	53;	13;	60;	35;	32;	11;	12;	23;	50;	7];
[24;	21;	52;	0;	59;	47;	17;	10;	32;	39;	46;	56;	21;	21];
[60;	20;	53;	59;	0;	13;	55;	24;	21;	15;	20;	45;	58;	9];
[52;	13;	13;	47;	13;	0;	23;	30;	36;	33;	60;	39;	16;	44];
[36;	7;	60;	17;	55;	23;	0;	47;	48;	39;	17;	23;	35;	53];
[55;	9;	35;	10;	24;	30;	47;	0;	34;	7;	22;	25;	12;	36];
[54;	16;	32;	32;	21;	36;	48;	34;	0;	40;	28;	52;	26;	26];
[10;	22;	11;	39;	15;	33;	39;	7;	40;	0;	20;	22;	31;	57];
[8;	36;	12;	46;	20;	60;	17;	22;	28;	20;	0;	38;	30;	58];
[53;	41;	23;	56;	45;	39;	23;	25;	52;	22;	38;	0;	8;	21];
[36;	54;	50;	21;	58;	16;	35;	12;	26;	31;	30;	8;	0;	33];
[37;	36;	7;	21;	9;	44;	53;	36;	26;	57;	58;	21;	33;	0];
]
*)

let cost k j = List.nth_exn (List.nth_exn distance_matrix j) k

(* size of the problem *)
let n = List.length distance_matrix

(* 
Map; (S : int set, k : int) -> (distance : int, path : int list)
*)

(* FIXME using Map is more efficient *)
(* FIXME is using Set easy? *)
(* the base cases where final destination j=0,
   aka all paths of length one starting from 
   the vertex 0. 
   
   the format is
   ([0; 1], 1), (269, [0; 1])
   (([set of visited cities], current_city), 
   (total distance from 1 to current city,[the route - ordered list])
*)
let subroutes = 
  List.foldi
    (List.hd_exn distance_matrix) 
    ~init:Subroutes.empty
    ~f:(fun i t distance -> Subroutes.add t ([0; i], i) (distance, [0; i]))

let compute_distance subroutes s j k = 
  let s' = List.filter s (fun x -> x <> j) in
  match Subroutes.find subroutes (0 :: s', k) with
      None -> failwith 
	(sprintf "([%s], k=%i), j=%i" 
	   (String.concat (List.map s ~f:(fun x -> sprintf "%i" x)) ~sep:"; ") 
	   k j) (* FIXME this is awful style *)
    | Some (distance, route) -> ((0::s, j), (distance + (cost k j), route@[j]))

let minimum_path paths =
  List.hd_exn (
    List.sort paths ~cmp:(fun ((_,_), (dist1,_)) ((_,_), (dist2,_)) -> Int.ascending dist1 dist2)
  )
      
let solve_subproblem_for_given_s_and_j subroutes (s, j) =
  let ks = List.filter s (fun x -> x <> j) in
  let paths = List.map ks ~f:(compute_distance subroutes s j) in
  let min_path = minimum_path paths in
  Subroutes.add subroutes (fst min_path) (snd min_path)

let solve_subproblem_for_given_s subroutes s = 
  List.fold_left
    (List.map s ~f:(fun j -> (s, j)))
    ~init:subroutes
    ~f:solve_subproblem_for_given_s_and_j

let solve_subproblem subroutes m = 
  let () = eprintf "solve_subproblem %i...\n%!" m in
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

(* FIXME clusterfuck from now on *)

let long_routes_only t = 
  let routes = Subroutes.to_list t in
  List.filter routes
    (fun ((_,_), (_, route)) -> List.length route = n)
    
let complete_routes = List.map (long_routes_only result)
  ~f:(fun ((_,_), (distance, route)) -> 
    (distance + (cost (List.last_exn route) 0), route @ [0]))

let best_routes = 
  List.sort complete_routes ~cmp:(fun (dist1, _) (dist2, _) -> Int.ascending dist1 dist2)

(* FIXME is there a better way to print a list of numbers joined by space? *)
let () = List.iter best_routes ~f:(fun (distance, route) -> printf "%i: %s\n" 
  distance (String.concat ~sep:" " (List.map route ~f:(fun x -> sprintf "%i" (x+1)))))
