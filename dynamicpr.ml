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
   the format is
   ([0; 1], 1), (269, [0; 1])
   (([set of visited cities], current_city), 
   (total distance from 1 to current city,[the route - ordered list])
*)
let subroutes m = 
  List.foldi
    (List.map (List.range 1 (Distance_matrix.length m))
       ~f:(fun destination -> Distance_matrix.cost m 0 destination))
    ~init:Subroutes.empty
    ~f:(fun i t distance -> Subroutes.add t ([0; i+1], i+1) (distance, [0; i+1]))

let compute_distance m subroutes s j k = 
  let s' = List.filter s (fun x -> x <> j) in
  match Subroutes.find subroutes (0 :: s', k) with
      None -> failwith 
	(sprintf "([%s], k=%i), j=%i" 
	   (String.concat (List.map s ~f:(fun x -> sprintf "%i" x)) ~sep:"; ") 
	   k j) (* FIXME this is awful style *)
    | Some (distance, route) -> ((0::s, j), (distance + (Distance_matrix.cost m k j), route@[j]))

let minimum_path paths =
  List.hd_exn (
    List.sort paths ~cmp:(fun ((_,_), (dist1,_)) ((_,_), (dist2,_)) -> Int.ascending dist1 dist2)
  )
      
let solve_subproblem_for_given_s_and_j m subroutes (s, j) =
  let ks = List.filter s (fun x -> x <> j) in
  let paths = List.map ks ~f:(compute_distance m subroutes s j) in
  let min_path = minimum_path paths in
  Subroutes.add subroutes (fst min_path) (snd min_path)

let solve_subproblem_for_given_s m subroutes s = 
  List.fold_left
    (List.map s ~f:(fun j -> (s, j)))
    ~init:subroutes
    ~f:(solve_subproblem_for_given_s_and_j m)

let solve_subproblem matrix subroutes m = 
  let () = eprintf "solve_subproblem %i...\n%!" m in
  let s = extract m (List.range 1 (Distance_matrix.length matrix)) in
  List.fold_left
    s
    ~init:subroutes
    ~f:(solve_subproblem_for_given_s matrix)

let filter n routes = 
  List.filter routes ~f:(fun (_, route) -> List.length route = n)
    
let add_last_hop m t = List.map t
  ~f:(fun (distance, route) -> 
    (distance + (Distance_matrix.cost m (List.last_exn route) 0), route))
  
let sort t = 
  List.sort t ~cmp:(fun (dist1, _) (dist2, _) -> Int.ascending dist1 dist2)

(* FIXME is there a better way to print a list of numbers joined by space? *)
let print t =
  let () = printf "The solutions (distance:path) are:\n%!" in
  List.iter t ~f:(fun (distance, route) -> printf "%i: %s\n" 
    distance (String.concat ~sep:" " (List.map route ~f:(fun x -> sprintf "%i" (x+1)))))

let print_legend matrix =
  List.iteri (Distance_matrix.names matrix)
    ~f:(fun i x -> printf "%i: %s\n%!" (i+1) x)

(* main *)
let command =
  Command.basic
    ~summary:"TSP solver using dynamical programming techniques (Bellman, 1961)"
    Command.Spec.(
      empty
      +> flag "distancematrix" (required file) 
	~doc:"filename input file containing all distances"
    )
    (fun matrixf () ->
      let matrix = In_channel.with_file matrixf ~f:(fun ic ->
	In_channel.fold_lines ic ~init:Distance_matrix.empty 
	  ~f:(fun acc x ->
	     match String.split x ~on:'|' with
		 origin::destination::distance::_::[] ->
		   let distance = Int.of_string distance in
		   Distance_matrix.add acc origin destination distance
	       | _ -> failwith (sprintf "reading distance matrix line %s" x))) in
      printf "Distance matrix size is %i\n%!" (Distance_matrix.length matrix);
      let n = Distance_matrix.length matrix in  (* problem size *)
      let ms = List.range 2 n in                (* list of all subproblems *)
      let () = eprintf "solving for n = %i\n%!" n in
      let result = 
	List.fold_left 
	  ms 
	  ~init:(subroutes matrix)
	  ~f:(fun subroutes m -> solve_subproblem matrix subroutes m)
	|> Subroutes.to_list
	|> List.map ~f:(fun ((_,_), x) -> x)
	|> (filter (Distance_matrix.length matrix))
	|> (add_last_hop matrix)
	|> sort
      in
      print result;
      print_legend matrix
    )

let () = Command.run command
