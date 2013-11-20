
open Core.Std

type route = {
  origin : string;
  destination : string;
  distance : int;
  duration : int
}

(* prune any whitespace-only or empty lines from the input *)
(* FIXME this is used in other file, move into utilities? *)
let prune list = 
  list
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun x -> not (String.is_empty x))

let command =
  Command.basic
    ~summary:"Read the definition of a cluster and filter the distance matrix data"
    Command.Spec.(
      empty
      +> flag "-cluster" (required file) ~doc:"filename text file which defines the cluster"
      +> flag "-matrix"  (required file) ~doc:"filename text file with output from the distance-matrix program"
      +> flag "-clustermatrix" (required file) ~doc:"filename output; matrix filtered with cluster entries"
    )
    (fun clusterf matrixf clustermatrixf () ->
      printf "Starting up...\n%!";
      let cluster = In_channel.with_file clusterf ~f:(fun ic ->
	In_channel.fold_lines ic ~init:[] ~f:(fun acc x -> x :: acc)) 
	|> prune 
	|> List.fold ~init:(Set.empty String.comparator) ~f:Set.add in
      printf "Size of cluster is %i\n%!" (Set.length cluster);
      let matrix = In_channel.with_file matrixf ~f:(fun ic ->
	In_channel.fold_lines ic ~init:[] ~f:(fun acc x ->
	  match String.split x ~on:'|' with
	      origin::destination::distance::duration::[] ->
		let distance = Int.of_string distance in
		let duration = Int.of_string duration in
		{origin; destination; distance; duration} :: acc
	    | _ -> failwith (sprintf "failed to read distance matrix line %s" x))) in
      printf "Size of distance matrix is %i\n%!" (List.length matrix);
      let cluster_matrix = List.filter matrix ~f:(fun {origin;destination;_} -> 
	(Set.mem cluster origin) && (Set.mem cluster destination)) in
      printf "The cluster matrix size is %i\n%!" (List.length cluster_matrix);
      Out_channel.with_file clustermatrixf ~f:(fun oc ->
	List.iter cluster_matrix ~f:(fun {origin;destination;distance;duration} ->
	  Out_channel.output_string oc
	    (sprintf "%s|%s|%i|%i\n%!" origin destination distance duration)))
    )

let () = Command.run command
