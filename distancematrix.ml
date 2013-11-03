(** Extract travel distance and time for a matrix of origins and destinations
    using JSON over HTTP.

    Google Distance Matrix
    https://developers.google.com/maps/documentation/distancematrix/

    -- Ollie Frolovs
*)

open Core.Std
open Lwt

(** Input/Output related functions *)
(* prune any whitespace-only or empty lines from the input *)
let prune list = 
  list
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun x -> not (String.is_empty x))

(* we consider any non-blank line a valid input and prune any other input *)
let read_input ic = prune (In_channel.input_lines ic)

let choose_input_channel ifile = 
  match ifile with
      "-"      -> In_channel.stdin
    | filename -> In_channel.create filename

let choose_output_channel ofile = 
  match ofile with
      "-" -> Out_channel.stdout
    | _   -> Out_channel.stdout

(** End of input/output related functions *)




(** Uri construction related functions *)
(* Google API wants parameters separated by pipe rather than a comma.
   This is a violation of the RFC. 
   Uri does not provide for this as of 1.3.10
   But Google API seems to be happy with character escaped version. *)
(* FIXME rfc does not actually disallow pipes as long as they are % encoded *)
let anti_rfc_fy s = String.concat ~sep:"|" s

let base_uri_string = "https://maps.googleapis.com/maps/api/distancematrix/json?"

(* creates the uri to query distances from town t to list of nodes ts *)
let create_uri_for t ts = 
  let base_uri = Uri.of_string base_uri_string in
  Uri.add_query_params base_uri [("origins", [t]);
				 ("destinations", [anti_rfc_fy ts]);
				 ("sensor", ["false"])]

(** End of uri construction related functions *)




(** HTTP client functions *)
(* FIXME this is where the connection might fail; upd: dafuq, why exception when it returns option? *)
let run_matrix_query uri =
  let response = Lwt_main.run (Cohttp_lwt_unix.Client.get uri) in
  match response with
      None -> failwith "error: request failed -- any reason"
    | Some (_,b) -> Lwt_main.run (Cohttp_lwt_body.string_of_body b)

(** End of HTTP client functions *)





(** JSON parsing related functions *)
(* TODO should JSON related code not be in a separate file *)

let pretty_print_json json = Yojson.Basic.pretty_to_channel Out_channel.stdout json

(* FIXME more complex type needed? *)
(*
type response = Failure of string | element
*)
type element = {
  status : string;
  distance : int;
  duration : int;
  name : string
}

(* FIXME how to avoid this ugly hack with name *)
(* FIXME why pattern matching does not work inside records? *)
let json_element e = Yojson.Basic.Util.(
  let status = e |> member "status" |> to_string in {
    status;
    distance = if status = "OK" then e |> member "distance" |> member "value" |> to_int else 0;
    duration = if status = "OK" then e |> member "duration" |> member "value" |> to_int else 0;
    name = ""
})

let bootstrap_json_from body = Yojson.Basic.from_string body

let origin_from_json json =  Yojson.Basic.Util.(
  match List.hd (json |> member "origin_addresses" |> to_list |> filter_string) with
      None -> "???"
    | Some x -> x
)

(* TODO json toplevel status must be "OK", otherwise give a warning *)
(* TODO json element status must be "OK", otherwise give a warning *)
(* FIXME this p-arser expects the matrix to be a row vector. works for me, but not clever *)

let process_response json = 
  let open Yojson.Basic.Util in
  let status = json |> member "status" |> to_string in
  match status with
      "OK" ->
	(*  let origin_addresses = json |> member "origin_addresses" |> to_list |> filter_string in *)
	let destination_addresses = json |> member "destination_addresses" |> to_list |> filter_string in
	let row = List.hd (json |> member "rows" |> to_list) in
	let elements = match row with 
	    None -> failwith "empty row"
	  | Some (row) -> row |> member "elements" |> to_list in 
	let data = List.map2_exn destination_addresses elements 
	  ~f:(fun destination json -> { (json_element json) with name = destination }) in
	(status, Some data)
    | _ -> (status, None)

(** End of JSON functions*)




type outcome = {
  destination : string;
  result : element
}

(* TODO should these be optional command line parameters? *)
(* FIXME stupid limitation of 100 elements per 10 sec; yet good enough for our project *)
let limit = 100  (* elements = origins * destinations *)
let timeout = 10 (* seconds *)

(*
let download_all_distances ifile ofile () = 
  let outcome = 
    let nodes = read_input_f ifile in
    let _ = begin
      eprintf "-- read input, N = %i\n" (List.length nodes);
      if List.length nodes >= 100 then failwith "can only linearize less than 100 elements";
      eprintf "-- downloading data for:\n%!"
    end in 


    let rec get_distances t (result, nodes, processed) = 
      if processed + (List.length nodes) >= limit
      then 
	begin 
	  eprintf "-- sleeping for %i seconds\n%!" timeout;
	  Unix.sleep timeout; 
	  get_distances t (result, nodes, 0) 
	end 
      else
	let _ = eprintf "   -- %s... %!" t in
	let uri = (create_uri_for t nodes) in
	let response = run_matrix_query uri in
	let json = bootstrap_json_from response in
	let (status, r) = process_response json in
	let _ = eprintf "%s\n%!" status in
	((origin_from_json json, r)::result, nodes, processed + List.length nodes)
    in match List.fold_right nodes ~f:get_distances ~init:([], nodes, 0) with (o,_,_) -> o
  in let _ = eprintf "-- download complete\n%!" in

     let _ = List.iter outcome ~f:(fun (origin,destinations) -> match destinations with
	 None -> printf "ERROR \n%!"
       | Some destinations ->
	 List.iter destinations 
	   ~f:(fun destination -> if origin <> destination.name then printf "%s|%s|%s|%i|%i\n%!" 
	       destination.status 
	       origin 
	       destination.name
	       destination.distance
	       destination.duration)) in ()
*)
 
(* TODO what is the desired format for the output? *)

let download_all_distances ifile ofile () =
  let ic = choose_input_channel  ifile in
  let oc = choose_output_channel ofile in
  let locations = read_input ic in
  let number_of_locations = List.length locations in
  let () = if number_of_locations >= limit then failwith 
      (sprintf "can only download the data for 100 or fewer locations, you have %i" number_of_locations) in
  let () = eprintf "-- read %i node%s\n%!" number_of_locations (if number_of_locations = 1 then "" else "s") in
(*  List.iter locations ~f:(fun x -> eprintf "-- Location: %s\n%!" x) *)
  let distance_matrix = [
    { status = "OK"; name = "University of Gruffalo"; distance = 1;   duration = 2 };
    { status = "OK"; name = "University of Moust";    distance = 100; duration = 120 };
    { status = "OK"; name = "University of Boofle";   distance = 200; duration = 220 }
  ] in
  List.iter distance_matrix ~f:(fun x -> fprintf oc "%s|%s|%i|%i\n" x.status x.name x.distance x.duration);
  Out_channel.flush oc
  (* FIXME how can i close oc if i don't know it it's stdout? *)

let command = 
  Command.basic
    ~summary:"Download distance/duration for trips between given locations from Google Distance Matrix"
    Command.Spec.(
      empty 
      +> flag "-input"  (optional_with_default "-" file)  ~doc:"file input file"
      +> flag "-output" (optional_with_default "-" file)  ~doc:"file output file"
    )
    download_all_distances

    
let () = Command.run ~version:"1.0" ~build_info:"olliefr" command
