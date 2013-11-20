(** Extract travel distance and time for a matrix of origins and destinations
    using JSON over HTTP.

    Google Distance Matrix
    https://developers.google.com/maps/documentation/distancematrix/

    -- Ollie Frolovs
*)

open Core.Std
open Lwt

type route = {
  origin : string;
  destination : string;
  distance : int;
  duration : int
}

(** Input/Output related functions *)
(* prune any whitespace-only or empty lines from the input *)
(* FIXME this is used in other files, move into utilities? *)
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
    | _   -> Out_channel.create ofile (* FIXME now i have not to forget to close it; better options? *)

(* TODO what is the desired format for the output? *)
let print_distance_matrix distance_matrix oc = 
  List.iter distance_matrix ~f:(fun y ->
    List.iter y ~f:(fun x -> match x with 
	Error err -> fprintf oc "error %s\n" err
      | Ok x -> if x.origin <> x.destination 
	        then fprintf oc "%s|%s|%i|%i\n" x.origin x.destination x.distance x.duration)
  );
  Out_channel.flush oc;
  if oc <> Out_channel.stdout && oc <> Out_channel.stdout then Out_channel.close oc
(* FIXME is there a better way to ignore 'special' output channels? *)

(** End of input/output related functions *)

(** Uri construction related functions *)
(* Google API wants parameters separated by pipe rather than a comma.
   Uri does not provide for this as of 1.3.10
   But Google API seems to be happy with url-encoded version. *)
let concat_with_pipe s = String.concat ~sep:"|" s

let base_uri_string = "https://maps.googleapis.com/maps/api/distancematrix/json?"

(* creates the uri to query distances from town t to list of nodes ts *)
let create_uri_for t locations = 
  let base_uri = Uri.of_string base_uri_string in
  Uri.add_query_params base_uri [("origins", [t]);
				 ("destinations", [concat_with_pipe locations]);
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

(* FIXME this p-arser expects the matrix to be a row vector. works for me, but not clever *)
(* FIXME if the server returns empty string, this will fail; what to do? *)
(* Assumption: the locations have been validated beforehand; we use original names, not those returned by the Matirx *)
let locations_from_json s origin destinations = 
  let json = Yojson.Basic.from_string s in
  let open Yojson.Basic.Util in
  match json |> member "status" |> to_string with
      "OK" ->
	let elements = 
	  [json] 
            |> filter_member "rows"
            |> flatten
            |> filter_member "elements"
            |> flatten
	in Ok (List.map2_exn destinations elements 
		 ~f:(fun destination element -> 
		   match element |> member "status" |> to_string with
		       "OK" -> Ok {
			 origin;
			 destination;
			 distance =  element |> member "distance" |> member "value" |> to_int;
			 duration =  element |> member "duration" |> member "value" |> to_int
		       }
		     | err -> Error err))
    | err -> Error err
	
(** End of JSON functions*)



(* TODO should these be optional command line parameters? *)
(* FIXME stupid limitation of 100 elements per 10 sec; yet good enough for our project *)
let limit = 100  (* elements = origins * destinations *)
let timeout = 12 (* seconds *)

(* runs a query for one location *)
let query_location location locations =
  let uri = create_uri_for location locations in
  let json = run_matrix_query uri in
  locations_from_json json location locations

(* TODO put failwith/error in a separate function, we only care about OVER_QUERY_LIMIT here *)
let rec query_location_with_retry location locations = 
  match query_location location locations with
      Ok distance_vector -> distance_vector
    | Error "INVALID_REQUEST" -> failwith "invalid request"
    | Error "MAX_ELEMENTS_EXCEEDED" -> failwith "the product of origins and destinations exceeds the per-query limit"
    | Error "OVER_QUERY_LIMIT" -> begin
      eprintf "-- query limit exceeded; sleeping for %is\n%!" timeout;
      Unix.sleep timeout;
      query_location_with_retry location locations
    end
    | Error "REQUEST_DENIED" -> failwith "the service denied use of the Distance Matrix service"
    | Error "UNKNOWN_ERROR" -> failwith "server error"
    | Error x -> failwith x

let download_all_distances ifile ofile () =
  let ic = choose_input_channel  ifile in
  let oc = choose_output_channel ofile in
  let locations = read_input ic in
  let number_of_locations = List.length locations in
  let () = if number_of_locations >= limit then failwith 
      (sprintf "can only download the data for 100 or fewer locations, you have %i" number_of_locations) in
  let () = eprintf "-- read %i node%s\n%!" number_of_locations (if number_of_locations = 1 then "" else "s") in
  let distance_matrix = List.map locations ~f:(fun location -> query_location_with_retry location locations) in
  print_distance_matrix distance_matrix oc
    
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
