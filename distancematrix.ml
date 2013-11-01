(** Extract travel distance and time for a matrix of origins and destinations
    using JSON over HTTP.

    Google Distance Matrix
    https://developers.google.com/maps/documentation/distancematrix/

    -- Ollie Frolovs
*)

open Core.Std
open Lwt

(* Google API wants parameters separated by pipe rather than a comma.
   This is a violation of the RFC. 
   Uri does not provide for this as of 1.3.10
   But Google API seems to be happy with character escaped version. *)
let anti_rfc_fy s = String.concat ~sep:"|" s

let base_uri_string = "https://maps.googleapis.com/maps/api/distancematrix/json?"

(* TODO these be read from a file or standard input *)
(* TODO read from standard input *)
(* TODO if --input file is given, read from file *)


(* prune any whitespace-only or empty lines from the input *)
let prune list = 
  list
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun x -> not (String.is_empty x))

(* FIXME error be here if file not found *)
(* TODO use with_file instead *)
let read_input_f name =
  let file = In_channel.create name in
  let nodes = In_channel.input_lines file in
  In_channel.close file;
  nodes

(* FIXME should this not be merged with read_input_f somehow? *)
(* we consider any non-blank line a valid input and prune any other input *)
let read_input_stdin = 
  In_channel.input_lines stdin
  |> prune


(* creates the uri to query distances from town t to list of nodes ts *)
let create_uri_for t ts = 
  let base_uri = Uri.of_string base_uri_string in
  Uri.add_query_params base_uri [("origins", [t]);
				 ("destinations", [anti_rfc_fy ts]);
				 ("sensor", ["false"])]

(* FIXME this is where the connection might fail *)
let run_matrix_query uri =
  let response = Lwt_main.run (Cohttp_lwt_unix.Client.get uri) in
  match response with
      None -> failwith "error: request failed -- any reason"
    | Some (_,b) -> Lwt_main.run (Cohttp_lwt_body.string_of_body b)

(* TODO should JSON related code not be in a separate file *)

let pretty_print_json json = Yojson.Basic.pretty_to_channel Out_channel.stdout json

type element = {
  status : string;
  distance : int;
  duration : int
}

let json_element e = Yojson.Basic.Util.({
  status = e |> member "status" |> to_string;
  distance = e |> member "distance" |> member "value" |> to_int;
  duration = e |> member "duration" |> member "value" |> to_int
})

let bootstrap_json_from body = Yojson.Basic.from_string body

let process_response json = 

(* TODO json toplevel status must be "OK", otherwise give a warning *)
(* TODO json element status must be "OK", otherwise give a warning *)
(* FIXME this p-arser expects the matrix to be a row vector. works for me, but not clever *)
  let open Yojson.Basic.Util in
  let status = json |> member "status" |> to_string in
  let origin_addresses = json |> member "origin_addresses" |> to_list |> filter_string in
  let destination_addresses = json |> member "destination_addresses" |> to_list |> filter_string in
  let row = List.hd (json |> member "rows" |> to_list) in
  let elements = match row with 
      None -> failwith "empty row"
    | Some (row) -> row |> member "elements" |> to_list in 
  let data = (List.map elements ~f:json_element) in
  (status, data)

(*
printf "-- Status: %s\n" status;
printf "-- Origins: %s\n" (String.concat ~sep:", " origin_addresses);
printf "-- Destinations: %s\n" (String.concat ~sep:", " destination_addresses);
printf "-- Rows: %i\n" (List.length rows);
printf "-- Data: %i\n" (List.length data);
printf "Status,Distance(m),Duration(s),Destination\n";
List.iteri data 
  ~f:(fun k x -> printf "%s,%i,%i,\"%s\"\n" x.status x.distance x.duration (List.nth_exn destination_addresses k))
*)

type outcome = {
  destination : string;
  result : element
}


(* TODO report progress along the way *)

(* status report after reading the nodes *)
(*
let _ = 
  eprintf "--input complete, N = %i\n" (List.length inp);
  eprintf "-- attempting network connection now\n%!"
*)

(* FIXME stupid limitation on size yet good enough for our project *)
(*
let () = if List.length nodes >= 100 then failwith "can only linearize less than 100 elements"
*)

(* TODO take breaks every 100 elements, or the requests will fail *)
let outcome = 
  let nodes = read_input_stdin in
  let get_distances t (result, nodes) = 
    let uri = (create_uri_for t nodes) in
    let response = run_matrix_query uri in
    let json = bootstrap_json_from response in
    let (status, r) = process_response json in
    ((t, r)::result, nodes)
  in 
  List.fold_right nodes ~f:get_distances ~init:([], nodes)

(* TODO output to the standard output *)
(* TODO if --output file is given, write to the file *)
(* TODO but what is the desired format for the output? *)
