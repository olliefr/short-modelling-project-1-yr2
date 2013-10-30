(** Extract travel distance and time for a matrix of origins and destinations
    using JSON over HTTP.

    Google Distance Matrix
    https://developers.google.com/maps/documentation/distancematrix/

    -- Ollie Frolovs
*)

open Core.Std
open Lwt

let base_uri_string = "https://maps.googleapis.com/maps/api/distancematrix/json?"

(* TODO these be read from file *)
let origins = ["Bristol"]
let destinations = ["Bristol"; "London"; "Cambridge UK"; "Edinburgh"]

(* Google API wants parameters separated by pipe rather than a comma.
   This is a violation of the RFC. 
   Uri does not provide for this as of 1.3.10
   But Google API seems to be happy with character escaped version. *)
let anti_rfc_fy s = String.concat ~sep:"|" s

(* TODO mind the limits *)

let base_uri = Uri.of_string base_uri_string
let uri = Uri.add_query_params base_uri [("origins", [anti_rfc_fy origins]);
					 ("destinations", [anti_rfc_fy destinations]);
					 ("sensor", ["false"])]

let response = Lwt_main.run (Cohttp_lwt_unix.Client.get uri)
let body = match response with
    None -> failwith "error: request failed -- any reason"
  | Some (_,b) -> Lwt_main.run (Cohttp_lwt_body.string_of_body b)

(* TODO json be p-arsed *)
(*
let () = printf "%s" body
*)

(* TODO should this not be in a separate file *)
type element = {
  status : string;
  distance: int;
  duration: int
}
let json_element e = Yojson.Basic.Util.({
  status = e |> member "status" |> to_string;
  distance = e |> member "distance" |> member "value" |> to_int;
  duration = e |> member "duration" |> member "value" |> to_int
})


let json = Yojson.Basic.from_string body

(* TODO json toplevel status must be "OK", otherwise give a warning *)
(* TODO json element status must be "OK", otherwise give a warning *)
(* FIXME this p-arser expects the matrix to be a row vector. works for me, but not clever *)
let open Yojson.Basic.Util in
let status = json |> member "status" |> to_string in
let origin_addresses = json |> member "origin_addresses" |> to_list |> filter_string in
let destination_addresses = json |> member "destination_addresses" |> to_list |> filter_string in
let rows = json |> member "rows" |> to_list in
let elements = List.hd (List.map rows ~f:(fun elements -> elements |> member "elements" |> to_list)) in
let data = match elements with
    None -> failwith "empty elements"
  | Some (elements) -> List.map elements ~f:json_element in
printf "-- Status: %s\n" status;
printf "-- Origins: %s\n" (String.concat ~sep:", " origin_addresses);
printf "-- Destinations: %s\n" (String.concat ~sep:", " destination_addresses);
printf "-- Rows: %i\n" (List.length rows);
printf "-- Data: %i\n" (List.length data);
printf "Status,Distance(m),Duration(s),Destination\n";
List.iteri data 
  ~f:(fun k x -> printf "%s,%i,%i,\"%s\"\n" x.status x.distance x.duration (List.nth_exn destination_addresses k))
