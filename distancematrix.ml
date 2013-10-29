(** Extract travel distance and time for a matrix of origins and destinations
    using JSON over HTTP.

    Google Distance Matrix
    https://developers.google.com/maps/documentation/distancematrix/

    -- Ollie Frolovs
*)

open Core.Std
open Lwt

(* TODO these be parameters *)
let base_uri_string = "https://maps.googleapis.com/maps/api/distancematrix/json?"
let origins = ["Bristol"; "London"; "Cambridge"]
let destinations = origins

(* Google API wants parameters separated by pipe rather than a comma.
   This is a violation of the RFC. 
   Uri does not provide for this as of 1.3.10
   But Google API seems to be happy with character escaped version. *)
let anti_rfc_fy s = String.concat ~sep:"|" s

let base_uri = Uri.of_string base_uri_string
let uri = Uri.add_query_params base_uri [("origins", [anti_rfc_fy origins]);
					 ("destinations", [anti_rfc_fy destinations]);
					 ("sensor", ["false"])]

let response = Lwt_main.run (Cohttp_lwt_unix.Client.get uri)
let body = match response with
    None -> assert false
  | Some (_,b) -> Lwt_main.run (Cohttp_lwt_body.string_of_body b)

(* TODO json be p-arsed *)
let () = printf "%s" body
