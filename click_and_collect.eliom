[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_parameter
]

open Parse_sequent
open Apply_rule
open Is_proof_complete
open Yojson


(*********)
(* UTILS *)
(*********)

let send_json ~code json =
  Eliom_registration.String.send ~code (json, "application/json")

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream


(************)
(* MAIN APP *)
(************)

module Linearon_app =
  Eliom_registration.App (
  struct
    let application_name = "click_and_collect"
    let global_data_path = None
  end)


(*****************)
(* PARSE SEQUENT *)
(*****************)

(* Service declaration *)
let parse_sequent_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["parse_sequent"])
    ~meth:(Eliom_service.Get (Eliom_parameter.string "sequentAsString"))
    ()

(* Service definition *)
let _ =
  Eliom_registration.String.register
    ~service:parse_sequent_service
    (fun sequent_as_string () ->
      let success, result = safe_parse sequent_as_string in
        let response =
            if success then `Assoc [
                ("is_valid", `Bool true);
                ("sequent_as_json", result)
            ] else `Assoc [
                ("is_valid", `Bool false);
                ("error_message", result)
            ] in
        Lwt.return (Yojson.to_string response, "application/json"));;


(**************)
(* APPLY RULE *)
(**************)

(* Service declaration *)
let apply_rule_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["apply_rule"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let apply_rule_handler () (content_type, raw_content_opt) =
    match raw_content_opt with
    | None -> send_json ~code:400 "Body content is missing"
    | Some raw_content -> read_raw_content raw_content >>= fun request_as_string ->
        try
            let request_as_json = Yojson.Basic.from_string request_as_string in
            let technical_success, json_response = apply_rule request_as_json in
            if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
            else send_json ~code:400 (Yojson.Basic.to_string json_response)
        with Yojson.Json_error s -> send_json ~code:400 "Body content is not a valid json"

let () =
  Eliom_registration.Any.register apply_rule_service apply_rule_handler;
  ()


(*********************)
(* IS PROOF COMPLETE *)
(*********************)

(* Service declaration *)
let is_proof_complete_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["is_proof_complete"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let is_proof_complete_handler () (content_type, raw_content_opt) =
    match raw_content_opt with
    | None -> send_json ~code:400 "Body content is missing"
    | Some raw_content -> read_raw_content raw_content >>= fun request_as_string ->
        try
            let request_as_json = Yojson.Basic.from_string request_as_string in
            let technical_success, json_response = is_proof_complete request_as_json in
            if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
            else send_json ~code:400 (Yojson.Basic.to_string json_response)
        with Yojson.Json_error s -> send_json ~code:400 "Body content is not a valid json"

let () =
  Eliom_registration.Any.register is_proof_complete_service is_proof_complete_handler;
  ()
