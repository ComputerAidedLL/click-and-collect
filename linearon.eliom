[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_parameter
]

open Parse_sequent
open Apply_rule
open Yojson

module Linearon_app =
  Eliom_registration.App (
  struct
    let application_name = "linearon"
    let global_data_path = None
  end)

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

(* Service declaration *)
let apply_rule_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["apply_rule"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let send_json ~code json =
  Eliom_registration.String.send ~code (json, "application/json")

let read_raw_content ?(length = 4096) raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  Ocsigen_stream.string_of_stream length content_stream

let apply_rule_handler () (content_type, raw_content_opt) =
    match raw_content_opt with
    | None -> send_json ~code:400 "Body content is missing"
    | Some raw_content -> read_raw_content raw_content >>= fun location_str ->
        try
            let request_as_json = Yojson.Basic.from_string location_str in
            let success, json_response = apply_rule request_as_json in
            if success then send_json ~code:200 (Yojson.Basic.to_string json_response)
            else send_json ~code:400 (Yojson.Basic.to_string json_response)
        with Yojson.Json_error s -> send_json ~code:400 "Body content is not a valid json"

let () =
  Eliom_registration.Any.register apply_rule_service apply_rule_handler;
  ()
