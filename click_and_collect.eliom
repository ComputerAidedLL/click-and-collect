[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_parameter
]

open Parse_sequent
open Apply_rule
open Is_proof_complete
open Export_as_coq
open Export_as_latex
open Yojson


(*********)
(* UTILS *)
(*********)

let send_json ~code json =
  Eliom_registration.String.send ~code (json, "application/json")

let send_file ~code file_as_string =
  Eliom_registration.String.send ~code (file_as_string, "text/plain")

let read_raw_content raw_content =
  let content_stream = Ocsigen_stream.get raw_content in
  (* Servers POST body size limit is usually 2MB *)
  Ocsigen_stream.string_of_stream 524288 content_stream

let uncompress_json request_as_string =
    let abbreviations = [("\"s\":", "\"sequent\":");
                        ("\"ar\":", "\"appliedRule\":");
                        ("\"rr\":", "\"ruleRequest\":");
                        ("\"p\":", "\"premises\":");
                        ("\"r\":", "\"rule\":");
                        ("\"fp\":", "\"formulaPosition\":");
                        ("\"t\":", "\"type\":");
                        ("\"v\":", "\"value\":");
                        ("\"v1\":", "\"value1\":");
                        ("\"v2\":", "\"value2\":")] in
    let s = ref request_as_string in
    List.iter (function (compressed_field, field) ->
        s := Str.global_replace (Str.regexp compressed_field) field !s) abbreviations;
    !s

let post_handler raw_content_opt function_on_json =
    match raw_content_opt with
    | None -> send_json ~code:400 "Body content is missing"
    | Some raw_content -> Lwt.catch (fun () ->
        read_raw_content raw_content >>= fun request_as_string ->
        try
            let request_as_json = Yojson.Basic.from_string (uncompress_json request_as_string) in
            function_on_json request_as_json
        with Yojson.Json_error s -> send_json ~code:400 "Body content is not a valid json")
        (function
        | Ocsigen_stream.String_too_large -> send_json ~code:400 "Body content is too big"
        | _ as e -> raise e)


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
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, json_response = apply_rule request_as_json in
        if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
        else send_json ~code:400 (Yojson.Basic.to_string json_response))

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
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, json_response = is_proof_complete request_as_json in
        if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
        else send_json ~code:400 (Yojson.Basic.to_string json_response))

let () =
  Eliom_registration.Any.register is_proof_complete_service is_proof_complete_handler;
  ()

(*****************)
(* EXPORT AS COQ *)
(*****************)

(* Service declaration *)
let export_as_coq_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["export_as_coq"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let export_as_coq_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, string_response = export_as_coq request_as_json in
        if technical_success then send_file ~code:200 string_response
        else send_json ~code:400 string_response)

let () =
  Eliom_registration.Any.register export_as_coq_service export_as_coq_handler;
  ()

(*******************)
(* EXPORT AS LATEX *)
(*******************)

(* Service declaration *)
let export_as_latex_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["export_as_latex"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let export_as_latex_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, string_response = export_as_latex request_as_json in
        if technical_success then send_file ~code:200 string_response
        else send_json ~code:400 string_response)

let () =
  Eliom_registration.Any.register export_as_latex_service export_as_latex_handler;
  ()
