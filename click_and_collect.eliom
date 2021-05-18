[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Eliom_parameter
]

open Parse_sequent
open Apply_rule
open Export_as_coq
open Export_as_latex
open Is_sequent_provable
open Auto_reverse_sequent
open Auto_prove_sequent
open Proof_compression
open Yojson


(***********)
(* LOGGING *)
(***********)

let () =
  Lwt_log.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ();

  Lwt_log.add_rule "*" Lwt_log.Warning

(*********)
(* UTILS *)
(*********)

let get_boolean_value = function
    | None -> false
    | Some s -> s = "true" || s = "1"

let send_json ~code json =
  Eliom_registration.String.send ~code (json, "application/json")

let send_file ~code file_as_string file_type =
  Eliom_registration.String.send ~code (file_as_string, file_type)

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
                ("proof", result)
            ] else `Assoc [
                ("is_valid", `Bool false);
                ("error_message", result)
            ] in
        Lwt.return (Yojson.to_string response, "application/json"));;


(*****************)
(* PARSE FORMULA *)
(*****************)

(* Service declaration *)
let parse_formula_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["parse_formula"])
    ~meth:(Eliom_service.Get (Eliom_parameter.string "formulaAsString"))
    ()

(* Service definition *)
let _ =
  Eliom_registration.String.register
    ~service:parse_formula_service
    (fun formula_as_string () ->
      let success, result = safe_parse_formula formula_as_string in
        let response =
            if success then `Assoc [
                ("is_valid", `Bool true);
                ("formula", result)
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
        if technical_success then send_file ~code:200 string_response "text/plain"
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
      ~meth:(Eliom_service.Post (Eliom_parameter.prod (Eliom_parameter.string "format") (Eliom_parameter.opt (Eliom_parameter.string "implicitExchange")), Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let export_as_latex_handler (format, implicit_exchange_opt) (content_type, raw_content_opt) =
    let implicit_exchange = get_boolean_value implicit_exchange_opt in
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, string_response, file_type = export_as_latex implicit_exchange format request_as_json in
        if technical_success then send_file ~code:200 string_response file_type
        else send_json ~code:400 string_response)

let () =
  Eliom_registration.Any.register export_as_latex_service export_as_latex_handler;
  ()

(***********************)
(* IS SEQUENT PROVABLE *)
(***********************)

(* Service declaration *)
let is_sequent_provable_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["is_sequent_provable"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let is_sequent_provable_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, json_response = is_sequent_provable request_as_json in
        if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
        else send_json ~code:400 (Yojson.Basic.to_string json_response))

let () =
  Eliom_registration.Any.register is_sequent_provable_service is_sequent_provable_handler;
  ()

(************************)
(* AUTO REVERSE SEQUENT *)
(************************)

(* Service declaration *)
let auto_reverse_sequent_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["auto_reverse_sequent"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let auto_reverse_sequent_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, json_response = auto_reverse_sequent request_as_json in
        if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
        else send_json ~code:400 (Yojson.Basic.to_string json_response))

let () =
  Eliom_registration.Any.register auto_reverse_sequent_service auto_reverse_sequent_handler;
  ()

(**********************)
(* AUTO PROVE SEQUENT *)
(**********************)

(* Service declaration *)
let auto_prove_sequent_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["auto_prove_sequent"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let auto_prove_sequent_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, json_response = auto_prove_sequent request_as_json in
        if technical_success then send_json ~code:200 (Yojson.Basic.to_string json_response)
        else send_json ~code:400 (Yojson.Basic.to_string json_response))

let () =
  Eliom_registration.Any.register auto_prove_sequent_service auto_prove_sequent_handler;
  ()

(******************)
(* COMPRESS PROOF *)
(******************)

(* Service declaration *)
let compress_proof_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["compress_proof"])
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.raw_post_data))
      ()

(* Service definition *)
let compress_proof_handler () (content_type, raw_content_opt) =
    post_handler raw_content_opt (function request_as_json ->
        let technical_success, string_response = compress_proof request_as_json in
        if technical_success then send_file ~code:200 string_response "text/plain"
        else send_json ~code:400 string_response)

let () =
  Eliom_registration.Any.register compress_proof_service compress_proof_handler;
  ()


(********************)
(* UNCOMPRESS PROOF *)
(********************)

(* Service declaration *)
let uncompress_proof_service =
  Eliom_service.create
      ~path:(Eliom_service.Path ["uncompress_proof"])
      ~meth:(Eliom_service.Get (Eliom_parameter.string "compressedProof"))
      ()

(* Service definition *)
let _ =
  Eliom_registration.String.register
    ~service:uncompress_proof_service
    (fun compressed_proof () ->
      let success, result = uncompress_proof compressed_proof in
        let response =
            if success then `Assoc [
                ("is_valid", `Bool true);
                ("proof", result)
            ] else `Assoc [
                ("is_valid", `Bool false);
                ("error_message", result)
            ] in
        Lwt.return (Yojson.to_string response, "application/json"));;
