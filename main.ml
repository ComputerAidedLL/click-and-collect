open Opium
open Lwt.Syntax

(* UTILS *)

let bool_from_string s =
    s = "true" || s = "1"

let from_req req =
    let+ json = Request.to_json_exn req in
    Yojson.Basic.from_string (Proof_compression.uncompress_json (Yojson.Safe.to_string json))

let common_get_handler handler_function handler_param req =
    Lwt.catch (fun () ->
        Lwt.return (Response.of_json (handler_function (Router.param req handler_param))))
    (fun t -> Logs.err (fun m -> m "%s" (Printexc.to_string t));
        Request.pp_hum Format.std_formatter req; raise t)

let common_post_handler ok_response_method bad_request_response_method transform_message handler_function req =
    Lwt.catch (fun () ->
        try
            let+ request_as_json = from_req req in
            let technical_success, response = handler_function request_as_json in
            if technical_success then ok_response_method response
            else bad_request_response_method response
        with Yojson.Json_error _ -> Lwt.return (bad_request_response_method (transform_message "Body content is not a valid json")))
    (fun t -> Logs.err (fun m -> m "%s" (Printexc.to_string t));
        Request.pp_hum Format.std_formatter req; raise t)

let json_handler json_function req =
    common_post_handler (Response.of_json ~status:`OK) (Response.of_json ~status:`Bad_request) (fun s -> `String s) json_function req

let plain_text_handler plain_text_function req =
    common_post_handler (Response.of_plain_text ~status:`OK) (Response.of_plain_text ~status:`Bad_request) (fun s -> s) plain_text_function req

(* HANDLERS *)

let static_handler = Middleware.static_unix ~local_path:"./static" ~uri_prefix:"/static" ();;

let index_handler _request =
    Response.of_file "./index.html" ~mime:"text/html; charset=utf-8";;

let parse_sequent_handler req =
    common_get_handler Parse_sequent.safe_parse "sequent_as_string" req ;;

let parse_empty_sequent_handler _req =
    Lwt.return (Response.of_json (Parse_sequent.safe_parse ""));;

let parse_formula_handler req =
    common_get_handler Parse_sequent.safe_parse_formula "formula_as_string" req;;

let is_valid_litt_handler req =
    common_get_handler Parse_sequent.safe_is_valid_litt "litt" req;;

let is_sequent_provable_handler req =
    json_handler Is_sequent_provable.is_sequent_provable req

let apply_rule_handler req =
    json_handler Apply_rule.apply_rule req

let auto_reverse_handler req =
    json_handler Auto_reverse_sequent.auto_reverse_sequent req

let auto_prove_handler req =
    json_handler Auto_prove_sequent.auto_prove_sequent req

let compress_proof_handler req =
    json_handler Proof_compression.compress_proof req

let uncompress_proof_handler req =
    json_handler Proof_compression.uncompress_proof req;;

let export_as_coq_handler req =
    plain_text_handler Export_as_coq.export_as_coq req

let export_as_latex_handler req =
    let implicit_exchange = bool_from_string (Router.param req "implicit_exchange") in
    let format = Router.param req "format" in
    let+ response = plain_text_handler (Export_as_latex.export_as_latex implicit_exchange format) req in
    if format = "png" then Response.set_content_type "image/png" response
    else if format = "pdf" then Response.set_content_type "application/pdf" response
    else response

let get_proof_transformation_options_handler req =
    json_handler Proof_transformation.get_proof_transformation_options req

(** Configure the logger *)
let set_logger () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug)

let _ =
  set_logger ();
  App.empty
  |> App.middleware static_handler
  |> App.get "/" index_handler
  |> App.get "/parse_sequent/:sequent_as_string" parse_sequent_handler
  |> App.get "/parse_sequent/" parse_empty_sequent_handler
  |> App.get "/parse_formula/:formula_as_string" parse_formula_handler
  |> App.get "/is_valid_litt/:litt" is_valid_litt_handler
  |> App.post "/is_sequent_provable" is_sequent_provable_handler
  |> App.post "/apply_rule" apply_rule_handler
  |> App.post "/auto_reverse_sequent" auto_reverse_handler
  |> App.post "/auto_prove_sequent" auto_prove_handler
  |> App.post "/compress_proof" compress_proof_handler
  |> App.post "/uncompress_proof" uncompress_proof_handler
  |> App.post "/export_as_coq" export_as_coq_handler
  |> App.post "/export_as_latex/:format/:implicit_exchange" export_as_latex_handler
  |> App.post "/get_proof_transformation_options" get_proof_transformation_options_handler
  |> App.run_command
;;