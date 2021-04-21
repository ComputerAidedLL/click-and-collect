open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util

(* The utils function *)
let call_api_get path =
    let body =
      let complete_uri = "http://localhost:8080/" ^ path in
      Client.get (Uri.of_string complete_uri) >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Alcotest.(check int) "returns 200" 200 code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

let call_api_post path body_as_string expected_code =
    let body =
      let complete_uri = "http://localhost:8080/" ^ path in
      let body = Cohttp_lwt.Body.of_string body_as_string in
      Client.post ~body:body (Uri.of_string complete_uri) >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Alcotest.(check int) ("status code for " ^ body_as_string) expected_code code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

(* The tests *)
let call_api_parse_sequent_full_response () =
    Alcotest.(check string) "valid" "{\"is_valid\":true,\"sequent_as_json\":{\"cons\":[{\"type\":\"litt\",\"value\":\"a\"}]}}" (call_api_get "parse_sequent?sequentAsString=a");
    Alcotest.(check string) "invalid" "{\"is_valid\":false,\"error_message\":\"Syntax error: please read the syntax rules.\"}" (call_api_get "parse_sequent?sequentAsString=a*")

let call_api_parse_sequent () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_parse_sequent" |> to_list in
    let run_test test_sample =
        let sequent_as_string = test_sample |> member "sequent_as_string" |> to_string in
        let expected_sequent_as_json = test_sample |> member "expected_sequent_as_json" in
        let body = call_api_get ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
        let data = Yojson.Basic.from_string body in
        let is_valid = data |> member "is_valid" |> to_bool in
        Alcotest.(check bool) (sequent_as_string ^ " is valid") true is_valid;
        let sequent_as_json = data |> member "sequent_as_json" in
        Alcotest.(check string) ("check sequent returned for " ^ sequent_as_string) (Yojson.Basic.to_string expected_sequent_as_json) (Yojson.Basic.to_string sequent_as_json) in
    List.iter run_test test_samples

let call_api_parse_sequent_syntax_exception () =
    let assert_syntax_exception sequent_as_string =
        let body = call_api_get ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
        let data = Yojson.Basic.from_string body in
        let is_valid = data |> member "is_valid" |> to_bool in
        Alcotest.(check bool) (sequent_as_string ^ " is invalid") false is_valid in
    assert_syntax_exception "a*";
    assert_syntax_exception "a|-a|-";
    assert_syntax_exception "|-a|-a";
    assert_syntax_exception "|-|-";
    assert_syntax_exception "|-,a";
    assert_syntax_exception "|-a,";
    assert_syntax_exception ",,";
    assert_syntax_exception "a^,~a";
    assert_syntax_exception "!a,b\\/c";
    assert_syntax_exception "2"

let call_api_apply_rule_full_response () =
    let body_as_string = "{\"rr\":{\"r\": \"par\", \"fp\":0}, \"s\": {\"cons\": [{\"t\": \"par\", \"v1\":{\"t\": \"litt\", \"v\":\"a\"},\"v2\":{\"t\": \"litt\", \"v\":\"a\"}}]}}" in
    let response_as_string = call_api_post "apply_rule" body_as_string 200 in
    let expected_response_as_string = "{\"success\":true,\"premises\":[{\"sequent\":{\"cons\":[{\"type\":\"litt\",\"value\":\"a\"},{\"type\":\"litt\",\"value\":\"a\"}]},\"appliedRule\":null}]}" in
    Alcotest.(check string) "valid" expected_response_as_string response_as_string

let call_api_apply_rule () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_apply_rule" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_premises = test_sample |> member "expected_premises" in
        let response_as_string = call_api_post "apply_rule" (Yojson.Basic.to_string request_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" true success;
        let premises = response_as_json |> member "premises" in
        Alcotest.(check string) "check sequent list returned" (Yojson.Basic.to_string expected_premises) (Yojson.Basic.to_string premises) in
    List.iter run_test test_samples

let call_api_apply_rule_technical_exception () =
    let assert_technical_exception body_as_string =
        let body = call_api_post "apply_rule" body_as_string 400 in
        Alcotest.(check bool) "response not null" true (0 < String.length body) in
    assert_technical_exception "{";
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_apply_rule_technical_exception" |> to_list in
    let run_test test_sample =
        assert_technical_exception (Yojson.Basic.to_string test_sample) in
    List.iter run_test test_samples

let call_api_apply_rule_logic_exception () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_apply_rule_logic_exception" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_error_message = test_sample |> member "expected_error_message" |> to_string in
        let response_as_string = call_api_post "apply_rule" (Yojson.Basic.to_string request_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" false success;
        let error_message = response_as_json |> member "errorMessage" |> to_string in
        Alcotest.(check string) "check errorMessage" expected_error_message error_message in
    List.iter run_test test_samples

let call_api_apply_rule_auto_reverse_mode_full_response () =
    let body_as_string = "{\"rr\":{\"r\": \"par\", \"fp\":0}, \"s\": {\"cons\": [{\"t\": \"par\", \"v1\":{\"t\": \"litt\", \"v\":\"a\"},\"v2\":{\"t\": \"dual\", \"v\":{\"t\": \"litt\", \"v\":\"a\"}}}]}}" in
    let response_as_string = call_api_post "apply_rule?auto_reverse_mode=true" body_as_string 200 in
    let expected_response_as_string = "{\"success\":true,\"premises\":[{\"sequent\":{\"cons\":[{\"type\":\"litt\",\"value\":\"a\"},{\"type\":\"dual\",\"value\":{\"type\":\"litt\",\"value\":\"a\"}}]},\"appliedRule\":{\"ruleRequest\":{\"rule\":\"axiom\"},\"premises\":[]}}]}" in
    Alcotest.(check string) "valid" expected_response_as_string response_as_string

let call_api_apply_rule_auto_reverse_mode () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_apply_rule_auto_reverse" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_premises = test_sample |> member "expected_premises" in
        let response_as_string = call_api_post "apply_rule?auto_reverse_mode=true" (Yojson.Basic.to_string request_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" true success;
        let premises = response_as_json |> member "premises" in
        Alcotest.(check string) "check sequent list returned" (Yojson.Basic.to_string expected_premises) (Yojson.Basic.to_string premises) in
    List.iter run_test test_samples

let call_api_is_proof_complete_full_response () =
    let body_as_string = "{\"s\":{\"cons\": [{\"t\":\"litt\",\"v\":\"a\"},{\"t\":\"dual\",\"v\":{\"t\":\"litt\",\"v\":\"a\"}}]},\"appliedRule\":{\"rr\":{\"r\":\"axiom\"},\"premises\":[]}}" in
    let response_as_string = call_api_post "is_proof_complete" body_as_string 200 in
    let expected_response_as_string = "{\"is_complete\":true}" in
    Alcotest.(check string) "valid" expected_response_as_string response_as_string

let call_api_is_proof_complete () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_is_proof_complete" |> to_list in
    let run_test test_sample =
        let response_as_string = call_api_post "is_proof_complete" (Yojson.Basic.to_string test_sample) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "is_complete" |> to_bool in
        Alcotest.(check bool) "is_complete" true success in
    List.iter run_test test_samples

let call_api_is_proof_complete_exception () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_is_proof_complete_exception" |> to_list in
    let run_test test_sample =
        let response = call_api_post "is_proof_complete" (Yojson.Basic.to_string test_sample) 400 in
        Alcotest.(check bool) "not empty response" true (response <> "") in
    List.iter run_test test_samples

let call_api_test_png () =
    let body_as_string = "{\"s\":{\"cons\": [{\"t\":\"litt\",\"v\":\"a\"},{\"t\":\"dual\",\"v\":{\"t\":\"litt\",\"v\":\"a\"}}]},\"ar\":{\"rr\":{\"r\":\"axiom\"},\"p\":[]}}" in
    let response_as_string = call_api_post "export_as_latex?format=png" body_as_string 200 in
    Alcotest.(check bool) "not empty response" true (response_as_string <> "")

let call_api_sequent_is_provable () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_sequent_is_provable" |> to_list in
    let run_test test_sample =
        let sequent_as_json = test_sample |> member "sequent_as_json" in
        let expected_provability = test_sample |> member "expected_provability" |> to_bool in
        let response_as_string = call_api_post "is_sequent_provable" (Yojson.Basic.to_string sequent_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let provable = response_as_json |> member "is_provable" |> to_bool in
        Alcotest.(check bool) "check provability" provable expected_provability in
    List.iter run_test test_samples

let test_parse_sequent = [
    "Test full response", `Quick, call_api_parse_sequent_full_response;
    "Test sequent", `Quick, call_api_parse_sequent;
    "Test syntax exeption", `Quick, call_api_parse_sequent_syntax_exception;
]

let test_apply_rule = [
    "Test full response", `Quick, call_api_apply_rule_full_response;
    "Test sequent", `Quick, call_api_apply_rule;
    "Test technical exception", `Quick, call_api_apply_rule_technical_exception;
    "Test logic exception", `Quick, call_api_apply_rule_logic_exception;
    "Test auto reverse mode full response", `Quick, call_api_apply_rule_auto_reverse_mode_full_response;
    "Test auto reverse mode", `Quick, call_api_apply_rule_auto_reverse_mode;
]

let test_is_proof_complete = [
    "Test full response", `Quick, call_api_is_proof_complete_full_response;
    "Test proof", `Quick, call_api_is_proof_complete;
    "Test proof exception", `Quick, call_api_is_proof_complete_exception;
]

let test_export_as_latex = [
    "Test png", `Quick, call_api_test_png;
]

let test_sequent_is_provable = [
    "Test sequent is provable", `Quick, call_api_sequent_is_provable;
]

(* Run it *)
let () =
    Alcotest.run "API on localhost:8080" [
        "test_parse_sequent", test_parse_sequent;
        "test_apply_rule", test_apply_rule;
        "test_is_proof_complete", test_is_proof_complete;
        "test_export_as_latex", test_export_as_latex;
        "test_sequent_is_provable", test_sequent_is_provable;
    ]
