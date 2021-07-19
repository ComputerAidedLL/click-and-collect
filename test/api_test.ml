open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util

(* The utils function *)
let url_encode_string s =
    Str.global_replace (Str.regexp "?") "%3F"
    (Str.global_replace (Str.regexp "/") "%2F" s)

let call_api_get path =
    let body =
      let uri = Uri.of_string ("http://localhost:3000/" ^ path) in
      Client.get uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Alcotest.(check int) (Printf.sprintf "returns 200 for path %s" path) 200 code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

let call_api_post path body_as_string expected_code =
    let body =
      let complete_uri = "http://localhost:3000/" ^ path in
      let body = Cohttp_lwt.Body.of_string body_as_string in
      Client.post ~body:body (Uri.of_string complete_uri) >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Alcotest.(check int) (Printf.sprintf "status code %d for path %s and body %s" expected_code path body_as_string) expected_code code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

(* The tests *)
let call_api_parse_sequent_full_response () =
    Alcotest.(check string) "valid" "{\"is_valid\":true,\"proof\":{\"sequent\":{\"cons\":[{\"type\":\"litt\",\"value\":\"a\"}]},\"appliedRule\":null}}" (call_api_get "parse_sequent/a");
    Alcotest.(check string) "invalid" "{\"is_valid\":false,\"error_message\":\"Syntax error: please read the syntax rules.\"}" (call_api_get "parse_sequent/a*")

let call_api_parse_sequent () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_parse_sequent" |> to_list in
    let run_test test_sample =
        let sequent_as_string = test_sample |> member "sequent_as_string" |> to_string in
        let expected_sequent_as_json = test_sample |> member "expected_sequent_as_json" in
        let body = call_api_get ("parse_sequent/" ^ (url_encode_string sequent_as_string)) in
        let data = Yojson.Basic.from_string body in
        let is_valid = data |> member "is_valid" |> to_bool in
        Alcotest.(check bool) (sequent_as_string ^ " is valid") true is_valid;
        let sequent_as_json = data |> member "proof" |> member "sequent" in
        Alcotest.(check string) ("check sequent returned for " ^ sequent_as_string) (Yojson.Basic.to_string expected_sequent_as_json) (Yojson.Basic.to_string sequent_as_json) in
    List.iter run_test test_samples

let call_api_parse_sequent_syntax_exception () =
    let assert_syntax_exception sequent_as_string =
        let body = call_api_get ("parse_sequent/" ^ (url_encode_string sequent_as_string)) in
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
        let premises = response_as_json |> member "proof" |> member "appliedRule" |> member "premises" in
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

let call_api_auto_reverse_full_response () =
    let body_as_string = "{\"sequent\":{\"cons\": [{\"t\": \"par\", \"v1\":{\"t\": \"litt\", \"v\":\"a\"},\"v2\":{\"t\": \"dual\", \"v\":{\"t\": \"litt\", \"v\":\"a\"}}}]},\"notations\":[]}" in
    let response_as_string = call_api_post "auto_reverse_sequent" body_as_string 200 in
    let expected_response_as_string = "{\"sequent\":{\"cons\":[{\"type\":\"par\",\"value1\":{\"type\":\"litt\",\"value\":\"a\"},\"value2\":{\"type\":\"dual\",\"value\":{\"type\":\"litt\",\"value\":\"a\"}}}]},\"appliedRule\":{\"ruleRequest\":{\"rule\":\"par\",\"formulaPosition\":0},\"premises\":[{\"sequent\":{\"cons\":[{\"type\":\"litt\",\"value\":\"a\"},{\"type\":\"dual\",\"value\":{\"type\":\"litt\",\"value\":\"a\"}}]},\"appliedRule\":{\"ruleRequest\":{\"rule\":\"axiom\"},\"premises\":[]}}]}}" in
    Alcotest.(check string) "valid" expected_response_as_string response_as_string

let call_api_auto_reverse () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_auto_reverse" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_proof = test_sample |> member "expected_proof" in
        let response_as_string = call_api_post "auto_reverse_sequent" (Yojson.Basic.to_string request_as_json) 200 in
        Alcotest.(check string) "check sequent list returned" (Yojson.Basic.to_string expected_proof) response_as_string in
    List.iter run_test test_samples

let call_api_test_png () =
    let body_as_string = "{\"notations\":[],\"proof\":{\"s\":{\"cons\": [{\"t\":\"litt\",\"v\":\"a\"},{\"t\":\"dual\",\"v\":{\"t\":\"litt\",\"v\":\"a\"}}]},\"ar\":{\"rr\":{\"r\":\"axiom\"},\"p\":[]}}}" in
    let response_as_string = call_api_post "export_as_latex/png/false" body_as_string 200 in
    Alcotest.(check bool) "not empty response" true (response_as_string <> "")

let call_api_sequent_is_provable () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_sequent_is_provable" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_provability = test_sample |> member "expected_provability" |> to_bool in
        let response_as_string = call_api_post "is_sequent_provable" (Yojson.Basic.to_string request_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let provable = response_as_json |> member "is_provable" |> to_bool in
        Alcotest.(check bool) "check provability" provable expected_provability in
    List.iter run_test test_samples

let parse_auto_prove_and_verify () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "parse_auto_prove_and_verify" |> to_list in
    let run_test test_sample =
        let sequent_as_string = test_sample |> to_string in
        let parse_sequent_data = Yojson.Basic.from_string (call_api_get ("parse_sequent/" ^ (url_encode_string sequent_as_string))) in
        let is_valid = parse_sequent_data |> member "is_valid" |> to_bool in
        Alcotest.(check bool) (sequent_as_string ^ " is valid") true is_valid;
        let sequent_as_json = parse_sequent_data |> member "proof" |> member "sequent" in
        let request = `Assoc [ ("sequent", sequent_as_json); ("notations", `List [])] in
        let response_as_string = call_api_post "auto_prove_sequent" (Yojson.Basic.to_string request) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" success true;
        let proof_as_json = response_as_json |> member "proof" in
        let request_as_json = `Assoc [("notations", `List []); ("proof", proof_as_json)] in
        let _ = call_api_post "export_as_coq" (Yojson.Basic.to_string request_as_json) 200 in
        () in
    List.iter run_test test_samples

let auto_prove_non_provable () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "auto_prove_non_provable" |> to_list in
    let run_test test_sample =
        let response_as_string = call_api_post "auto_prove_sequent" (Yojson.Basic.to_string test_sample) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" success false;
        let is_provable = response_as_json |> member "is_provable" |> to_bool in
        Alcotest.(check bool) "is_provable" is_provable false in
    List.iter run_test test_samples

let auto_prove_and_check_simplified_proof () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "auto_prove_and_check_simplified_proof" |> to_list in
    let run_test test_sample =
        let sequent_as_json = test_sample |> member "sequent" in
        let expected_proof = test_sample |> member "proof" in
        let request = `Assoc [ ("sequent", sequent_as_json); ("notations", `List [])] in
        let response_as_string = call_api_post "auto_prove_sequent" (Yojson.Basic.to_string request) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        let proof = response_as_json |> member "proof" in
        Alcotest.(check bool) "success" success true;
        Alcotest.(check string) "check proof" (Yojson.Basic.to_string expected_proof) (Yojson.Basic.to_string proof) in
    List.iter run_test test_samples

let auto_prove_with_notations () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "auto_prove_with_notations" |> to_list in
    let run_test test_sample =
        let response_as_string = call_api_post "auto_prove_sequent" (Yojson.Basic.to_string test_sample) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let success = response_as_json |> member "success" |> to_bool in
        Alcotest.(check bool) "success" success true in
    List.iter run_test test_samples

let test_compress_and_uncompress () =
    let check_json_file json_file =
        let big_proof_as_json = Yojson.Basic.from_file json_file in
        let proof_as_latex = call_api_post "export_as_latex/tex/true" (Yojson.Basic.to_string big_proof_as_json) 200 in
        let response_as_string = call_api_post "compress_proof" (Yojson.Basic.to_string big_proof_as_json) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let compressed_proof = response_as_json |> member "compressedProof" |> to_string in
        Alcotest.(check bool) "less than 2000 characters long" true (String.length compressed_proof < 2000);
        let uncompressed_proof_as_string = call_api_post "uncompress_proof" response_as_string 200 in
        let uncompressed_proof = Yojson.Basic.from_string uncompressed_proof_as_string in
        Alcotest.(check bool) "has proof" true ((uncompressed_proof |> member "proof") <> `Null);
        let uncompressed_proof_as_latex = call_api_post "export_as_latex/tex/true" uncompressed_proof_as_string 200 in
        Alcotest.(check string) "check proof as latex" proof_as_latex uncompressed_proof_as_latex in
    check_json_file "test/proof_test_data/lcm23.json";
    check_json_file "test/proof_test_data/axiom_with_notations.json";
    check_json_file "test/proof_test_data/litt_with_quote.json"

let call_api_apply_transformation () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "apply_transformation" |> to_list in
    let run_test test_sample =
        let compressed_proof = call_api_post "compress_proof" (Yojson.Basic.to_string test_sample) 200 in
        let uncompressed_proof = call_api_post "uncompress_proof" compressed_proof 200 |> Yojson.Basic.from_string |> member "proof" in
        let notations = test_sample |> member "notations" in
        let response_as_string = call_api_post "apply_transformation" (Yojson.Basic.to_string test_sample) 200 in
        let response_as_json = Yojson.Basic.from_string response_as_string in
        let proof = response_as_json |> member "proof" in
        let request_as_json = `Assoc [("proof", proof); ("notations", notations)] in
        (* Check proof validity *)
        let _ = call_api_post "export_as_latex/tex/true" (Yojson.Basic.to_string request_as_json) 200 in
        let proof_first_sequent = uncompressed_proof |> member "sequent" |> Yojson.Basic.to_string in
        let got_proof_first_sequent = proof |> member "sequent" |> Yojson.Basic.to_string in
        Alcotest.(check string) "check proof first sequent" proof_first_sequent got_proof_first_sequent in
    List.iter run_test test_samples

let call_api_apply_transformation_and_check_result () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "apply_transformation_and_check_result" |> to_list in
    let run_test test_sample =
        let request = test_sample |> member "request" in
        let response_as_string = call_api_post "apply_transformation" (Yojson.Basic.to_string request) 200 in
        let expected_result = test_sample |> member "expected_result" |> Yojson.Basic.to_string in
        Alcotest.(check string) "check proof match" expected_result response_as_string in
    List.iter run_test test_samples

let test_parse_sequent = [
    "Test full response", `Quick, call_api_parse_sequent_full_response;
    "Test sequent", `Quick, call_api_parse_sequent;
    "Test syntax exeption", `Quick, call_api_parse_sequent_syntax_exception;
]

let test_apply_rule = [
    "Test sequent", `Quick, call_api_apply_rule;
    "Test technical exception", `Quick, call_api_apply_rule_technical_exception;
    "Test logic exception", `Quick, call_api_apply_rule_logic_exception;
]

let test_export_as_latex = [
    "Test png", `Quick, call_api_test_png;
]

let test_sequent_is_provable = [
    "Test sequent is provable", `Quick, call_api_sequent_is_provable;
]

let test_auto_reverse_sequent = [
    "Test auto reverse full response", `Quick, call_api_auto_reverse_full_response;
    "Test auto reverse", `Quick, call_api_auto_reverse;
]

let test_auto_prove_sequent = [
    "Test parse, auto-prove and verify", `Quick, parse_auto_prove_and_verify;
    "Test parse, auto-prove on non provable sequent", `Quick, auto_prove_non_provable;
    "Test auto-prove and check simplified proof", `Quick, auto_prove_and_check_simplified_proof;
    "Test auto-prove with notations", `Quick, auto_prove_with_notations;
]

let test_compress_uncompress = [
    "Test compress and uncompress", `Quick, test_compress_and_uncompress;
]

let test_apply_transformation = [
    "Test apply transformation", `Quick, call_api_apply_transformation;
    "Test apply transformation and check result", `Quick, call_api_apply_transformation_and_check_result;
]

(* Run it *)
let () =
    Alcotest.run "API on localhost:8080" [
        "test_parse_sequent", test_parse_sequent;
        "test_apply_rule", test_apply_rule;
        "test_export_as_latex", test_export_as_latex;
        "test_sequent_is_provable", test_sequent_is_provable;
        "test_auto_reverse_sequent", test_auto_reverse_sequent;
        "test_auto_prove_sequent", test_auto_prove_sequent;
        "test_compress_uncompress", test_compress_uncompress;
        "test_apply_transformation", test_apply_transformation;
    ]
