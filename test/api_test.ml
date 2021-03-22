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
        Alcotest.(check int) "status code" expected_code code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

(* The tests *)
let call_api_parse_sequent_full_response () =
    Alcotest.(check string) "valid" "{\"is_valid\":true,\"sequent_as_json\":{\"hyp\":[],\"cons\":[{\"type\":\"litteral\",\"value\":\"a\"}]}}" (call_api_get "parse_sequent?sequentAsString=a");
    Alcotest.(check string) "invalid" "{\"is_valid\":false,\"error_message\":\"Syntax error: please read the syntax rules\"}" (call_api_get "parse_sequent?sequentAsString=aa")

let assert_sequent_equal sequent_as_string expected_sequent_as_json =
    let body = call_api_get ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
    let data = Yojson.Basic.from_string body in
    let is_valid = data |> member "is_valid" |> to_bool in
    Alcotest.(check bool) (sequent_as_string ^ " is valid") true is_valid;
    let sequent_as_json = data |> member "sequent_as_json" in
    Alcotest.(check string) ("check sequent returned for " ^ sequent_as_string) expected_sequent_as_json (Yojson.Basic.to_string sequent_as_json)

let call_api_parse_sequent () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_parse_sequent" |> to_list in
    let run_test test_sample =
        let sequent_as_string = test_sample |> member "sequent_as_string" |> to_string in
        let expected_sequent_as_json = test_sample |> member "expected_sequent_as_json" in
        assert_sequent_equal sequent_as_string (Yojson.Basic.to_string expected_sequent_as_json) in
    List.iter run_test test_samples

let assert_syntax_exception sequent_as_string =
    let body = call_api_get ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
    let data = Yojson.Basic.from_string body in
    let is_valid = data |> member "is_valid" |> to_bool in
    Alcotest.(check bool) (sequent_as_string ^ " is invalid") false is_valid

let call_api_parse_sequent_syntax_exception () =
    assert_syntax_exception "aa";
    assert_syntax_exception "a|-a|-";
    assert_syntax_exception "|-a|-a";
    assert_syntax_exception "|-|-";
    assert_syntax_exception "|-,a";
    assert_syntax_exception "|-a,";
    assert_syntax_exception ",,";
    assert_syntax_exception "a^,~a";
    assert_syntax_exception "!a,b\\/c"

let call_api_apply_rule_full_response () =
    let body_as_string = "{\"rule\":\"par\", \"sequent\": {\"hyp\": [],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"litteral\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":0}" in
    let response_as_string = call_api_post "apply_rule" body_as_string 200 in
    Alcotest.(check string) "valid" "[{\"hyp\":[],\"cons\":[{\"type\":\"litteral\",\"value\":\"a\"},{\"type\":\"litteral\",\"value\":\"a\"}]}]" response_as_string

let assert_apply_rule_equal body_as_json expected_sequent_list_as_json =
    let response_as_string = call_api_post "apply_rule" (Yojson.Basic.to_string body_as_json) 200 in
    Alcotest.(check string) "check sequent list returned" (Yojson.Basic.to_string expected_sequent_list_as_json) response_as_string

let call_api_apply_rule () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_apply_rule" |> to_list in
    let run_test test_sample =
        let request_as_json = test_sample |> member "request" in
        let expected_sequent_list_as_json = test_sample |> member "expected_sequent_list_as_json" in
        assert_apply_rule_equal request_as_json expected_sequent_list_as_json in
    List.iter run_test test_samples

let assert_apply_rule_exception body_as_string =
    let body = call_api_post "apply_rule" body_as_string 400 in
    Alcotest.(check bool) "response not null" true (0 < String.length body)

let call_api_apply_rule_exception () =
    assert_apply_rule_exception "";
    assert_apply_rule_exception "hello";
    assert_apply_rule_exception "{}";
    (* unknown rule *)
    assert_apply_rule_exception "{\"rule\":\"hello\", \"sequent\": {\"hyp\": [],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"litteral\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":0}";
    (* invalid sequent *)
    assert_apply_rule_exception "{\"rule\":\"par\", \"sequent\": {\"hyp\": [],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"hello\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":0}";
    (* sequent with hypotheses *)
    assert_apply_rule_exception "{\"rule\":\"par\", \"sequent\": {\"hyp\": [{\"type\": \"litteral\", \"value\":\"a\"}],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"litteral\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":0}";
    (* formulaPosition out of range *)
    assert_apply_rule_exception "{\"rule\":\"par\", \"sequent\": {\"hyp\": [],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"litteral\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":1}";
    (* negative formulaPosition *)
    assert_apply_rule_exception "{\"rule\":\"par\", \"sequent\": {\"hyp\": [],\"cons\": [{\"type\": \"par\", \"value1\":{\"type\": \"litteral\", \"value\":\"a\"},\"value2\":{\"type\": \"litteral\", \"value\":\"a\"}}]}, \"formulaPosition\":-1}"

let test_parse_sequent = [
    "Test full response", `Quick, call_api_parse_sequent_full_response;
    "Test sequent", `Quick, call_api_parse_sequent;
    "Test syntax exeption", `Quick, call_api_parse_sequent_syntax_exception;
]

let test_apply_rule = [
    "Test full response", `Quick, call_api_apply_rule_full_response;
    "Test sequent", `Quick, call_api_apply_rule;
    "Test exception", `Quick, call_api_apply_rule_exception;
]

(* Run it *)
let () =
    Alcotest.run "API on localhost:8080" [
        "test_parse_sequent", test_parse_sequent;
        "test_apply_rule", test_apply_rule;
    ]
