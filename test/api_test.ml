open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util

(* The utils function *)
let call_api path =
    let body =
      let complete_uri = "http://localhost:8080/" ^ path in
      Client.get (Uri.of_string complete_uri) >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Alcotest.(check int) "returns 200" 200 code;
        Cohttp_lwt.Body.to_string body in
    Lwt_main.run body

let assert_sequent_equal sequent_as_string expected_sequent_as_json =
    let body = call_api ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
    let data = Yojson.Basic.from_string body in
    let is_valid = data |> member "is_valid" |> to_bool in
    Alcotest.(check bool) (sequent_as_string ^ " is valid") true is_valid;
    let sequent_as_json = data |> member "sequent_as_json" in
    Alcotest.(check string) ("check sequent returned for " ^ sequent_as_string) expected_sequent_as_json (Yojson.Basic.to_string sequent_as_json)

let assert_syntax_exception sequent_as_string =
    let body = call_api ("parse_sequent?sequentAsString=" ^ sequent_as_string) in
    let data = Yojson.Basic.from_string body in
    let is_valid = data |> member "is_valid" |> to_bool in
    Alcotest.(check bool) (sequent_as_string ^ " is invalid") false is_valid

(* The tests *)
let call_api_parse_sequent_full_response () =
    Alcotest.(check string) "valid" "{\"is_valid\":true,\"sequent_as_json\":{\"hyp\":[],\"cons\":[{\"type\":\"litteral\",\"value\":\"a\"}]}}" (call_api "parse_sequent?sequentAsString=a");
    Alcotest.(check string) "invalid" "{\"is_valid\":false,\"error_message\":\"Syntax error: please read the syntax rules\"}" (call_api "parse_sequent?sequentAsString=aa")

let call_api_parse_sequent () =
    let json_file = Yojson.Basic.from_file "test/api_test_data.json" in
    let test_samples = json_file |> member "call_api_parse_sequent" |> to_list in
    let run_test test_sample =
        let sequent_as_string = test_sample |> member "sequent_as_string" |> to_string in
        let expected_sequent_as_json = test_sample |> member "expected_sequent_as_json" in
        assert_sequent_equal sequent_as_string (Yojson.Basic.to_string expected_sequent_as_json) in
    List.iter run_test test_samples

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

let test_parse_sequent = [
    "Test full response", `Quick, call_api_parse_sequent_full_response;
    "Test sequent", `Quick, call_api_parse_sequent;
    "Test syntax exeption", `Quick, call_api_parse_sequent_syntax_exception;
]

(* Run it *)
let () =
    Alcotest.run "API on localhost:8080" [
        "test_parse_sequent", test_parse_sequent;
    ]
