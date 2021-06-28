(* ABBREVIATIONS JSON COMPRESSION *)
(* Json compression with human readibility *)

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


(* LZMA JSON COMPRESSION *)
(* Relies on liblzma-dev linux package *)

exception Uncompress_exception of string

let get_command_output cmd =
    let in_channel = Unix.open_process_in cmd in
    let lines = ref [] in
    begin try
        while true do
            lines := input_line in_channel :: !lines
        done;
    with End_of_file ->
        let status = Unix.close_process_in in_channel in
        if status <> (Unix.WEXITED 0)
            then raise (Failure ("Error while executing command: " ^ cmd))
    end;
    String.concat "" (List.rev !lines)

let compress_json_lzma proof_with_notations =
    let json_as_string = Yojson.Basic.to_string (Proof_with_notations.to_json proof_with_notations) in
    let json_with_safe_quote = Str.global_replace (Str.regexp "\"") "\\\"" json_as_string in
    let cmd = Printf.sprintf "echo \"%s\" | lzma | base64 --wrap=0" json_with_safe_quote in
    get_command_output cmd

let uncompress_json_lzma encoded_string =
    let cmd = Printf.sprintf "echo '%s' | base64 -d | unlzma" encoded_string in
    let decoded_json_as_string = get_command_output cmd in
    try Yojson.Basic.from_string decoded_json_as_string
    with Yojson.Json_error _ -> raise (Uncompress_exception ("Encoded string is not a valid json: " ^ decoded_json_as_string))

(* HANDLERS *)

let compress_proof request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let compressed_proof = compress_json_lzma proof_with_notations in
        true, `Assoc ["compressedProof", `String compressed_proof]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad request: " ^ m);;

let uncompress_proof request_as_json =
    try let compressed_proof = Request_utils.get_string request_as_json "compressedProof" in
        let uncompressed_proof_and_notations_as_string = uncompress_json_lzma compressed_proof in
        let uncompressed_proof_and_notations = Proof_with_notations.from_json uncompressed_proof_and_notations_as_string in
        let uncompressed_proof_and_notations_as_json = Proof_with_notations.to_json uncompressed_proof_and_notations in
        true, uncompressed_proof_and_notations_as_json
    with Uncompress_exception m -> false, `Assoc [("error_message", `String m)]
        | Proof_with_notations.Json_exception m -> true, `Assoc [("error_message", `String ("Bad proof with notations: " ^ m))]
        | Request_utils.Bad_request_exception m -> false, `Assoc [("error_message", `String ("Bad request: " ^ m))]