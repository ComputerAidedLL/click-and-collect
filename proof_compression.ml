let compress_proof_with_exceptions request_as_json =
    let proof_with_notations = Proof_with_notations.from_json request_as_json in
    Proof_marshal.compress_json_lzma proof_with_notations;;

let compress_proof request_as_json =
    try let compressed_proof = compress_proof_with_exceptions request_as_json in
        true, compressed_proof
    with Proof_with_notations.Json_exception m -> false, "Bad request: " ^ m;;

let uncompress_proof_with_exceptions request_as_string =
    Proof_marshal.uncompress_json_lzma request_as_string;;

let uncompress_proof request_as_string =
    (* The replacements of = by ~ is only done by tests, because of buggy ocaml url-encoding *)
    let request_with_equals = Str.global_replace (Str.regexp "~") "=" request_as_string in
    try let uncompressed_proof_and_notations = uncompress_proof_with_exceptions request_with_equals in
        let uncompressed_proof_as_json = Proof_with_notations.to_json uncompressed_proof_and_notations in
        true, uncompressed_proof_as_json
    with Proof_marshal.Decode_exception -> false, `String ("Could not uncompress proof.")