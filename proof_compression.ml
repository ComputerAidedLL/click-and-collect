let compress_proof_with_exceptions request_as_json =
    let proof = Proof.from_json request_as_json in
    Proof_marshal.compress_json_lzma proof;;

let compress_proof request_as_json =
    try let compressed_proof = compress_proof_with_exceptions request_as_json in
        true, compressed_proof
    with Proof.Json_exception m -> false, "Bad proof json: " ^ m
        | Raw_sequent.Json_exception m -> false, "Bad sequent json: " ^ m
        | Rule_request.Json_exception m -> false, "Bad rule_request json: " ^ m
        | Proof.Rule_exception (_, m) -> false, "Invalid proof: " ^ m
        | Proof_marshal.Encode_exception m -> false, "Could not compress proof: " ^ m;;

let uncompress_proof_with_exceptions request_as_string =
    Proof_marshal.uncompress_json_lzma request_as_string;;

let uncompress_proof request_as_string =
    (* The replacements of = by ~ is only done by tests, because of buggy ocaml url-encoding *)
    let request_with_equals = Str.global_replace (Str.regexp "~") "=" request_as_string in
    try let uncompressed_proof = uncompress_proof_with_exceptions request_with_equals in
        let uncompressed_proof_as_json = Proof.to_json uncompressed_proof in
        true, uncompressed_proof_as_json
    with Proof_marshal.Decode_exception -> false, `String ("Could not uncompress proof.")