open Proof

exception Encode_exception of string
exception Decode_exception

(* PROOF <-> COMPRESSED JSON STRING *)

let to_base64 s =
    Base64.encode_exn s

let from_base64 s =
    Base64.decode_exn s

let compress_json_lzma proof_with_notations =
    let json_as_string = Yojson.Basic.to_string (Proof_with_notations.to_json proof_with_notations) in
    let input = Bytes.of_string json_as_string in
    let output = Bytes.of_string (String.make 2000 ' ') in
    try let _ = Lzma.lzma_easy_buffer_encode 0 [Lzma.LZMA_PRESET_EXTREME] Lzma.LZMA_CHECK_NONE input output 0 in
    let encoded_json = String.trim (Bytes.to_string output) in
    to_base64 encoded_json
    with Failure m when m = "lzma_easy_buffer_encode: not enough output buffer space" -> raise (Encode_exception "proof is too big")

let uncompress_json_lzma s =
    let input = Bytes.of_string (from_base64 s) in
    let output = Bytes.of_string (String.make 1000000 ' ') in
    try let _ = Lzma.lzma_stream_buffer_decode Int64.max_int [] input 0 output 0 in
        let decoded_json = String.trim (Bytes.to_string output) in
        Proof_with_notations.from_json (Yojson.Basic.from_string decoded_json)
    with Failure m when m = "lzma_stream_buffer_decode: format error" -> raise Decode_exception
