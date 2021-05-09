open Proof

exception Encode_exception of string
exception Decode_exception

(* PROOF <-> COMPRESSED JSON STRING *)

let to_base64 s =
    Base64.encode_exn s

let from_base64 s =
    Base64.decode_exn s

let compress_json_lzma proof =
    let json_as_string = Yojson.Basic.to_string (Proof.to_json proof) in
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
        Proof.from_json (Yojson.Basic.from_string decoded_json)
    with Failure m when m = "lzma_stream_buffer_decode: format error" -> raise Decode_exception


(* PROOF <-> MARSHALLED PROOF STRING *)
(* Not used for the moment, marshalling is more efficient than compressing json, but less backward compatible *)

(* copied from Ocaml 4.11 *)
let fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l in
  aux accu [] l

let rec to_request_option_list proof =
    match proof with
    | Hypothesis_proof _ -> [None]
    | _ -> let rule_request = get_rule_request proof in
           let premises = get_premises proof in
           Some rule_request :: List.concat (List.map to_request_option_list premises)

exception Invalid_request_list of string

let from_request_option_list rule_requests sequent =
  let rec from_request_option_stream stream sequent =
    match stream with
    | [] -> raise (Invalid_request_list "Too short")
    | None :: tail -> (tail, Hypothesis_proof sequent)
    | Some rule_request :: tail ->
       let proof = from_sequent_and_rule_request sequent rule_request in
       let premises_conclusions = List.map get_conclusion (get_premises proof) in
       let tail_tail, premises = fold_left_map from_request_option_stream tail premises_conclusions in
       (tail_tail, set_premises proof premises) in
  match from_request_option_stream rule_requests sequent with
  | [], proof -> proof
  | _ -> raise (Invalid_request_list "Too long")

let direct_marshal proof =
    to_base64 (Marshal.to_string proof [])

let direct_unmarshal str =
    Marshal.from_string (from_base64 str) 0

let compact_marshal proof =
    to_base64 (Marshal.to_string (get_conclusion proof, to_request_option_list proof) [])

let compact_unmarshal str =
    try
        match (Marshal.from_string (from_base64 str) 0 : Sequent.sequent * (Rule_request.rule_request option) list) with
        | sequent, rule_requests -> from_request_option_list rule_requests sequent
    with Invalid_argument _ -> raise Decode_exception
