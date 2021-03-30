let check_proof_complete_with_exceptions request_as_json =
    let proof = Proof.json_to_proof request_as_json in
    Proof.is_complete proof

let is_proof_complete request_as_json =
    try let is_complete = check_proof_complete_with_exceptions request_as_json in
        true, `Assoc [("is_complete", `Bool is_complete)]
    with
        | Proof.Bad_proof_json_exception m -> false, `String ("Bad proof json: " ^ m)
