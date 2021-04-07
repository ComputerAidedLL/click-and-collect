let check_proof_complete_with_exceptions request_as_json =
    let proof = Proof.from_json request_as_json in
    Proof.is_complete proof;;

let is_proof_complete request_as_json =
    try let is_complete = check_proof_complete_with_exceptions request_as_json in
        true, `Assoc [("is_complete", `Bool is_complete)]
    with Proof.Json_exception m -> false, `String ("Bad proof json: " ^ m)
        | Sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m)
        | Rule_request.Json_exception m -> false, `String ("Bad rule_request json: " ^ m)
        | Proof.Technical_exception m -> false, `String ("Invalid proof: " ^ m);;
