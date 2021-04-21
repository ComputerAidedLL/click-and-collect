let auto_reverse_sequent_with_exceptions request_as_json =
    let sequent = Raw_sequent.sequent_from_json request_as_json in
    let proof = Proof.apply_reversible_rule (Hypothesis_proof sequent) in
    Proof.to_json proof;;

let auto_reverse_sequent request_as_json =
    try let proof_as_json = auto_reverse_sequent_with_exceptions request_as_json in
        true, proof_as_json
    with Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m);;
