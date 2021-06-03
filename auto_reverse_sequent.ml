let auto_reverse_sequent_with_exceptions request_as_json =
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    let proof = Proof.rec_apply_reversible_rule sequent_with_notations.notations (Hypothesis_proof sequent_with_notations.sequent) in
    Proof.to_json proof;;

let auto_reverse_sequent request_as_json =
    try let proof_as_json = auto_reverse_sequent_with_exceptions request_as_json in
        true, proof_as_json
    with Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m);;
