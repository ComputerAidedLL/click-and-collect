let auto_prove_sequent_with_exceptions request_as_json =
    let sequent = Raw_sequent.sequent_from_json request_as_json in
    let proof = Focused_proof.prove_sequent sequent in
    Proof.to_json proof;;

let auto_prove_sequent request_as_json =
    try let proof_as_json = auto_prove_sequent_with_exceptions request_as_json in
        true, `Assoc [("success", `Bool true); ("proof", proof_as_json)]
    with Focused_proof.NonProvableSequent -> true, `Assoc [("success", `Bool false); ("is_provable", `Bool false)]
        | Focused_proof.NonAutoProvableSequent -> true, `Assoc [("success", `Bool false); ("is_provable", `Bool true)]
        | Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m);;
