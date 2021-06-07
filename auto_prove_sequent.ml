let auto_prove_sequent_with_exceptions request_as_json =
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    try Some (Proof.auto_weak sequent_with_notations.sequent), true
    with Proof.AutoWeakNotApplicable ->
        Focused_proof.prove_sequent sequent_with_notations

let auto_prove_sequent request_as_json =
    try match auto_prove_sequent_with_exceptions request_as_json with
        | Some proof, _ -> true, `Assoc [("success", `Bool true); ("proof", Proof.to_json proof)]
        | None, is_provable -> true, `Assoc [("success", `Bool false); ("is_provable", `Bool is_provable)]
    with Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m);;
