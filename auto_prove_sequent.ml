let try_auto_prover_on_sequent sequent =
    try Some (Focused_proof.prove_sequent sequent), true
    with Focused_proof.NonProvableSequent -> None, false
         | Focused_proof.NonAutoProvableSequent -> None, true

let auto_prove_sequent_with_exceptions request_as_json =
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    try Some (Proof.auto_weak sequent_with_notations.sequent), true
    with Proof.AutoWeakNotApplicable ->
        let result = try_auto_prover_on_sequent sequent_with_notations.sequent in
        match result with
        | Some proof, _ -> result
        | None, _ ->
            let cyclic_notations, acyclic_notations = Sequent_with_notations.split_cyclic_acyclic sequent_with_notations in
            if List.length cyclic_notations + List.length acyclic_notations > 0
            then None, true
            else result


let auto_prove_sequent request_as_json =
    try match auto_prove_sequent_with_exceptions request_as_json with
        | Some proof, _ -> true, `Assoc [("success", `Bool true); ("proof", Proof.to_json proof)]
        | None, is_provable -> true, `Assoc [("success", `Bool false); ("is_provable", `Bool is_provable)]
    with Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m);;
