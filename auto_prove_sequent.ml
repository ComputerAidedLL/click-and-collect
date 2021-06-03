open Sequent_with_notations

let try_auto_prover_on_sequent sequent =
    try Some (Focused_proof.prove_sequent sequent), true
    with Focused_proof.NonProvableSequent -> None, false
         | Focused_proof.NonAutoProvableSequent -> None, true

let auto_prove_sequent_with_exceptions request_as_json =
    let swn = Sequent_with_notations.from_json request_as_json in
    try Some (Proof.auto_weak swn.sequent), true
    with Proof.AutoWeakNotApplicable ->
        let result = try_auto_prover_on_sequent swn.sequent in
        match result with
        | Some proof, _ -> result
        | None, _ ->
            let cyclic_notations, acyclic_notations = split_cyclic_acyclic swn in
            if List.length cyclic_notations > 0
            then None, true
            else let fully_replaced_sequent = replace_all_notations_in_sequent swn.sequent acyclic_notations in
                match try_auto_prover_on_sequent fully_replaced_sequent with
                | Some fully_replaced_proof, _ ->
                    let proof = Proof.from_fully_replaced_proof acyclic_notations swn.sequent fully_replaced_proof in
                    Some proof, true
                | result -> result


let auto_prove_sequent request_as_json =
    try match auto_prove_sequent_with_exceptions request_as_json with
        | Some proof, _ -> true, `Assoc [("success", `Bool true); ("proof", Proof.to_json proof)]
        | None, is_provable -> true, `Assoc [("success", `Bool false); ("is_provable", `Bool is_provable)]
    with Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m);;
