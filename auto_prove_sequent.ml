exception NotImplemented of string

let auto_prove_sequent_with_exceptions request_as_json =
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    let cyclic_notations, acyclic_notations = Sequent_with_notations.split_cyclic_acyclic sequent_with_notations in
    if List.length cyclic_notations + List.length acyclic_notations > 0
    then raise (NotImplemented "Auto-prover has not been implemented yet on sequent with notations.")
    else try Proof.auto_weak sequent_with_notations.sequent
        with Proof.AutoWeakNotApplicable ->
            Focused_proof.prove_sequent sequent_with_notations.sequent;;

let auto_prove_sequent request_as_json =
    try let proof = auto_prove_sequent_with_exceptions request_as_json in
        let proof_as_json = Proof.to_json proof in
        200, `Assoc [("success", `Bool true); ("proof", proof_as_json)]
    with Focused_proof.NonProvableSequent -> 200, `Assoc [("success", `Bool false); ("is_provable", `Bool false)]
        | Focused_proof.NonAutoProvableSequent -> 200, `Assoc [("success", `Bool false); ("is_provable", `Bool true)]
        | Sequent_with_notations.Json_exception m -> 400, `String ("Bad sequent with notations: " ^ m)
        | NotImplemented m -> 501, `String m;;
