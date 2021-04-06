open Proof

exception Cannot_export_proof_as_coq_exception of string;;

let proof_variables proof =
    let unique_variable_names = Linear_logic.get_unique_variable_names proof.sequent in
    Printf.sprintf "Variable %s : formula.\n" (String.concat " " unique_variable_names);;

let proof_to_coq proof =
    let import_line = "Require Import macroll.\n" in
    let section_the_proof_line = "Section TheProof.\n" in
    let variable_line = proof_variables proof in
    let goal_line = Printf.sprintf "Goal %s.\n" (Linear_logic.sequent_to_coq proof.sequent) in
    let proof_line = "Proof.\n" in
    let qed_line = "Qed.\n" in
    let end_the_proof_line = "End TheProof.\n" in
    Printf.sprintf "%s\n%s\n%s\n%s%s%s\n%s\n" import_line section_the_proof_line variable_line goal_line proof_line qed_line end_the_proof_line;;

let export_as_coq_with_exceptions request_as_json =
    let proof = Proof.json_to_proof request_as_json in
    if not (Proof.is_valid proof) then raise (Cannot_export_proof_as_coq_exception "proof is not valid")
    else if not (Proof.is_complete proof) then raise (Cannot_export_proof_as_coq_exception "proof is not complete")
    else proof_to_coq proof;;

let export_as_coq request_as_json =
    try let proof_as_coq = export_as_coq_with_exceptions request_as_json in
        true, proof_as_coq
    with Proof.Bad_proof_json_exception m -> false, "Bad proof json: " ^ m
        | Proof.Invalid_proof_exception m -> false, "Invalid proof: " ^ m
        | Rule.Bad_rule_string_exception m -> false, "Bad rule json: " ^ m
        | Cannot_export_proof_as_coq_exception m -> false, "Cannot export proof as coq: " ^ m;;
