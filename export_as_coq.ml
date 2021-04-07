open Proof

exception Cannot_export_proof_as_coq_exception of string;;

let proof_variables conclusion =
    let unique_variable_names = Sequent.get_unique_variable_names conclusion in
    match unique_variable_names with
    | [] -> ""
    | _ -> Printf.sprintf "Variable %s : formula.\n\n" (String.concat " " unique_variable_names);;

let proof_to_coq proof =
    let conclusion = get_conclusion proof in
    let start_file_line = "Require Import macroll.\n\nSection TheProof.\n\n" in
    let variable_line = proof_variables conclusion in
    let goal_line = Printf.sprintf "Goal %s.\n" (Sequent.sequent_to_coq conclusion) in
    let start_proof_line = "Proof.\n" in
    let proof_lines = Proof.to_coq proof in
    let end_proof_line = "Qed.\n\nEnd TheProof.\n" in
    Printf.sprintf "%s%s%s%s%s%s" start_file_line variable_line goal_line start_proof_line proof_lines end_proof_line;;

let export_as_coq_with_exceptions request_as_json =
    let proof = Proof.from_json request_as_json in
    if not (Proof.is_complete proof) then raise (Cannot_export_proof_as_coq_exception "proof is not complete")
    else proof_to_coq proof;;

let export_as_coq request_as_json =
    try let proof_as_coq = export_as_coq_with_exceptions request_as_json in
        true, proof_as_coq
    with Proof.Json_exception m -> false, "Bad proof json: " ^ m
        | Sequent.Json_exception m -> false, "Bad sequent json: " ^ m
        | Rule_request.Json_exception m -> false, "Bad rule_request json: " ^ m
        | Proof.Technical_exception m -> false, "Invalid proof: " ^ m
        | Cannot_export_proof_as_coq_exception m -> false, "Cannot export proof as coq: " ^ m;;
