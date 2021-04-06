open Proof

exception Cannot_export_proof_as_coq_exception of string;;

let proof_variables proof =
    let unique_variable_names = Linear_logic.get_unique_variable_names proof.sequent in
    match unique_variable_names with
    | [] -> ""
    | _ -> Printf.sprintf "Variable %s : formula.\n\n" (String.concat " " unique_variable_names);;

let build_proof_body proof =
    match proof.applied_rule with
    | None -> raise (Cannot_export_proof_as_coq_exception "proof is not complete")
    | Some applied_rule -> Rule.rule_to_coq applied_rule.rule proof.sequent applied_rule.formula_positions

let proof_to_coq proof =
    let start_file_line = "Require Import macroll.\n\nSection TheProof.\n\n" in
    let variable_line = proof_variables proof in
    let goal_line = Printf.sprintf "Goal %s.\n" (Linear_logic.sequent_to_coq proof.sequent) in
    let start_proof_line = "Proof.\n" in
    let proof_lines = build_proof_body proof in
    let end_proof_line = "Qed.\n\nEnd TheProof.\n" in
    Printf.sprintf "%s%s%s%s%s%s" start_file_line variable_line goal_line start_proof_line proof_lines end_proof_line;;

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
