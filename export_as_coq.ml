open Proof
open Proof_with_notations

let variables_cyclic_acyclic_notations proof_with_notations =
    let proof_variables = Proof.get_unique_variable_names proof_with_notations.proof in
    let cyclic_notations, sorted_acyclic_notations = Notations.split_cyclic_acyclic proof_with_notations.notations (Some proof_variables) in
    let all_variables = List.sort_uniq String.compare (
        proof_variables
        @ Notations.get_variable_names cyclic_notations
        @ Notations.get_variable_names sorted_acyclic_notations) in
    let variables = List.filter (fun s -> not (List.mem_assoc s sorted_acyclic_notations)) all_variables in
    variables, cyclic_notations, sorted_acyclic_notations

let proof_variables = function
    | [] -> ""
    | variables -> Printf.sprintf "Variable %s : formula.\n\n" (String.concat " " variables)

let rec proof_definitions = function
    | [] -> ""
    | (s, f) :: tail -> proof_definitions tail
        ^ Printf.sprintf "Definition %s := %s.\n" s (Sequent.formula_to_coq (Raw_sequent.to_formula f))

let rec proof_hypotheses = function
    | [] -> ""
    | (s, f) :: tail -> proof_hypotheses tail
        ^ Printf.sprintf "Hypothesis Hyp_%s : %s = %s.\n" s s (Sequent.formula_to_coq (Raw_sequent.to_formula f))

let list_of_intros k =
  String.concat " " (List.init k (fun n -> "Hyp" ^ (string_of_int n)))

let proof_to_coq proof_with_notations =
    let variables, cyclic_notations, sorted_acyclic_notations = variables_cyclic_acyclic_notations proof_with_notations in

    let header = "(* This Coq file has been generated using the C1ick ⅋ c⊗LLec⊥ tool. *)\n"
        ^ "(* https://click-and-collect.linear-logic.org/ *)\n"
        ^ "(* First download and install NanoYalla version 1.1.3, see README.md: *)\n"
        ^ "(* https://click-and-collect.linear-logic.org/download/nanoyalla.zip *)\n\n" in
    let start_file_line = "From NanoYalla Require Import macrollcut.\n\nImport LLNotations.\n\nSection TheProof.\n\n" in

    let variable_line = proof_variables variables in
    let definitions_lines = proof_definitions sorted_acyclic_notations ^
        if List.length sorted_acyclic_notations > 0 then "\n" else "" in
    let hypotheses_lines = proof_hypotheses cyclic_notations ^
        if List.length cyclic_notations > 0 then "\n" else "" in

    let proof_lines, number_of_hypotheses, hypotheses = to_coq_with_hyps cyclic_notations proof_with_notations.proof in
    let conclusion_as_coq = Sequent.sequent_to_coq (get_conclusion proof_with_notations.proof) in
    let goal_line = Printf.sprintf "Goal %s.\n" (String.concat " -> " (hypotheses @ [conclusion_as_coq])) in
    let intros_list =
        if number_of_hypotheses > 0 then
          Printf.sprintf "intros %s.\n" (list_of_intros number_of_hypotheses)
        else "" in
    let start_proof_line = "Proof.\n" ^ intros_list in
    let end_proof_line = "Qed.\n\nEnd TheProof.\n" in
    Printf.sprintf "%s%s%s%s%s%s%s%s%s" header start_file_line variable_line definitions_lines hypotheses_lines goal_line start_proof_line proof_lines end_proof_line;;

let export_as_coq_with_exceptions request_as_json =
    let proof_with_notations = Proof_with_notations.from_json request_as_json in
    proof_to_coq proof_with_notations;;

let export_as_coq request_as_json =
    try let proof_as_coq = export_as_coq_with_exceptions request_as_json in
        true, proof_as_coq
    with Proof_with_notations.Json_exception m -> false, "Bad request: " ^ m;
