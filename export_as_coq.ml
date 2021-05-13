open Proof

let proof_variables conclusion =
    let unique_variable_names = Sequent.get_unique_variable_names conclusion in
    match unique_variable_names with
    | [] -> ""
    | _ -> Printf.sprintf "Variable %s : formula.\n\n" (String.concat " " unique_variable_names);;

let list_of_intros k =
  String.concat " " (List.init k (fun n -> "Hyp" ^ (string_of_int n)))

let proof_to_coq extraction proof =
    let conclusion = get_conclusion proof in
    let header = "(* This Coq file has been generated using the C1ick ⅋ c⊗LLec⊥ tool. *)\n"
        ^ "(* https://click-and-collect.linear-logic.org/ *)\n"
        ^ "(* First download and install NanoYalla version 1.0.3, see README.md: *)\n"
        ^ "(* https://click-and-collect.linear-logic.org/download/nanoyalla.zip *)\n\n" in
    let start_file_line = "From NanoYalla Require Import " ^
                          (if extraction then "extractionll" else "macroll") ^
                          ".\n\nImport LLNotations.\n\nSection TheProof.\n\n" in
    let variable_line = proof_variables conclusion in
    let proof_lines, number_of_hypotheses, hypotheses = to_coq_with_hyps proof in
    let goal_line = 
        (if extraction then "Lemma pi :" else "Goal") ^
        (Printf.sprintf " %s.\n" (String.concat " -> " (hypotheses @ [Sequent.sequent_to_coq conclusion]))) in
    let intros_list =
        if number_of_hypotheses > 0 then
          Printf.sprintf "intros %s.\n" (list_of_intros number_of_hypotheses)
        else "" in
    let start_proof_line = "Proof.\n" ^ intros_list in
    let end_proof_line =
        (if extraction then "Defined" else "Qed") ^
        ".\n\nEnd TheProof.\n" ^
        (if extraction then "\nExtraction \"extracted.ml\" pi.\n" else "") in
    Printf.sprintf "%s%s%s%s%s%s%s" header start_file_line variable_line goal_line start_proof_line proof_lines end_proof_line;;

let export_as_coq_with_exceptions request_as_json =
    let proof = Proof.from_json request_as_json in
    proof_to_coq false proof;;

let export_as_coq request_as_json =
    try let proof_as_coq = export_as_coq_with_exceptions request_as_json in
        true, proof_as_coq
    with Proof.Json_exception m -> false, "Bad proof json: " ^ m
        | Raw_sequent.Json_exception m -> false, "Bad sequent json: " ^ m
        | Rule_request.Json_exception m -> false, "Bad rule_request json: " ^ m
        | Proof.Rule_exception (_, m) -> false, "Invalid proof: " ^ m;;
