let get_transformation_options_json proof =
    Proof.to_json ~transform_options:true proof;;


(* HANDLERS *)

let get_proof_transformation_options request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let proof_with_transformation_options = get_transformation_options_json proof_with_notations.proof in
        true, `Assoc ["proofWithTransformationOptions", proof_with_transformation_options]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad request: " ^ m);;