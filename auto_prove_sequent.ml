exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

exception NotImplemented of string

let auto_prove_sequent_with_exceptions request_as_json =
    let sequent_as_json = get_key request_as_json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    let notations_as_json = get_key request_as_json "notations" in
    let notations = Notations.from_json notations_as_json in
    let cyclic_notations, acyclic_notations = Notations.split_cyclic_acyclic notations sequent in
    if List.length cyclic_notations + List.length acyclic_notations > 0
    then raise (NotImplemented "Auto-prover has not been implemented yet on sequent with notations.")
    else try Proof.auto_weak sequent
        with Proof.AutoWeakNotApplicable ->
            Focused_proof.prove_sequent sequent;;

let auto_prove_sequent request_as_json =
    try let proof = auto_prove_sequent_with_exceptions request_as_json in
        let proof_as_json = Proof.to_json proof in
        200, `Assoc [("success", `Bool true); ("proof", proof_as_json)]
    with Focused_proof.NonProvableSequent -> 200, `Assoc [("success", `Bool false); ("is_provable", `Bool false)]
        | Focused_proof.NonAutoProvableSequent -> 200, `Assoc [("success", `Bool false); ("is_provable", `Bool true)]
        | Bad_request_exception m -> 400, `String ("Bad request: " ^ m)
        | Raw_sequent.Json_exception m -> 400, `String ("Bad sequent json: " ^ m)
        | Notations.Json_exception m -> 400, `String ("Bad notations json: " ^ m)
        | NotImplemented m -> 501, `String m;;
