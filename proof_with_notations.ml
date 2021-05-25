open Proof
open Notations

type proof_with_notations = {proof: proof; notations: notations}

exception Json_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let from_json proof_with_notations_as_json =
    try
        let notations_as_json = get_key proof_with_notations_as_json "notations" in
        let notations = Notations.from_json notations_as_json in
        let proof_as_json = get_key proof_with_notations_as_json "proof" in
        let proof = Proof.from_json notations proof_as_json in
        {proof=proof; notations=notations}
    with Proof.Json_exception m -> raise (Json_exception ("bad proof json: " ^ m))
         | Raw_sequent.Json_exception m -> raise (Json_exception ("bad sequent json: " ^ m))
         | Rule_request.Json_exception m -> raise (Json_exception ("bad rule_request json: " ^ m))
         | Proof.Rule_exception (_, m) -> raise (Json_exception ("invalid proof: " ^ m))
         | Notations.Json_exception m -> raise (Json_exception ("bad notations: " ^ m))

let to_json proof_with_notations =
    `Assoc [
        ("proof", Proof.to_json proof_with_notations.proof);
        ("notations", Notations.to_json proof_with_notations.notations)
    ]