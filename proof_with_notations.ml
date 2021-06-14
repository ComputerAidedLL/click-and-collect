open Proof
open Notations

type proof_with_notations = {proof: proof; notations: notations}

exception Json_exception of string

let from_json proof_with_notations_as_json =
    try
        let notations_as_json = Request_utils.get_key proof_with_notations_as_json "notations" in
        let notations = Notations.from_json notations_as_json in
        let proof_as_json = Request_utils.get_key proof_with_notations_as_json "proof" in
        let proof = Proof.from_json notations proof_as_json in
        {proof=proof; notations=notations}
    with Request_utils.Bad_request_exception m -> raise (Json_exception ("bad request: " ^ m))
         | Proof.Json_exception m -> raise (Json_exception ("bad proof json: " ^ m))
         | Raw_sequent.Json_exception m -> raise (Json_exception ("bad sequent json: " ^ m))
         | Rule_request.Json_exception m -> raise (Json_exception ("bad rule_request json: " ^ m))
         | Proof.Rule_exception (_, m) -> raise (Json_exception ("invalid proof: " ^ m))
         | Notations.Json_exception m -> raise (Json_exception ("bad notations: " ^ m))

let to_json proof_with_notations =
    `Assoc [
        ("proof", Proof.to_json proof_with_notations.proof);
        ("notations", Notations.to_json proof_with_notations.notations)
    ]