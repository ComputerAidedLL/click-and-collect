open Sequent
open Notations

type sequent_with_notations = {sequent: sequent; notations: notations}

exception Json_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let from_json sequent_with_notations_as_json =
    try
        let notations_as_json = get_key sequent_with_notations_as_json "notations" in
        let notations = Notations.from_json notations_as_json in
        let sequent_as_json = get_key sequent_with_notations_as_json "sequent" in
        let sequent = Raw_sequent.sequent_from_json sequent_as_json in
        {sequent=sequent; notations=notations}
    with Raw_sequent.Json_exception m -> raise (Json_exception ("bad sequent json: " ^ m))
         | Notations.Json_exception m -> raise (Json_exception ("bad notations: " ^ m))

(* OPERATIONS *)

let split_cyclic_acyclic sequent_with_notations =
    let all_variables = get_unique_variable_names sequent_with_notations.sequent in
    Notations.split_cyclic_acyclic sequent_with_notations.notations all_variables

let rec replace_all_notations_in_sequent sequent = function
    | [] -> sequent
    | (s, formula) :: tail -> replace_all_notations_in_sequent (replace_in_sequent (Litt s) formula (replace_in_sequent (Dual s) (dual formula) sequent)) tail
