open Sequent

type notations = (string * formula) list;;

exception Json_exception of string;;

let pair_from_json pair_as_json =
    try let pair_as_list = Yojson.Basic.Util.to_list pair_as_json in
        (if List.length pair_as_list <> 2 then raise (Json_exception "a notation pair must be a list of exactly two elements"));
        let notation_name = Yojson.Basic.Util.to_string (List.hd pair_as_list) in
        let formula = Raw_sequent.formula_from_json (List.nth pair_as_list 1) in
        notation_name, formula
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception "a notation pair must be a list of a string and a formula")

let pair_to_json pair =
    let notation_name, formula = pair in
    `List [`String notation_name; Raw_sequent.formula_to_json formula]

let from_json notations_as_json =
    try List.map pair_from_json (Yojson.Basic.Util.to_list notations_as_json)
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception "notations must be a list of notation pair")

let to_json notations =
    `List (List.map pair_to_json notations)

