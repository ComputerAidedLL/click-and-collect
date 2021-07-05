open Raw_sequent

type transform_request =
    | Expand_axiom
    | Expand_axiom_full
    | Eliminate_cut_left
    | Eliminate_cut_key_case
    | Eliminate_cut_right
    | Eliminate_cut_full
    | Eliminate_all_cuts
    | Simplify
    | Substitute of string * raw_formula
    ;;

(* JSON -> TRANSFORM REQUEST *)

exception Json_exception of string;;

let from_json transform_request_as_json =
    try let transformation = Request_utils.get_string transform_request_as_json "transformation" in
        match transformation with
            | "expand_axiom" -> Expand_axiom
            | "expand_axiom_full" -> Expand_axiom_full
            | "eliminate_cut_left" -> Eliminate_cut_left
            | "eliminate_cut_key_case" -> Eliminate_cut_key_case
            | "eliminate_cut_right" -> Eliminate_cut_right
            | "eliminate_cut_full" -> Eliminate_cut_full
            | "eliminate_all_cuts" -> Eliminate_all_cuts
            | "simplify" -> Simplify
            | "substitute" ->
                let alias = Request_utils.get_string transform_request_as_json "alias" in
                let raw_formula = Request_utils.get_raw_formula transform_request_as_json "formula" in
                Substitute (alias, raw_formula)
            | _ -> raise (Json_exception ("unknown transformation '" ^ transformation ^ "'"))
    with Request_utils.Bad_request_exception m -> raise (Json_exception ("bad request: " ^ m));;

(* TRANSFORM REQUEST-> STRING *)

let to_string = function
    | Expand_axiom -> "expand_axiom"
    | Expand_axiom_full -> "expand_axiom_full"
    | Eliminate_cut_left -> "eliminate_cut_left"
    | Eliminate_cut_key_case -> "eliminate_cut_key_case"
    | Eliminate_cut_right -> "eliminate_cut_right"
    | Eliminate_cut_full -> "eliminate_cut_full"
    | Eliminate_all_cuts -> "eliminate_all_cuts"
    | Simplify -> "simplify"
    | Substitute (alias, raw_formula) -> Printf.sprintf "substitute %s ::= %s" alias (Raw_sequent.raw_formula_to_ascii false raw_formula)
    ;;