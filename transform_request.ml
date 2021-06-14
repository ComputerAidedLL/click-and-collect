type transform_request =
    | Expand_axiom
    | Expand_axiom_full
    | Eliminate_cut_left
    | Eliminate_cut_key_case
    | Eliminate_cut_right
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
            | _ -> raise (Json_exception ("unknown transformation '" ^ transformation ^ "'"))
    with Request_utils.Bad_request_exception m -> raise (Json_exception ("bad request: " ^ m));;

(* TRANSFORM REQUEST-> STRING *)

let to_string = function
    | Expand_axiom -> "expand_axiom"
    | Expand_axiom_full -> "expand_axiom_full"
    | Eliminate_cut_left -> "eliminate_cut_left"
    | Eliminate_cut_key_case -> "eliminate_cut_key_case"
    | Eliminate_cut_right -> "eliminate_cut_right"
    ;;