type transform_request =
    | Expand_axiom;;

(* JSON -> TRANSFORM REQUEST *)

exception Json_exception of string;;

let from_json transform_request_as_json =
    try let transformation = Request_utils.get_string transform_request_as_json "transformation" in
        match transformation with
            | "expand_axiom" -> Expand_axiom
            | _ -> raise (Json_exception ("unknown transformation '" ^ transformation ^ "'"))
    with Request_utils.Bad_request_exception m -> raise (Json_exception ("bad request: " ^ m));;

(* TRANSFORM REQUEST-> STRING *)

let to_string = function
    | Expand_axiom -> "expand_axiom"
    ;;