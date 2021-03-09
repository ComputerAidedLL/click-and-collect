open Str
open Yojson

exception SyntaxException of string

let thesis_sign = "|-";;

let parse_formula formula =
    `String formula;;

let parse_formulas formulas_as_one_string =
    let formulas_as_string = split (regexp ",") formulas_as_one_string in
    List.map parse_formula formulas_as_string;;

let process_hypotheses_and_conclusions = function
    [] -> `Assoc [
        ("cons", `List [])]
    | consequences_as_string::[] -> `Assoc [
        ("cons", `List (parse_formulas consequences_as_string))]
    | hypotheses_as_string::consequences_as_string::[] -> `Assoc [
        ("hyp", `List (parse_formulas hypotheses_as_string));
        ("cons", `List (parse_formulas consequences_as_string))]
    | _ -> raise (SyntaxException "Too many thesis signs");;

let parse proof_as_string =
    let hypotheses_and_conclusions_as_string = split_delim (regexp thesis_sign) proof_as_string in
    Yojson.to_string (process_hypotheses_and_conclusions hypotheses_and_conclusions_as_string);;

let safe_parse proof_as_string =
    try true, parse proof_as_string
    with SyntaxException error_message -> false, error_message;;
