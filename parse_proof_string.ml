open Yojson

let formula_list_to_json formula_list =
    List.map Formula.to_json formula_list;;

let parse proof_as_string =
    let hypotheses, consequences = Parser.main Lexer.token (Lexing.from_string proof_as_string) in
    let sequent_as_json = `Assoc [
        ("hyp", `List (formula_list_to_json hypotheses));
        ("cons", `List (formula_list_to_json consequences))] in
    Yojson.to_string sequent_as_json;;

let safe_parse proof_as_string =
    try true, parse proof_as_string
    with Stdlib.Parsing.Parse_error -> false, "Syntax error: please read the syntax rules";;
