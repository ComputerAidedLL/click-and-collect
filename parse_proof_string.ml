open Yojson

let parse proof_as_string =
    let sequent = Parser.main Lexer.token (Lexing.from_string proof_as_string) in
    let sequent_as_json = Formula.sequent_to_json sequent in
    Yojson.to_string sequent_as_json;;

let safe_parse proof_as_string =
    try true, parse proof_as_string
    with Stdlib.Parsing.Parse_error -> false, "Syntax error: please read the syntax rules";;
