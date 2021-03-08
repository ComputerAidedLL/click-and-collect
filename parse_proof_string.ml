open Str

exception SyntaxException of string

let thesis_sign = "|-";;

let parse_formulas formulas_as_one_string =
    let formulas_as_string = split (regexp ",") formulas_as_one_string in
    let proof_as_json = String.concat ";" formulas_as_string in
    "\"" ^ proof_as_json ^ "\"";;

let process_hypotheses_and_conclusions = function
    [] -> raise (SyntaxException "Empty sequent")
    | consequences_as_string::[] -> parse_formulas consequences_as_string
    | hypotheses_as_string::consequences_as_string::[] ->
        parse_formulas hypotheses_as_string ^ (parse_formulas consequences_as_string)
    | _ -> raise (SyntaxException "Too many thesis signs");;

let parse proof_as_string =
    let hypotheses_and_conclusions_as_string = split (regexp thesis_sign) proof_as_string in
    process_hypotheses_and_conclusions hypotheses_and_conclusions_as_string;;

let safe_parse proof_as_string =
    try true, parse proof_as_string
    with SyntaxException error_message -> false, error_message;;

let parse_and_print proof_as_string =
    let success, result = safe_parse proof_as_string in
        print_string (Bool.to_string success);
        print_string result;
        print_newline ();;

parse_and_print "hello,world,bar,foo";;
parse_and_print "hello,world|-bar,foo";;
parse_and_print "";;
parse_and_print "hello,world|-bar,foo|-too,much";;