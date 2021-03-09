#use "topfind";;
#require "yojson";;
#require "str";;
#use "parse_proof_string.ml";;

let parse_and_print proof_as_string =
    print_string (proof_as_string ^ "  ");
    let success, result = safe_parse proof_as_string in
        print_string (Bool.to_string success ^ "  ");
        print_string result;
        print_newline ();;

parse_and_print "hello,world,bar,foo";;
parse_and_print "hello,world|-bar,foo";;
parse_and_print "";;
parse_and_print "hello,world|-bar,foo|-too,much";;
parse_and_print "hello,world|-|-";;

let person1 = `Assoc [ ("success", `Bool true); ("proof_as_json", `String "Anil")];;
print_string (Yojson.to_string person1);;
print_newline ();
