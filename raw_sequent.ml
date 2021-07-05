open Sequent

(* DEFINITION *)

type raw_formula =
  | One
  | Bottom
  | Top
  | Zero
  | Litt of string
  | Dual of raw_formula
  | Tensor of raw_formula * raw_formula
  | Par of raw_formula * raw_formula
  | With of raw_formula * raw_formula
  | Plus of raw_formula * raw_formula
  | Lollipop of raw_formula * raw_formula
  | Ofcourse of raw_formula
  | Whynot of raw_formula;;

type raw_sequent = {hyp: raw_formula list; cons: raw_formula list};;


(* RAW_SEQUENT -> SEQUENT *)

let rec to_formula = function
    | One -> Sequent.One
    | Bottom -> Sequent.Bottom
    | Top -> Sequent.Top
    | Zero -> Sequent.Zero
    | Litt x -> Sequent.Litt x
    | Dual e -> dual (to_formula e)
    | Tensor (e1, e2) -> Sequent.Tensor (to_formula e1, to_formula e2)
    | Par (e1, e2) -> Sequent.Par (to_formula e1, to_formula e2)
    | With (e1, e2) -> Sequent.With (to_formula e1, to_formula e2)
    | Plus (e1, e2) -> Sequent.Plus (to_formula e1, to_formula e2)
    | Lollipop (e1, e2) -> Sequent.Par (to_formula (Dual e1), to_formula e2)
    | Ofcourse e -> Sequent.Ofcourse (to_formula e)
    | Whynot e -> Sequent.Whynot (to_formula e);;

let to_sequent raw_sequent =
    List.map dual (List.map to_formula raw_sequent.hyp) @ List.map to_formula raw_sequent.cons;;


(* SEQUENT -> RAW_SEQUENT *)

let rec to_raw_formula =
    function
    | Sequent.One -> One
    | Sequent.Bottom -> Bottom
    | Sequent.Top -> Top
    | Sequent.Zero -> Zero
    | Sequent.Litt x -> Litt x
    | Sequent.Dual x -> Dual (Litt x)
    | Sequent.Tensor (e1, e2) -> Tensor (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Par (e1, e2) -> Par (to_raw_formula e1, to_raw_formula e2)
    | Sequent.With (e1, e2) -> With (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Plus (e1, e2) -> Plus (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Ofcourse e -> Ofcourse (to_raw_formula e)
    | Sequent.Whynot e -> Whynot (to_raw_formula e);;

let to_raw_sequent sequent =
    {hyp=[]; cons=List.map to_raw_formula sequent};;

(* REPLACEMENT *)

let rec replace_in_raw_formula alias formula = function
    | Litt s when s = alias -> formula
    | Dual e -> Dual (replace_in_raw_formula alias formula e)
    | Tensor (e1, e2) -> Tensor (replace_in_raw_formula alias formula e1, replace_in_raw_formula alias formula e2)
    | Par (e1, e2) -> Par (replace_in_raw_formula alias formula e1, replace_in_raw_formula alias formula e2)
    | With (e1, e2) -> With (replace_in_raw_formula alias formula e1, replace_in_raw_formula alias formula e2)
    | Plus (e1, e2) -> Plus (replace_in_raw_formula alias formula e1, replace_in_raw_formula alias formula e2)
    | Ofcourse e -> Ofcourse (replace_in_raw_formula alias formula e)
    | Whynot e -> Whynot (replace_in_raw_formula alias formula e)
    | Lollipop (e1, e2) -> Lollipop (replace_in_raw_formula alias formula e1, replace_in_raw_formula alias formula e2)
    | e -> e

(* RAW_SEQUENT -> JSON *)

let rec raw_formula_to_json =
  function
  | One -> `Assoc ([("type", `String "one")])
  | Bottom -> `Assoc ([("type", `String "bottom")])
  | Top -> `Assoc ([("type", `String "top")])
  | Zero -> `Assoc ([("type", `String "zero")])
  | Litt x -> `Assoc ([("type", `String "litt") ; ("value", `String x)])
  | Dual e -> `Assoc ([("type", `String "dual") ; ("value", raw_formula_to_json e)])
  | Tensor (e1, e2) -> `Assoc ([("type", `String "tensor") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Par (e1, e2) -> `Assoc ([("type", `String "par") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | With (e1, e2) -> `Assoc ([("type", `String "with") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Plus (e1, e2) -> `Assoc ([("type", `String "plus") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Lollipop (e1, e2) -> `Assoc ([("type", `String "lollipop") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Ofcourse e -> `Assoc ([("type", `String "ofcourse") ; ("value", raw_formula_to_json e)])
  | Whynot e -> `Assoc ([("type", `String "whynot") ; ("value", raw_formula_to_json e)]);;

let to_json raw_sequent =
    if raw_sequent.hyp = []
    then `Assoc [("cons", `List (List.map raw_formula_to_json raw_sequent.cons))]
    else `Assoc [
        ("hyp", `List (List.map raw_formula_to_json raw_sequent.hyp));
        ("cons", `List (List.map raw_formula_to_json raw_sequent.cons))
    ];;


(* JSON -> RAW_SEQUENT *)

exception Json_exception of string;;

let optional_field json key =
    try Yojson.Basic.Util.member key json
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("a sequent and a formula (or sub-formula) must be a json object"))

let required_field json key =
    let value = optional_field json key in
    if value = `Null
    then raise (Json_exception ("required field '" ^ key ^ "' is missing"))
    else value

let get_json_string json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ key ^ "' must be a string"))

let get_json_list json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ key ^ "' must be a list"))

let rec json_to_raw_formula json =
  let formula_type = get_json_string json "type" in
  match formula_type with
  | "one" -> One
  | "bottom" -> Bottom
  | "top" -> Top
  | "zero" -> Zero
  | "litt" -> Litt (get_json_string json "value")
  | "dual" -> Dual (json_to_raw_formula (required_field json "value"))
  | "tensor" -> Tensor ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "par" -> Par ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "with" -> With ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "plus" -> Plus ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "lollipop" -> Lollipop ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "ofcourse" -> Ofcourse (json_to_raw_formula (required_field json "value"))
  | "whynot" -> Whynot (json_to_raw_formula (required_field json "value"))
  | _ -> raise (Json_exception ("unknown formula type " ^ formula_type));;

let from_json sequent_as_json =
    let hyp_formulas = if (optional_field sequent_as_json "hyp") = `Null then []
    else List.map json_to_raw_formula (get_json_list sequent_as_json "hyp") in
    let cons_formulas = List.map json_to_raw_formula (get_json_list sequent_as_json "cons") in
    {hyp=hyp_formulas; cons=cons_formulas}


(* SEQUENT <-> JSON *)

let sequent_from_json raw_sequent_as_json =
    to_sequent (from_json raw_sequent_as_json);;

let sequent_to_json sequent =
     to_json (to_raw_sequent sequent);;

(* FORMULA <-> JSON *)
let formula_from_json raw_formula_as_json =
    to_formula (json_to_raw_formula raw_formula_as_json);;

let formula_to_json formula =
    raw_formula_to_json (to_raw_formula formula);;

(* OPERATIONS *)
let rec get_variable_names =
    function
    | One -> []
    | Bottom -> []
    | Top -> []
    | Zero -> []
    | Litt x -> [x]
    | Dual e -> get_variable_names e
    | Tensor (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Par (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | With (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Plus (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Lollipop (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Ofcourse e -> get_variable_names e
    | Whynot e -> get_variable_names e;;

let get_unique_variable_names raw_sequent =
    List.sort_uniq String.compare (List.concat_map get_variable_names raw_sequent);;


(* EXPORTS *)

type raw_formula_format = {
    atom_preformat : string -> string;
    litt_format : (string -> string, unit, string) format;
    dual_format : (string -> string, unit, string) format;
    is_dual_atomic : bool;
    is_unary_atomic : bool;
    one_format : string;
    bottom_format : string;
    top_format : string;
    zero_format : string;
    tensor_format : (string -> string -> string, unit, string) format;
    par_format : (string -> string -> string, unit, string) format;
    with_format : (string -> string -> string, unit, string) format;
    plus_format : (string -> string -> string, unit, string) format;
    lollipop_format : (string -> string -> string, unit, string) format;
    ofcourse_format : (string -> string, unit, string) format;
    whynot_format : (string -> string, unit, string) format }

let rec raw_formula_export_atomic formatting =
  let unary_connective f e =
    let s, atomic = raw_formula_export_atomic formatting e in
    let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
    Printf.sprintf f s_parenthesis, formatting.is_unary_atomic in
  let binary_connective f e1 e2 =
    let s1, atomic1 = raw_formula_export_atomic formatting e1 in
    let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
    let s2, atomic2 = raw_formula_export_atomic formatting e2 in
    let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
    Printf.sprintf f s1_parenthesis s2_parenthesis, false in
  function
  | One -> formatting.one_format, true
  | Bottom -> formatting.bottom_format, true
  | Top -> formatting.top_format, true
  | Zero -> formatting.zero_format, true
  | Litt x -> Printf.sprintf formatting.litt_format (formatting.atom_preformat x), true
  | Dual (Litt x) -> Printf.sprintf formatting.dual_format (formatting.atom_preformat x), formatting.is_dual_atomic
  | Dual e -> let s, _atomic = raw_formula_export_atomic formatting e in
              let s_parenthesis = "(" ^ s ^ ")" in
              Printf.sprintf formatting.dual_format s_parenthesis, formatting.is_dual_atomic
  | Tensor (e1, e2) -> binary_connective formatting.tensor_format e1 e2
  | Par (e1, e2) -> binary_connective formatting.par_format e1 e2
  | With (e1, e2) -> binary_connective formatting.with_format e1 e2
  | Plus (e1, e2) -> binary_connective formatting.plus_format e1 e2
  | Lollipop (e1, e2) -> binary_connective formatting.lollipop_format e1 e2
  | Ofcourse e -> unary_connective formatting.ofcourse_format e
  | Whynot e -> unary_connective formatting.whynot_format e


(* SEQUENT -> LATEX *)

let raw_latex_format = {
    atom_preformat = litteral_to_latex;
    litt_format = "%s";
    dual_format = "{%s}\\orth";
    is_dual_atomic = true;
    is_unary_atomic = true;
    one_format = "\\one";
    bottom_format = "\\bot";
    top_format = "\\top";
    zero_format = "\\zero";
    tensor_format = "%s \\tensor %s";
    par_format = "%s \\parr %s";
    with_format = "%s \\with %s";
    plus_format = "%s \\plus %s";
    lollipop_format = "%s \\limp %s";
    ofcourse_format = "\\oc %s";
    whynot_format = "\\wn %s" }

let raw_formula_to_latex formula =
  let s, _ = raw_formula_export_atomic raw_latex_format formula in s


(* SEQUENT -> ASCII *)

let raw_ascii_format utf8 = {
    atom_preformat = (fun x -> x);
    litt_format = "%s";
    dual_format = "%s^";
    is_dual_atomic = true;
    is_unary_atomic = true;
    one_format = "1";
    bottom_format = "_";
    top_format = "T";
    zero_format = "0";
    tensor_format = "%s * %s";
    par_format = "%s | %s";
    with_format = "%s & %s";
    plus_format = "%s + %s";
    lollipop_format = if utf8 then "%s-o %s" else "%s -o %s";
    ofcourse_format = "!%s";
    whynot_format = "?%s" }

let raw_formula_to_ascii utf8 formula =
  let s, _ = raw_formula_export_atomic (raw_ascii_format utf8) formula in s
