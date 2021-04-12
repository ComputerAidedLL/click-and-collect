(* DEFINITION *)

type formula =
  | One
  | Bottom
  | Top
  | Zero
  | Litt of string
  | Orth of formula
  | Tensor of formula * formula
  | Par of formula * formula
  | With of formula * formula
  | Plus of formula * formula
  | Lollipop of formula * formula
  | Ofcourse of formula
  | Whynot of formula;;

type sequent = {hyp: formula list; cons: formula list};;

(* SEQUENT -> JSON *)

let rec formula_to_json =
  function
  | One -> `Assoc ([("type", `String "neutral") ; ("value", `String "one")])
  | Bottom -> `Assoc ([("type", `String "neutral") ; ("value", `String "bottom")])
  | Top -> `Assoc ([("type", `String "neutral") ; ("value", `String "top")])
  | Zero -> `Assoc ([("type", `String "neutral") ; ("value", `String "zero")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Orth e -> `Assoc ([("type", `String "orthogonal") ; ("value", formula_to_json e)])
  | Tensor (e1, e2) -> `Assoc ([("type", `String "tensor") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Par (e1, e2) -> `Assoc ([("type", `String "par") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | With (e1, e2) -> `Assoc ([("type", `String "with") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Plus (e1, e2) -> `Assoc ([("type", `String "plus") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Lollipop (e1, e2) -> `Assoc ([("type", `String "lollipop") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Ofcourse e -> `Assoc ([("type", `String "ofcourse") ; ("value", formula_to_json e)])
  | Whynot e -> `Assoc ([("type", `String "whynot") ; ("value", formula_to_json e)]);;

let sequent_to_json sequent = `Assoc [
        ("hyp", `List (List.map formula_to_json sequent.hyp));
        ("cons", `List (List.map formula_to_json sequent.cons))
    ];;


(* JSON -> SEQUENT *)

exception Json_exception of string;;

let required_field json key =
    let value =
        try Yojson.Basic.Util.member key json
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("a sequent and a formula (or sub-formula) must be a json object"))
    in
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

let rec json_to_formula json =
  let formula_type = get_json_string json "type" in
  match formula_type with
  "neutral" -> ( let neutral_value = get_json_string json "value" in
     match neutral_value with
       "one" -> One
       | "bottom" -> Bottom
       | "top" -> Top
       | "zero" -> Zero
       | _ -> raise (Json_exception ("unknown neutral value " ^ neutral_value)) )
  | "litteral" -> Litt (get_json_string json "value")
  | "orthogonal" -> Orth (json_to_formula (required_field json "value"))
  | "tensor" -> Tensor ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "par" -> Par ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "with" -> With ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "plus" -> Plus ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "lollipop" -> Lollipop ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "ofcourse" -> Ofcourse (json_to_formula (required_field json "value"))
  | "whynot" -> Whynot (json_to_formula (required_field json "value"))
  | _ -> raise (Json_exception ("unknown formula type " ^ formula_type));;

let from_json sequent_as_json =
    let hyp_formulas = List.map json_to_formula (get_json_list sequent_as_json "hyp") in
    let cons_formulas = List.map json_to_formula (get_json_list sequent_as_json "cons") in
    {hyp=hyp_formulas; cons=cons_formulas}


(* SEQUENT -> COQ *)

let rec formula_to_coq_atomic =
  function
  | One -> "one", true
  | Bottom -> "bot", true
  | Top -> "top", true
  | Zero -> "zero", true
  | Litt x -> x, true
  | Orth e ->
      let s, atomic = formula_to_coq_atomic e in
      let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
      Printf.sprintf "dual %s" s_parenthesis, false
  | Tensor (e1, e2) ->
      let s1, atomic1 = formula_to_coq_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_coq_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "tens %s %s" s1_parenthesis s2_parenthesis, false
  | Par (e1, e2) ->
      let s1, atomic1 = formula_to_coq_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_coq_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "parr %s %s" s1_parenthesis s2_parenthesis, false
  | With (e1, e2) ->
      let s1, atomic1 = formula_to_coq_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_coq_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "awith %s %s" s1_parenthesis s2_parenthesis, false
  | Plus (e1, e2) ->
      let s1, atomic1 = formula_to_coq_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_coq_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "aplus %s %s" s1_parenthesis s2_parenthesis, false
  | Lollipop (e1, e2) -> formula_to_coq_atomic (Par (Orth e1, e2))
  | Ofcourse e ->
      let s, atomic = formula_to_coq_atomic e in
      let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
      Printf.sprintf "oc %s" s_parenthesis, false
  | Whynot e ->
      let s, atomic = formula_to_coq_atomic e in
      let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
      Printf.sprintf "wn %s" s_parenthesis, false

let formula_to_coq formula =
  let s, _ = formula_to_coq_atomic formula in s

let formula_list_to_coq formula_list =
    Printf.sprintf "[%s]" (String.concat "; " (List.map formula_to_coq formula_list));;

let sequent_to_coq sequent =
    Printf.sprintf "ll %s" (formula_list_to_coq sequent.cons)


(* SEQUENT -> LATEX *)

let litteral_to_latex s =
   (* Set numbers as indices. E.g.: A01' -> A_{01}' *)
    Str.global_replace (Str.regexp "\\([0-9]+\\)") "_{\\1}" s;;

let rec formula_to_latex_atomic =
  function
  | One -> "\\one", true
  | Bottom -> "\\bot", true
  | Top -> "\\top", true
  | Zero -> "\\zero", true
  | Litt x -> litteral_to_latex x, true
  | Orth e ->
      let s, atomic = formula_to_latex_atomic e in
      let s_parenthesis = if atomic then "{" ^ s ^ "}" else "(" ^ s ^ ")" in
      Printf.sprintf "%s\\orth" s_parenthesis, true
  | Tensor (e1, e2) ->
      let s1, atomic1 = formula_to_latex_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_latex_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "%s \\tensor %s" s1_parenthesis s2_parenthesis, false
  | Par (e1, e2) ->
      let s1, atomic1 = formula_to_latex_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_latex_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "%s \\parr %s" s1_parenthesis s2_parenthesis, false
  | With (e1, e2) ->
      let s1, atomic1 = formula_to_latex_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_latex_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "%s \\with %s" s1_parenthesis s2_parenthesis, false
  | Plus (e1, e2) ->
      let s1, atomic1 = formula_to_latex_atomic e1 in
      let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
      let s2, atomic2 = formula_to_latex_atomic e2 in
      let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
      Printf.sprintf "%s \\plus %s" s1_parenthesis s2_parenthesis, false
  | Lollipop (e1, e2) -> formula_to_latex_atomic (Par (Orth e1, e2))
  | Ofcourse e ->
      let s, atomic = formula_to_latex_atomic e in
      let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
      Printf.sprintf "\\oc %s" s_parenthesis, true
  | Whynot e ->
      let s, atomic = formula_to_latex_atomic e in
      let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
      Printf.sprintf "\\wn %s" s_parenthesis, true

let formula_to_latex formula =
  let s, _ = formula_to_latex_atomic formula in s

let formula_list_to_latex formula_list =
    String.concat ", " (List.map formula_to_latex formula_list);;

let sequent_to_latex sequent =
    formula_list_to_latex sequent.cons


(* OPERATIONS *)

let rec orthogonal =
    function
    | One -> Bottom
    | Bottom -> One
    | Top -> Zero
    | Zero -> Top
    | Litt x -> Orth (Litt x)
    | Orth e -> e
    | Tensor (e1, e2) -> Par (orthogonal e1, orthogonal e2)
    | Par (e1, e2) -> Tensor (orthogonal e1, orthogonal e2)
    | With (e1, e2) -> Plus (orthogonal e1, orthogonal e2)
    | Plus (e1, e2) -> With (orthogonal e1, orthogonal e2)
    | Lollipop (e1, e2) -> Tensor (e1, orthogonal e2)
    | Ofcourse e -> Whynot (orthogonal e)
    | Whynot e -> Ofcourse (orthogonal e);;

let rec simplify =
    function
    | One -> One
    | Bottom -> Bottom
    | Top -> Top
    | Zero -> Zero
    | Litt x -> Litt x
    | Orth e -> orthogonal (simplify e)
    | Tensor (e1, e2) -> Tensor (simplify e1, simplify e2)
    | Par (e1, e2) -> Par (simplify e1, simplify e2)
    | With (e1, e2) -> With (simplify e1, simplify e2)
    | Plus (e1, e2) -> Plus (simplify e1, simplify e2)
    | Lollipop (e1, e2) -> Par (orthogonal (simplify e1), simplify e2)
    | Ofcourse e -> Ofcourse (simplify e)
    | Whynot e -> Whynot (simplify e);;

let get_monolatery_sequent sequent =
    {hyp=[]; cons=List.map simplify sequent.cons @ List.map orthogonal (List.map simplify sequent.hyp)};;

exception Not_whynot;;

let rec remove_whynot = function
    | [] -> []
    | Whynot e :: l -> e :: remove_whynot l
    | _ -> raise Not_whynot;;

let rec add_whynot = function
    | [] -> []
    | e :: l -> Whynot e :: add_whynot l;;

let rec get_variable_names =
    function
    | One -> []
    | Bottom -> []
    | Top -> []
    | Zero -> []
    | Litt x -> [x]
    | Orth e -> get_variable_names e
    | Tensor (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Par (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | With (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Plus (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Lollipop (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Ofcourse e -> get_variable_names e
    | Whynot e -> get_variable_names e;;

let get_unique_variable_names sequent =
    let hyp_variables_with_duplicates = List.concat (List.map get_variable_names sequent.hyp) in
    let cons_variables_with_duplicates = List.concat (List.map get_variable_names sequent.cons) in
    List.sort_uniq String.compare (hyp_variables_with_duplicates @ cons_variables_with_duplicates)
