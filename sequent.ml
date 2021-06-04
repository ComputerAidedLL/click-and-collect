(* DEFINITION *)

type formula =
  | One
  | Bottom
  | Top
  | Zero
  | Litt of string
  | Dual of string
  | Tensor of formula * formula
  | Par of formula * formula
  | With of formula * formula
  | Plus of formula * formula
  | Ofcourse of formula
  | Whynot of formula;;

type sequent = formula list;;


(* OPERATIONS *)

let rec sequent_to_formula = function
  | [] -> Bottom
  | [f] -> f
  | f1 :: f2 :: context -> sequent_to_formula (Par (f1, f2) :: context)

let rec dual =
    function
    | One -> Bottom
    | Bottom -> One
    | Top -> Zero
    | Zero -> Top
    | Litt x -> Dual x
    | Dual x -> Litt x
    | Tensor (e1, e2) -> Par (dual e1, dual e2)
    | Par (e1, e2) -> Tensor (dual e1, dual e2)
    | With (e1, e2) -> Plus (dual e1, dual e2)
    | Plus (e1, e2) -> With (dual e1, dual e2)
    | Ofcourse e -> Whynot (dual e)
    | Whynot e -> Ofcourse (dual e);;

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
    | Dual x -> [x]
    | Tensor (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Par (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | With (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Plus (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Ofcourse e -> get_variable_names e
    | Whynot e -> get_variable_names e;;

let get_unique_variable_names sequent =
    List.sort_uniq String.compare (List.concat (List.map get_variable_names sequent));;

let rec count_notation_in_formula notation_name = function
    | Litt x when x = notation_name -> 1
    | Dual x when x = notation_name -> 1
    | Tensor (e1, e2) | Par (e1, e2) | With (e1, e2) | Plus (e1, e2) ->
        count_notation_in_formula notation_name e1 + count_notation_in_formula notation_name e2
    | Ofcourse e | Whynot e -> count_notation_in_formula notation_name e
    | _ -> 0;;

let count_notation notation_name sequent =
    List.fold_right (fun f n -> count_notation_in_formula notation_name f + n) sequent 0

let sort sequent =
    List.sort compare sequent;;

let rec replace_in_formula alias formula = function
    | Litt s when s = alias -> formula
    | Dual s when s = alias -> dual formula
    | Tensor (e1, e2) -> Tensor (replace_in_formula alias formula e1, replace_in_formula alias formula e2)
    | Par (e1, e2) -> Par (replace_in_formula alias formula e1, replace_in_formula alias formula e2)
    | With (e1, e2) -> With (replace_in_formula alias formula e1, replace_in_formula alias formula e2)
    | Plus (e1, e2) -> Plus (replace_in_formula alias formula e1, replace_in_formula alias formula e2)
    | Ofcourse e -> Ofcourse (replace_in_formula alias formula e)
    | Whynot e -> Whynot (replace_in_formula alias formula e)
    | f -> f;;

let replace_in_sequent alias formula sequent =
     List.map (replace_in_formula alias formula) sequent;;

let rec partial_replace_in_formula alias formula = function
    | Litt s when s = alias -> [formula; Litt s]
    | Dual s when s = alias -> [dual formula; Dual s]
    | Tensor (e1, e2) ->
        let l1 = partial_replace_in_formula alias formula e1 in
        let l2 = partial_replace_in_formula alias formula e2 in
        List.concat (List.map (fun f1 -> List.map (fun f2 -> Tensor (f1, f2)) l2) l1)
    | Par (e1, e2) ->
        let l1 = partial_replace_in_formula alias formula e1 in
        let l2 = partial_replace_in_formula alias formula e2 in
        List.concat (List.map (fun f1 -> List.map (fun f2 -> Par (f1, f2)) l2) l1)
    | With (e1, e2) ->
        let l1 = partial_replace_in_formula alias formula e1 in
        let l2 = partial_replace_in_formula alias formula e2 in
        List.concat (List.map (fun f1 -> List.map (fun f2 -> With (f1, f2)) l2) l1)
    | Plus (e1, e2) ->
        let l1 = partial_replace_in_formula alias formula e1 in
        let l2 = partial_replace_in_formula alias formula e2 in
        List.concat (List.map (fun f1 -> List.map (fun f2 -> Plus (f1, f2)) l2) l1)
    | Ofcourse e ->
        let l = partial_replace_in_formula alias formula e in
        List.map (fun f -> Ofcourse f) l
    | Whynot e ->
        let l = partial_replace_in_formula alias formula e in
        List.map (fun f -> Whynot f) l
    | f -> [f];;

let rec partial_replace_in_sequent alias formula = function
    | [] -> [[]]
    | e :: tail ->
        let formulas = partial_replace_in_formula alias formula e in
        let tails = partial_replace_in_sequent alias formula tail in
        List.concat (List.map (fun f -> List.map (fun t -> f::t) tails) formulas);;

(* PATTERN MATCHING ON FORMULA *)

let is_top = function | Top -> true | _ -> false;;
let is_bottom = function | Bottom -> true | _ -> false;;
let is_par = function | Par _ -> true | _ -> false;;
let is_with = function | With _ -> true | _ -> false;;
let is_ofcourse = function | Ofcourse _ -> true | _ -> false;;


(* EXPORTS *)

type formula_format = {
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
    ofcourse_format : (string -> string, unit, string) format;
    whynot_format : (string -> string, unit, string) format }

let rec formula_export_atomic formatting =
  let unary_connective f e =
    let s, atomic = formula_export_atomic formatting e in
    let s_parenthesis = if atomic then s else "(" ^ s ^ ")" in
    Printf.sprintf f s_parenthesis, formatting.is_unary_atomic in
  let binary_connective f e1 e2 =
    let s1, atomic1 = formula_export_atomic formatting e1 in
    let s1_parenthesis = if atomic1 then s1 else "(" ^ s1 ^ ")" in
    let s2, atomic2 = formula_export_atomic formatting e2 in
    let s2_parenthesis = if atomic2 then s2 else "(" ^ s2 ^ ")" in
    Printf.sprintf f s1_parenthesis s2_parenthesis, false in
  function
  | One -> formatting.one_format, true
  | Bottom -> formatting.bottom_format, true
  | Top -> formatting.top_format, true
  | Zero -> formatting.zero_format, true
  | Litt x -> Printf.sprintf formatting.litt_format (formatting.atom_preformat x), true
  | Dual x -> Printf.sprintf formatting.dual_format (formatting.atom_preformat x), formatting.is_dual_atomic
  | Tensor (e1, e2) -> binary_connective formatting.tensor_format e1 e2
  | Par (e1, e2) -> binary_connective formatting.par_format e1 e2
  | With (e1, e2) -> binary_connective formatting.with_format e1 e2
  | Plus (e1, e2) -> binary_connective formatting.plus_format e1 e2
  | Ofcourse e -> unary_connective formatting.ofcourse_format e
  | Whynot e -> unary_connective formatting.whynot_format e


(* SEQUENT -> COQ *)

let coq_format = {
    atom_preformat = (fun x -> x);
    litt_format = "%s";
    dual_format = "dual %s";
    is_dual_atomic = false;
    is_unary_atomic = false;
    one_format = "one";
    bottom_format = "bot";
    top_format = "top";
    zero_format = "zero";
    tensor_format = "tens %s %s";
    par_format = "parr %s %s";
    with_format = "awith %s %s";
    plus_format = "aplus %s %s";
    ofcourse_format = "oc %s";
    whynot_format = "wn %s" }

let formula_to_coq formula =
  let s, _ = formula_export_atomic coq_format formula in s

let formula_list_to_coq formula_list =
    Printf.sprintf "[%s]" (String.concat "; " (List.map formula_to_coq formula_list));;

let sequent_to_coq sequent =
    Printf.sprintf "ll %s" (formula_list_to_coq sequent);;


(* SEQUENT -> LATEX *)

let litteral_to_latex s =
   (* Set numbers as indices. E.g.: A01' -> A_{01}' *)
    Str.global_replace (Str.regexp "\\([0-9]+\\)") "_{\\1}" s;;

let latex_format = {
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
    ofcourse_format = "\\oc %s";
    whynot_format = "\\wn %s" }

let formula_to_latex formula =
  let s, _ = formula_export_atomic latex_format formula in s

let sequent_to_latex sequent =
    String.concat ", " (List.map formula_to_latex sequent);;


(* SEQUENT -> ASCII *)

let ascii_format = {
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
    ofcourse_format = "!%s";
    whynot_format = "?%s" }

let formula_to_ascii formula =
  let s, _ = formula_export_atomic ascii_format formula in s

let sequent_to_ascii utf8 sequent =
  (if utf8 then "|-" else "|- ")
  ^ (String.concat ", " (List.map formula_to_ascii sequent));;
