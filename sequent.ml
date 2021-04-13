(* DEFINITION *)

type formula =
  | One
  | Bottom
  | Top
  | Zero
  | Litt of string
  | Orth of string
  | Tensor of formula * formula
  | Par of formula * formula
  | With of formula * formula
  | Plus of formula * formula
  | Ofcourse of formula
  | Whynot of formula;;

type sequent = formula list;;


(* OPERATIONS *)

let rec orthogonal =
    function
    | One -> Bottom
    | Bottom -> One
    | Top -> Zero
    | Zero -> Top
    | Litt x -> Orth x
    | Orth x -> Litt x
    | Tensor (e1, e2) -> Par (orthogonal e1, orthogonal e2)
    | Par (e1, e2) -> Tensor (orthogonal e1, orthogonal e2)
    | With (e1, e2) -> Plus (orthogonal e1, orthogonal e2)
    | Plus (e1, e2) -> With (orthogonal e1, orthogonal e2)
    | Ofcourse e -> Whynot (orthogonal e)
    | Whynot e -> Ofcourse (orthogonal e);;

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
    | Orth x -> [x]
    | Tensor (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Par (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | With (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Plus (e1, e2) -> get_variable_names e1 @ get_variable_names e2
    | Ofcourse e -> get_variable_names e
    | Whynot e -> get_variable_names e;;

let get_unique_variable_names sequent =
    List.sort_uniq String.compare (List.concat (List.map get_variable_names sequent))


(* SEQUENT -> COQ *)

let rec formula_to_coq_atomic =
  function
  | One -> "one", true
  | Bottom -> "bot", true
  | Top -> "top", true
  | Zero -> "zero", true
  | Litt x -> x, true
  | Orth x -> Printf.sprintf "dual %s" x, false
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
    Printf.sprintf "ll %s" (formula_list_to_coq sequent);;


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
  | Orth x -> Printf.sprintf "{%s}\\orth" x, true
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

let sequent_to_latex sequent =
    String.concat ", " (List.map formula_to_latex sequent);;
