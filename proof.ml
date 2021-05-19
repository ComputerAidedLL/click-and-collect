open Sequent
open Rule_request

(* PROOF *)

type proof =
    | Axiom_proof of formula
    | One_proof
    | Top_proof of formula list * formula list
    | Bottom_proof of formula list * formula list * proof
    | Tensor_proof of formula list * formula * formula * formula list * proof * proof
    | Par_proof of formula list * formula * formula * formula list * proof
    | With_proof of formula list * formula * formula * formula list * proof * proof
    | Plus_left_proof of formula list * formula * formula * formula list * proof
    | Plus_right_proof of formula list * formula * formula * formula list * proof
    | Promotion_proof of formula list * formula * formula list * proof
    | Dereliction_proof of formula list * formula * formula list * proof
    | Weakening_proof of formula list * formula * formula list * proof
    | Contraction_proof of formula list * formula * formula list * proof
    | Exchange_proof of sequent * int list * int list * proof
    | Cut_proof of formula list * formula * formula list * proof * proof
    | Hypothesis_proof of sequent;;


(* PERMUTATIONS *)

let identity n = List.init n (fun k -> k)

let is_valid_permutation l =
    let sorted_l = List.sort Int.compare l in
    sorted_l = identity (List.length l);;

let permute l =
    List.map (List.nth l);;

let rec position_in_list a = function
    | [] -> raise (Failure "Not found")
    | h :: tl -> if a = h then 0 else 1 + position_in_list a tl

let permutation_inverse perm =
    List.map (fun x -> position_in_list x perm) (identity (List.length perm))


(* GETTERS & SETTERS *)

let get_premises = function
    | Axiom_proof _ -> []
    | One_proof -> []
    | Top_proof (_, _) -> []
    | Bottom_proof (_, _, p) -> [p]
    | Tensor_proof (_, _, _, _, p1, p2) -> [p1; p2]
    | Par_proof (_, _, _, _, p) -> [p]
    | With_proof (_, _, _, _, p1, p2) -> [p1; p2]
    | Plus_left_proof (_, _, _, _, p) -> [p]
    | Plus_right_proof (_, _, _, _, p) -> [p]
    | Promotion_proof (_, _, _, p) -> [p]
    | Dereliction_proof (_, _, _, p) -> [p]
    | Weakening_proof (_, _, _, p) -> [p]
    | Contraction_proof (_, _, _, p) -> [p]
    | Exchange_proof (_, _, _, p) -> [p]
    | Cut_proof (_, _, _, p1, p2) -> [p1; p2]
    | Hypothesis_proof s -> raise (Failure "Can not get premises of hypothesis");;

let set_premises proof premises = match proof, premises with
    | Axiom_proof _, [] -> proof
    | One_proof, [] -> proof
    | Top_proof (_, _), [] -> proof
    | Bottom_proof (head, tail, _), [p] -> Bottom_proof (head, tail, p)
    | Tensor_proof (head, e1, e2, tail, _, _), [p1; p2] -> Tensor_proof (head, e1, e2, tail, p1, p2)
    | Par_proof (head, e1, e2, tail, _), [p]  -> Par_proof (head, e1, e2, tail, p)
    | With_proof (head, e1, e2, tail, _, _), [p1; p2] -> With_proof (head, e1, e2, tail, p1, p2)
    | Plus_left_proof (head, e1, e2, tail, _), [p] -> Plus_left_proof (head, e1, e2, tail, p)
    | Plus_right_proof (head, e1, e2, tail, _), [p] -> Plus_right_proof (head, e1, e2, tail, p)
    | Promotion_proof (head_without_whynot, e, tail_without_whynot, _), [p] ->
        Promotion_proof (head_without_whynot, e, tail_without_whynot, p)
    | Dereliction_proof (head, e, tail, _), [p] -> Dereliction_proof (head, e, tail, p)
    | Weakening_proof (head, e, tail, _), [p] -> Weakening_proof (head, e, tail, p)
    | Contraction_proof (head, e, tail, _), [p] -> Contraction_proof (head, e, tail, p)
    | Exchange_proof (sequent, display_permutation, permutation, _), [p] -> Exchange_proof (sequent, display_permutation, permutation, p)
    | Cut_proof (head, e, tail, _, _), [p1; p2] -> Cut_proof (head, e, tail, p1, p2)
    | Hypothesis_proof sequent, _ -> raise (Failure "Can not set premises of hypothesis")
    | _ -> raise (Failure "Number of premises mismatch with given proof");;

let get_conclusion = function
    | Axiom_proof e -> [e; dual e]
    | One_proof -> [Sequent.One]
    | Top_proof (head, tail) -> head @ [Sequent.Top] @ tail
    | Bottom_proof (head, tail, _) -> head @ [Sequent.Bottom] @ tail
    | Tensor_proof (head, e1, e2, tail, _, _) -> head @ [Sequent.Tensor (e1, e2)] @ tail
    | Par_proof (head, e1, e2, tail, _) -> head @ [Sequent.Par (e1, e2)] @ tail
    | With_proof (head, e1, e2, tail, _, _) -> head @ [Sequent.With (e1, e2)] @ tail
    | Plus_left_proof (head, e1, e2, tail, _) -> head @ [Plus (e1, e2)] @ tail
    | Plus_right_proof (head, e1, e2, tail, _) -> head @ [Plus (e1, e2)] @ tail
    | Promotion_proof (head_without_whynot, e, tail_without_whynot, _) ->
        let head = Sequent.add_whynot head_without_whynot in
        let tail = Sequent.add_whynot tail_without_whynot in
        head @ [Ofcourse e] @ tail
    | Dereliction_proof (head, e, tail, _) -> head @ [Whynot e] @ tail
    | Weakening_proof (head, e, tail, _) -> head @ [Whynot e] @ tail
    | Contraction_proof (head, e, tail, _) -> head @ [Whynot e] @ tail
    | Exchange_proof (sequent, _, permutation, _) -> permute sequent permutation
    | Cut_proof (head, _, tail, _, _) -> head @ tail
    | Hypothesis_proof sequent -> sequent;;


(* SEQUENT & RULE_REQUEST -> PROOF *)

exception Rule_exception of bool * string;;

let rec head_formula_tail n = function
    | [] -> raise (Rule_exception (false, "Argument formula_position is greater than the sequent"))
    | f :: formula_list -> if n = 0 then [], f, formula_list
        else let head, formula, tail = head_formula_tail (n - 1) formula_list
        in f::head, formula, tail;;

let rec slice l n =
    if n = 0 then [], l else
    match l with
    | [] -> raise (Rule_exception (false, "Argument formula_position is greater than the sequent"))
    | e :: l' -> let head, tail = slice l' (n - 1) in e::head, tail;;

let from_sequent_and_rule_request sequent = function
        | Axiom -> (
            match sequent with
            | [e1; e2] -> (if dual e1 <> e2
                then raise (Rule_exception (true, "Can not apply 'axiom' rule: the two formulas are not orthogonal.")));
                Axiom_proof e1
            | _ -> raise (Rule_exception (true, "Can not apply 'axiom' rule: the sequent must contain exactly two formulas."))
        )
        | One -> (
            match sequent with
            | [One] -> One_proof
            | _ -> raise (Rule_exception (true, "Can not apply 'one' rule: the sequent must be reduced to the single formula '1'."))
        )
        | Bottom n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            | Bottom -> Bottom_proof (head, tail, (Hypothesis_proof (head @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply bottom rule on this formula"))
        )
        | Top n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            | Top -> Top_proof (head, tail)
            | _ -> raise (Rule_exception (false, "Cannot apply top rule on this formula"))
        )
        | Zero -> raise (Rule_exception (true, "Can not apply 'zero' rule: there is no rule for introducing '0'."))
        | Tensor n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Tensor (e1, e2) -> Tensor_proof (head, e1, e2, tail, (Hypothesis_proof (head @ [e1])), (Hypothesis_proof ([e2] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply tensor rule on this formula"))
        )
        | Par n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Par (e1, e2) -> Par_proof (head, e1, e2, tail, (Hypothesis_proof (head @ [e1; e2] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply par rule on this formula"))
        )
        | With n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            With (e1, e2) -> With_proof (head, e1, e2, tail, (Hypothesis_proof (head @ [e1] @ tail)), (Hypothesis_proof (head @ [e2] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply with rule on this formula"))
        )
        | Plus_left n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Plus (e1, e2) -> Plus_left_proof (head, e1, e2, tail, (Hypothesis_proof (head @ [e1] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply plus_left rule on this formula"))
        )
        | Plus_right n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Plus (e1, e2) -> Plus_right_proof (head, e1, e2, tail, (Hypothesis_proof (head @ [e2] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply plus_right rule on this formula"))
        )
        | Promotion n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Ofcourse e -> (try
                let head_without_whynot = Sequent.remove_whynot head in
                let tail_without_whynot = remove_whynot tail in
                Promotion_proof (head_without_whynot, e, tail_without_whynot, (Hypothesis_proof (head @ [e] @ tail)))
                with Sequent.Not_whynot -> raise (Rule_exception (true, "Can not apply 'promotion' rule: the context must contain formulas starting by '?' only.")))
            | _ -> raise (Rule_exception (false, "Cannot apply promotion rule on this formula"))
        )
        | Dereliction n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Whynot e -> Dereliction_proof (head, e, tail, (Hypothesis_proof (head @ [e] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply dereliction rule on this formula"))
        )
        | Weakening n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Whynot e -> Weakening_proof (head, e, tail, (Hypothesis_proof (head @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply weakening rule on this formula"))
        )
        | Contraction n -> (
            let head, formula, tail = head_formula_tail n sequent in
            match formula with
            Whynot e -> Contraction_proof (head, e, tail, (Hypothesis_proof (head @ [Whynot e; Whynot e] @ tail)))
            | _ -> raise (Rule_exception (false, "Cannot apply contraction rule on this formula"))
        )
        | Exchange (display_permutation, permutation) -> (
            if List.length sequent <> List.length permutation
            then raise (Rule_exception (false, "When applying exchange rule, formula_positions and sequent must have same size"))
            else if not (is_valid_permutation permutation)
            then raise (Rule_exception (false, "When applying exchange rule, formula_positions should be a permutation of the size of sequent formula list"))
            else let permuted_sequent = permute sequent (permutation_inverse permutation) in
                 Exchange_proof (permuted_sequent, display_permutation, permutation, Hypothesis_proof permuted_sequent)
        )
        | Cut (formula, n) -> (
            let head, tail = slice sequent n in
            Cut_proof (head, formula, tail, Hypothesis_proof (head @ [formula]), Hypothesis_proof ([dual formula] @ tail))
        );;

let from_sequent_and_rule_request_and_premises sequent rule_request premises =
    let proof = from_sequent_and_rule_request sequent rule_request in
    let expected_premises_conclusion = List.map get_conclusion (get_premises proof) in
    let given_premises_conclusion = List.map get_conclusion premises in
    if expected_premises_conclusion <> given_premises_conclusion then
    raise (Rule_exception (false, Printf.sprintf "Premises conclusion do not match expected premises conclusion after applying rule %s}" (Yojson.Basic.to_string (Rule_request.to_json rule_request))))
    else set_premises proof premises;;

(* AUTO REVERSE MODE *)

exception NotApplicable;;

let rec get_formula_position condition = function
    | [] -> raise NotApplicable
    | f :: tail -> if condition f then 0 else 1 + (get_formula_position condition tail);;

let try_rule_request sequent rule_request =
    try from_sequent_and_rule_request sequent rule_request
    with Rule_exception _ -> raise NotApplicable;;

let apply_reversible_rule proof =
    let sequent = get_conclusion proof in
    try try_rule_request sequent (Top (get_formula_position is_top sequent))
        with NotApplicable ->
    try try_rule_request sequent (Bottom (get_formula_position is_bottom sequent))
        with NotApplicable ->
    try try_rule_request sequent (Par (get_formula_position is_par sequent))
        with NotApplicable ->
    try try_rule_request sequent (With (get_formula_position is_with sequent))
        with NotApplicable ->
    try try_rule_request sequent (Promotion (get_formula_position is_ofcourse sequent))
        with NotApplicable ->
    try try_rule_request sequent One
        with NotApplicable ->
    try if List.length sequent = 1 then try_rule_request sequent (Tensor 0) else raise NotApplicable
        with NotApplicable ->
    try try_rule_request sequent Axiom
        with NotApplicable ->
    proof;;

let rec rec_apply_reversible_rule proof =
    let new_proof = apply_reversible_rule proof in
    match new_proof with
        | Hypothesis_proof _ -> new_proof
        | _ -> let premises = get_premises new_proof in
            let new_premises = List.map rec_apply_reversible_rule premises in
            set_premises new_proof new_premises;;

(* AUTO WEAK MODE *)
exception AutoWeakNotApplicable;;

let rec auto_weak = function
    | [] -> raise AutoWeakNotApplicable
    | [Sequent.One] -> One_proof
    | [e] -> raise AutoWeakNotApplicable
    | [e1; e2] when e1 = dual e2 -> Axiom_proof e1
    | [One; Whynot f] -> Weakening_proof ([One], f, [], One_proof)
    | [Whynot f; One] -> Weakening_proof ([], f, [One], One_proof)
    | [e1; e2] -> raise AutoWeakNotApplicable
    | Whynot f :: tail when not (List.mem (dual (Whynot f)) tail) ->
        Weakening_proof ([], f, tail, auto_weak tail)
    | e :: Whynot f :: tail when e <> dual (Whynot f) ->
       Weakening_proof ([e], f, tail, auto_weak (e :: tail))
    | e1 :: e2 :: Whynot f :: tail ->
       Weakening_proof ([e1; e2], f, tail, auto_weak (e1 :: e2 :: tail))
    | _ -> raise AutoWeakNotApplicable;;

(* PROOF -> RULE REQUEST *)

let get_rule_request = function
    | Axiom_proof _ -> Axiom
    | One_proof -> One
    | Top_proof (head, _) -> Top (List.length head)
    | Bottom_proof (head, _, _) -> Bottom (List.length head)
    | Tensor_proof (head, _, _, _, _, _) -> Tensor (List.length head)
    | Par_proof (head, _, _, _, _) -> Par (List.length head)
    | With_proof (head, _, _, _, _, _) -> With (List.length head)
    | Plus_left_proof (head, _, _, _, _) -> Plus_left (List.length head)
    | Plus_right_proof (head, _, _, _, _) -> Plus_right (List.length head)
    | Promotion_proof (head_without_whynot, _, _, _) -> Promotion (List.length head_without_whynot)
    | Dereliction_proof (head, _, _, _) -> Dereliction (List.length head)
    | Weakening_proof (head, _, _, _) -> Weakening (List.length head)
    | Contraction_proof (head, _, _, _) -> Contraction (List.length head)
    | Exchange_proof (_, display_permutation, permutation, _) -> Exchange (display_permutation, permutation)
    | Cut_proof (head, formula, _, _, _) -> Cut (formula, List.length head)
    | Hypothesis_proof _ -> raise (Failure "Can not get rule request of hypothesis");;


(* JSON -> PROOF *)

exception Json_exception of string;;

let optional_field json key =
    let value =
        try Yojson.Basic.Util.member key json
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("a proof must be a json object"))
    in
    value;;

let required_field json key =
    let value = optional_field json key in
    if value = `Null
    then raise (Json_exception ("required field '" ^ key ^ "' is missing"))
    else value;;

let get_json_list json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ key ^ "' must be a list"));;

let rec from_json json =
    let sequent_as_json = required_field json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    let applied_ruled_as_json = optional_field json "appliedRule" in
    match applied_ruled_as_json with
        | `Null -> Hypothesis_proof sequent
        | _ -> let rule_request_as_json = required_field applied_ruled_as_json "ruleRequest" in
            let rule_request = Rule_request.from_json rule_request_as_json in
            let premises_as_json = get_json_list applied_ruled_as_json "premises" in
            let premises = List.map from_json premises_as_json in
            from_sequent_and_rule_request_and_premises sequent rule_request premises;;


(* PROOF -> JSON *)

let rec to_json proof =
    let sequent = get_conclusion proof in
    let sequent_as_json = Raw_sequent.sequent_to_json sequent in
    match proof with
    | Hypothesis_proof _ -> `Assoc [("sequent", sequent_as_json);
                                    ("appliedRule", `Null)]
    | _ -> let rule_request = get_rule_request proof in
        let rule_request_as_json = Rule_request.to_json rule_request in
        let premises = get_premises proof in
        let premises_as_json = List.map to_json premises in
        `Assoc [("sequent", sequent_as_json);
                ("appliedRule", `Assoc [("ruleRequest", rule_request_as_json);
                                        ("premises", `List premises_as_json)])];;


(* PROOF -> COQ *)

let coq_apply coq_rule =
    Printf.sprintf "apply %s; cbn.\n" coq_rule;;

let coq_apply_with_args coq_rule args =
    let args_as_string = (String.concat " " args) in
    Printf.sprintf "apply (%s %s); cbn.\n" coq_rule args_as_string;;

let permutation_to_coq permutation =
    Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int permutation)) ^ "%nat";;

let indent_line line =
    "  " ^ line;;

let add_indent_and_brace proof_as_coq =
    let lines = List.filter (fun s -> s <> "") (String.split_on_char '\n' proof_as_coq) in
    let indented_lines =
      match lines with
      | [] -> raise (Failure "Empty generated Coq proof")
      | first_line :: other_lines -> first_line :: List.map indent_line other_lines
    in Printf.sprintf "{ %s }\n" (String.concat "\n" indented_lines)

let rec to_coq_with_hyps_increment i = function
    | Axiom_proof _ -> "ax_expansion.\n", i, []
    | One_proof -> coq_apply "one_r_ext", i, []
    | Top_proof (head, _) -> coq_apply_with_args "top_r_ext" [formula_list_to_coq head], i, []
    | Bottom_proof (head, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "bot_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Tensor_proof (head, _, _, _, p1, p2) ->
        let s1, n1, hyps1 = to_coq_with_hyps_increment i p1 in
        let s2, n2, hyps2 = to_coq_with_hyps_increment n1 p2 in
        coq_apply_with_args "tens_r_ext" [formula_list_to_coq head] ^ add_indent_and_brace s1 ^ add_indent_and_brace s2, n2, hyps1 @ hyps2
    | Par_proof (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "parr_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | With_proof (head, _, _, _, p1, p2) ->
        let s1, n1, hyps1 = to_coq_with_hyps_increment i p1 in
        let s2, n2, hyps2 = to_coq_with_hyps_increment n1 p2 in
        coq_apply_with_args "with_r_ext" [formula_list_to_coq head] ^ add_indent_and_brace s1 ^ add_indent_and_brace s2, n2, hyps1 @ hyps2
    | Plus_left_proof (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "plus_r1_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Plus_right_proof (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "plus_r2_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Promotion_proof (head_without_whynot, e, tail_without_whynot, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "oc_r_ext" [formula_list_to_coq head_without_whynot; "(" ^ formula_to_coq e ^ ")"; formula_list_to_coq tail_without_whynot] ^ s, n, hyps
    | Dereliction_proof (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "de_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Weakening_proof (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "wk_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Contraction_proof (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "co_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Exchange_proof (sequent, _, permutation, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "ex_perm_r" [permutation_to_coq permutation; formula_list_to_coq sequent] ^ s, n, hyps
    | Cut_proof (head, cut, _, p1, p2) ->
        let s1, n1, hyps1 = to_coq_with_hyps_increment i p1 in
        let s2, n2, hyps2 = to_coq_with_hyps_increment n1 p2 in
        coq_apply_with_args "cut_r_ext" [formula_list_to_coq head; "(" ^ formula_to_coq cut ^ ")"] ^ add_indent_and_brace s1 ^ add_indent_and_brace s2, n2, hyps1 @ hyps2
    | Hypothesis_proof sequent -> coq_apply ("Hyp" ^ string_of_int i), i + 1, [Sequent.sequent_to_coq sequent];;

let to_coq_with_hyps = to_coq_with_hyps_increment 0


(* PROOF -> LATEX *)

let latex_apply latex_rule conclusion =
    Printf.sprintf "  \\%s{%s}\n" latex_rule conclusion

let rec to_latex_permute implicit_exchange permutation_opt proof =
    (* implicit_exchange is true when we don't display exchange rule.
       permutation_opt is [None] when conclusion is to display as is,
       [Some permutation] if we need to permute it before *)
    let to_latex_clear_exchange = to_latex_permute implicit_exchange None in
    let conclusion =
      let preconclusion = get_conclusion proof in
      match permutation_opt with
      | None -> sequent_to_latex preconclusion
      | Some permutation -> sequent_to_latex (permute preconclusion permutation) in
    match proof with
    | Axiom_proof _ -> latex_apply "axv" conclusion
    | One_proof -> latex_apply "onev" conclusion
    | Top_proof _ -> latex_apply "topv" conclusion
    | Bottom_proof (_, _, p) -> to_latex_clear_exchange p ^ (latex_apply "botv" conclusion)
    | Tensor_proof (_, _, _, _, p1, p2) -> to_latex_clear_exchange p1 ^ (to_latex_clear_exchange p2) ^ (latex_apply "tensorv" conclusion)
    | Par_proof (_, _, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "parrv" conclusion)
    | With_proof (_, _, _, _, p1, p2) -> to_latex_clear_exchange p1 ^ (to_latex_clear_exchange p2) ^ (latex_apply "withv" conclusion)
    | Plus_left_proof (_, _, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "pluslv" conclusion)
    | Plus_right_proof (_, _, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "plusrv" conclusion)
    | Promotion_proof (_, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "ocv" conclusion)
    | Dereliction_proof (_, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "dev" conclusion)
    | Weakening_proof (_, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "wkv" conclusion)
    | Contraction_proof (_, _, _, p) -> to_latex_clear_exchange p ^ (latex_apply "cov" conclusion)
    | Exchange_proof (_, display_permutation, permutation, p) ->
        if implicit_exchange
            then to_latex_permute implicit_exchange (Some display_permutation) p
            else to_latex_permute implicit_exchange None p ^
                if permutation = identity (List.length permutation) then ""
                else (latex_apply "exv" conclusion)
    | Cut_proof (_, _, _, p1, p2) -> to_latex_clear_exchange p1 ^ (to_latex_clear_exchange p2) ^ (latex_apply "cutv" conclusion)
    | Hypothesis_proof _ -> latex_apply "hypv" conclusion;;

let to_latex implicit_exchange =
    to_latex_permute implicit_exchange None

(* PROOF -> ASCII / UTF8 *)

(* as first step, proofs are translated as lists of ascii strings (all of the same length)
   with left_shift and right_shift giving the lateral position of conclusion
   then everything is concatenated
   and ascii can be converted to utf8 *)

type proof_text_list = int * int * string list

let rec string_repeat n s =
  match n with
  | 0 -> ""
  | k -> s ^ (string_repeat (k-1) s)

let left_shift_proof_text n =
  List.map (fun x -> String.make n ' ' ^ x)

let right_shift_proof_text n =
  List.map (fun x -> x ^ String.make n ' ')

let rec proof_text_width = function
  | [] -> 0
  | line :: _ -> String.length line

let concat_proof_text gap (left_shift1, right_shift1, proof1) (left_shift2, right_shift2, proof2) =
  let width1 = proof_text_width proof1 in
  let width2 = proof_text_width proof2 in
  let length1 = List.length proof1 in
  let length2 = List.length proof2 in
  let proof1_extended, proof2_extended = 
    if length1 > length2 then
      (proof1, List.init (length1 - length2) (fun _ -> String.make width2 ' ') @ proof2)
    else (List.init (length2 - length1) (fun _ -> String.make width1 ' ') @ proof1, proof2) in
  (left_shift1, right_shift2, List.map2 (fun line1 line2 -> line1 ^ (String.make gap ' ') ^ line2) proof1_extended proof2_extended)

let ascii_apply_hyp conclusion = (0, 0, [ conclusion ])

let ascii_apply0 rule_name conclusion =
  let width = String.length conclusion in
  let rule_name_width = String.length rule_name in
  (0, 1 + rule_name_width,
   [ Printf.sprintf "%s %s" (string_repeat width "-") rule_name;
     Printf.sprintf "%s %s" conclusion (String.make rule_name_width ' ') ])

let ascii_apply1 premise_data rule_name conclusion =
  let left_shift, right_shift, premise_text = premise_data in
  let premise_length = proof_text_width premise_text - (left_shift + right_shift) in
  let conclusion_length = String.length conclusion in
  let rule_length = max premise_length conclusion_length in
  let rule_name_width = String.length rule_name in
  let make_rule_line left right =
    Printf.sprintf "%s%s %s%s" (String.make left ' ') (string_repeat rule_length "-") rule_name (String.make right ' ') in
  let make_conclusion_line left right =
    Printf.sprintf "%s%s%s" (String.make left ' ') conclusion (String.make right ' ') in
  if premise_length > conclusion_length then
    let gap = premise_length - conclusion_length in
    let left_centering = gap / 2 in
    let right_centering = gap - left_centering in
    let right_rule_shift = right_shift - (1 + rule_name_width) in
    let right_enlarge = if right_rule_shift < 0 then - right_rule_shift else 0 in
    let rule_line = make_rule_line left_shift (right_rule_shift + right_enlarge) in
    let left_conclusion_shift = left_shift + left_centering in
    let right_conclusion_shift = right_centering + right_shift + right_enlarge in
    let conclusion_line = make_conclusion_line left_conclusion_shift right_conclusion_shift in
    (left_conclusion_shift, right_conclusion_shift,
     right_shift_proof_text right_enlarge premise_text @ [ rule_line; conclusion_line ])
  else
    let gap = conclusion_length - premise_length in
    let left_centering = gap / 2 in
    let right_centering = gap - left_centering in
    let left_rule_shift = left_shift - left_centering in
    let left_enlarge = if left_rule_shift < 0 then - left_rule_shift else 0 in
    let right_rule_shift = right_shift - (right_centering + 1 + rule_name_width) in
    let right_enlarge = if right_rule_shift < 0 then - right_rule_shift else 0 in
    let rule_line = make_rule_line (left_rule_shift + left_enlarge) (right_rule_shift + right_enlarge) in
    let left_conclusion_shift = left_rule_shift + left_enlarge in
    let right_conclusion_shift = right_rule_shift + right_enlarge + 1 + rule_name_width in
    let conclusion_line = make_conclusion_line left_conclusion_shift right_conclusion_shift in
    (left_conclusion_shift, right_conclusion_shift,
     left_shift_proof_text left_enlarge (right_shift_proof_text right_enlarge premise_text) @ [ rule_line; conclusion_line ])

let vertical_gap_ascii = 3

let rec to_ascii_list utf8 implicit_exchange permutation_opt proof =
    (* implicit_exchange is true when we don't display exchange rule.
       permutation_opt is [None] when conclusion is to display as is,
       [Some permutation] if we need to permute it before *)
    let to_ascii_list_clear_exchange = to_ascii_list utf8 implicit_exchange None in
    let conclusion =
      let preconclusion = get_conclusion proof in
      match permutation_opt with
      | None -> sequent_to_ascii utf8 preconclusion
      | Some permutation -> sequent_to_ascii utf8 (permute preconclusion permutation) in
    match proof with
    | Axiom_proof _ -> ascii_apply0 "ax" conclusion
    | One_proof -> ascii_apply0 "1" conclusion
    | Top_proof _ -> ascii_apply0 "T" conclusion
    | Bottom_proof (_, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "_" conclusion
    | Tensor_proof (_, _, _, _, p1, p2) ->
       let premise_data = concat_proof_text vertical_gap_ascii (to_ascii_list_clear_exchange p1) (to_ascii_list_clear_exchange p2) in
       ascii_apply1 premise_data "*" conclusion
    | Par_proof (_, _, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "|" conclusion
    | With_proof (_, _, _, _, p1, p2) ->
       let premise_data = concat_proof_text vertical_gap_ascii (to_ascii_list_clear_exchange p1) (to_ascii_list_clear_exchange p2) in
       ascii_apply1 premise_data "&" conclusion
    | Plus_left_proof (_, _, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "+1" conclusion
    | Plus_right_proof (_, _, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "+2" conclusion
    | Promotion_proof (_, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "!" conclusion
    | Dereliction_proof (_, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "?d" conclusion
    | Weakening_proof (_, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "?w" conclusion
    | Contraction_proof (_, _, _, p) -> ascii_apply1 (to_ascii_list_clear_exchange p) "?c" conclusion
    | Exchange_proof (_, display_permutation, permutation, p) ->
        if implicit_exchange
            then to_ascii_list utf8 implicit_exchange (Some display_permutation) p
            else let premise_data = to_ascii_list utf8 implicit_exchange None p in
                if permutation = identity (List.length permutation) then premise_data
                else ascii_apply1 premise_data "ex" conclusion
    | Cut_proof (_, _, _, p1, p2) ->
       let premise_data = concat_proof_text vertical_gap_ascii (to_ascii_list_clear_exchange p1) (to_ascii_list_clear_exchange p2) in
       ascii_apply1 premise_data "cut" conclusion
    | Hypothesis_proof _ -> ascii_apply_hyp conclusion;;

let to_ascii_utf8 utf8 implicit_exchange proof =
    let _, _, proof_text = to_ascii_list utf8 implicit_exchange None proof in
    (String.concat "\n" proof_text) ^ "\n"

let to_ascii =
    to_ascii_utf8 false

let to_utf8 implicit_exchange proof =
    Str.global_replace (Str.regexp "+") "⊕"
   (Str.global_replace (Str.regexp "*") "⊗"
   (Str.global_replace (Str.regexp "|") "⅋"
   (Str.global_replace (Str.regexp "T") "⊤"
   (Str.global_replace (Str.regexp "_") "⊥"
   (Str.global_replace (Str.regexp "-") "─"
   (Str.global_replace (Str.regexp "|-") "⊢ "
   (to_ascii_utf8 true implicit_exchange proof)))))))


(* SIMPLIFY : COMMUTE UP PERMUTATIONS *)

let rec head_tail element = function
    | [] -> raise (Failure "element not found")
    | e :: l -> if e = element then [], l else let head, tail = head_tail element l in e :: head, tail

let head_tail_perm head perm =
    let n_head = List.length head in
    head_tail n_head perm

let perm_minus_element n perm =
    List.map (fun k -> if k > n then k - 1 else k) (List.filter (fun k -> k <> n) perm)

let perm_plus_element n perm =
    List.concat (List.map (fun k -> if k = n then [n; n + 1] else if k > n then [k + 1] else [k]) perm)

let rec commute_permutations proof perm =
    let conclusion = get_conclusion proof in
    match proof with
    | Axiom_proof e -> if perm = [0; 1] then proof else Axiom_proof (dual e)
    | One_proof -> proof
    | Top_proof (head, tail) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Top_proof (permute conclusion head_perm, permute conclusion tail_perm)
    | Bottom_proof (head, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_minus_element (List.length head) perm in
        Bottom_proof (permute conclusion head_perm, permute conclusion tail_perm, commute_permutations p new_perm)
    | Tensor_proof (head, _, _, tail, p1, p2) ->
        let new_proof = set_premises proof [commute_permutations p1 (identity (List.length head + 1)); commute_permutations p2 (identity (1 + List.length tail))] in
        if perm = identity (List.length conclusion) then new_proof else Exchange_proof (conclusion, perm, perm, new_proof)
    | Par_proof (head, e1, e2, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_plus_element (List.length head) perm in
        Par_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, commute_permutations p new_perm)
    | With_proof (head, e1, e2, tail, p1, p2) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        With_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, commute_permutations p1 perm, commute_permutations p2 perm)
    | Plus_left_proof (head, e1, e2, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Plus_left_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, commute_permutations p perm)
    | Plus_right_proof (head, e1, e2, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Plus_right_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, commute_permutations p perm)
    | Promotion_proof (head_without_whynot, formula, tail_without_whynot, p) ->
        let head_perm, tail_perm = head_tail_perm head_without_whynot perm in
        let conclusion_without_whynot = head_without_whynot @ [formula] @ tail_without_whynot in
        Promotion_proof (permute conclusion_without_whynot head_perm, formula, permute conclusion_without_whynot tail_perm, commute_permutations p perm)
    | Dereliction_proof (head, formula, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Dereliction_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, commute_permutations p perm)
    | Weakening_proof (head, formula, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_minus_element (List.length head) perm in
        Weakening_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, commute_permutations p new_perm)
    | Contraction_proof (head, formula, tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_plus_element (List.length head) perm in
        Contraction_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, commute_permutations p new_perm)
    | Exchange_proof (_, _, permutation, p) -> commute_permutations p (permute permutation perm)
    | Cut_proof (head, _, tail, p1, p2) ->
        let new_proof = set_premises proof [commute_permutations p1 (identity (List.length head + 1)); commute_permutations p2 (identity (1 + List.length tail))] in
        if perm = identity (List.length conclusion) then new_proof else Exchange_proof (conclusion, perm, perm, new_proof)
    | Hypothesis_proof s -> Hypothesis_proof (permute s perm);;


(* SIMPLIFY : REMOVE LOOP *)

let rec index_list offset = function
    | [] -> []
    | e :: tail -> (e, offset) :: index_list (offset + 1) tail

let rec head_index_tail element = function
    | [] -> raise (Failure "Could not find element in list")
    | (e, i) :: l -> if e = element then [], i, l
        else let head, index, tail = head_index_tail element l in
        (e, i) :: head, index, tail

let rec get_permutation indexed_list = function
    | [] -> []
    | e :: l -> let head, i, tail = head_index_tail e indexed_list in i :: (get_permutation (head @ tail) l)

let permute_proof proof sequent_below =
    let sequent = get_conclusion proof in
    let indexed_sequent = index_list 0 sequent in
    let permutation = get_permutation indexed_sequent sequent_below in
    Exchange_proof (sequent, permutation, permutation, proof)

let rec find_shorter_proof original_sequent sorted_sequent max_size l =
    match l with
    | [] -> None
    | (original, sorted, size, p) :: tail ->
        if original = original_sequent
            then Some l
        else if sorted = sorted_sequent && size < max_size
            then Some ((original_sequent, sorted, size + 1, permute_proof p original_sequent) :: l)
        else find_shorter_proof original_sequent sorted_sequent max_size tail

let rec sum = function
  | [] -> 0
  | n :: l -> n + (sum l)

let get_first_proof proofs_of_sequent =
    let _, _, _, p = List.hd proofs_of_sequent in p

let get_size proofs_of_sequent =
    let _, _, size, _ = List.hd proofs_of_sequent in size

let rec get_proofs_of_sequents proof =
    match proof with
    | Hypothesis_proof s -> [(s, sort s, 1, proof)]
    | _ -> let proofs_by_premises = List.map get_proofs_of_sequents (get_premises proof) in
        let s = get_conclusion proof in
        let sorted = sort s in
        let size = 1 + sum (List.map get_size proofs_by_premises) in
        let proofs_of_premises = List.concat proofs_by_premises in
        match find_shorter_proof s sorted (size - 1) proofs_of_premises with
        | Some l -> l
        | None -> let p = set_premises proof (List.map get_first_proof proofs_by_premises) in
            (s, sorted, size, p) :: proofs_of_premises

let remove_loop proof =
    get_first_proof (get_proofs_of_sequents proof)


(* SIMPLIFY : COMMUTE DOWN WEAKENING *)

let rec tail_n l n =
    if n = 0 then l else tail_n (List.tl l) (n-1);;

let rec head_n l n =
    if n = 0 then [] else List.hd l :: head_n (List.tl l) (n-1);;

let new_head_wk_tail_wk_head_tail head tail head_wk tail_wk formula output_formula_length =
    if List.length head <= List.length head_wk - output_formula_length
    then
        let middle = tail_n head_wk (List.length head + output_formula_length) in
        head @ [formula] @ middle, tail_wk, head, middle @ tail_wk
    else
        let middle = head_n tail_wk (List.length tail_wk - List.length tail - output_formula_length) in
        head_wk, middle @ [formula] @ tail, head_wk @ middle, tail;;

let rec sort_weakenings_by_head_size_desc = function
    | Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))
        when List.length head_wk1 <= List.length head_wk2 ->
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        let sorted_weakening = sort_weakenings_by_head_size_desc (Weakening_proof (new_head_wk1, f1, new_tail_wk1, p)) in
        sort_weakenings_by_head_size_desc (Weakening_proof (new_head_wk2, f2, new_tail_wk2, sorted_weakening))
    | p -> p;;

let rec get_weakening_head_size_list = function
    | Weakening_proof (head_wk, _, _, p) -> List.length head_wk :: get_weakening_head_size_list p
    | _ -> [];;

let rec get_first_common_position_with_exception l1 l2 n = match l1, l2 with
    | [], l2 -> None
    | l1, [] -> None
    | e1 :: tail1, e2 :: tail2 -> if e1 = e2 && e1 <> n then Some e1
        else if e1 = n || e1 > e2
        then get_first_common_position_with_exception tail1 l2 n
        else get_first_common_position_with_exception l1 tail2 n;;

let rec get_weakening_with_head_size n_head = function
    | Weakening_proof (head_wk, f, tail_wk, p)
        when List.length head_wk = n_head ->
        Weakening_proof (head_wk, f, tail_wk, p)
    | Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))
        when List.length head_wk2 = n_head ->
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        Weakening_proof (new_head_wk2, f2, new_tail_wk2, Weakening_proof (new_head_wk1, f1, new_tail_wk1, p))
    | Weakening_proof (head_wk, f, tail_wk, p) ->
        get_weakening_with_head_size n_head (Weakening_proof (head_wk, f, tail_wk, get_weakening_with_head_size n_head p))
    | _ -> raise (Failure "Weakening not found");;

let get_commuted_proof = function
    | _, proof -> proof;;

let has_commuted = function
    | b, _ -> b;;

let rec rec_commute_down_weakenings proof =
    match proof with
    | Axiom_proof _ -> false, proof
    | One_proof -> false, proof
    | Top_proof _ -> false, proof
    | Bottom_proof (head, tail, Weakening_proof (head_wk, formula, tail_wk, p)) ->
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk tail_wk Sequent.Bottom 0 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Bottom_proof (new_head, new_tail, p))) in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Tensor_proof (head, e1, e2, tail, Weakening_proof (head_wk, formula, e :: tail_wk, p), p2) ->
        (* Last element of tail_wk should be equal to e1. If e1 is weakened just after, we can not commute down weakening (cf next case).*)
        let new_tail_wk = head_n (e :: tail_wk) (List.length tail_wk) in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Tensor_proof (head_wk @ new_tail_wk, e1, e2, tail, p, p2))) in
        true, Weakening_proof (head_wk, formula, new_tail_wk @ [Sequent.Tensor (e1,e2)] @ tail, new_proof)
    | Tensor_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, [], Weakening_proof (head_wk2, f2, tail_wk2, p)), p2) ->
        (* e1 should be equal to Whynot f1. We swap the two weakenings. *)
        let swapped_weakenings = Weakening_proof (head_wk2, f2, tail_wk2 @ [e1], Weakening_proof (head_wk2 @ tail_wk2, f1, [], p)) in
        rec_commute_down_weakenings (Tensor_proof (head, e1, e2, tail, swapped_weakenings, p2))
    | Tensor_proof (head, e1, e2, tail, p1, Weakening_proof (e :: head_wk, formula, tail_wk, p)) ->
        (* e should be equal to e2. If e2 is weakened just after, we can not commute down weakening (cf next case).*)
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Tensor_proof (head, e1, e2, head_wk @ tail_wk, p1, p))) in
        true, Weakening_proof (head @ [Sequent.Tensor (e1,e2)] @ head_wk, formula, tail_wk, new_proof)
    | Tensor_proof (head, e1, e2, tail, p1, Weakening_proof ([], f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))) ->
        (* e2 should be equal to Whynot f1. We swap the two weakenings. *)
        let swapped_weakenings = Weakening_proof ([e2] @ head_wk2, f2, tail_wk2, Weakening_proof ([], f1, head_wk2 @ tail_wk2, p)) in
        rec_commute_down_weakenings (Tensor_proof (head, e1, e2, tail, p1, swapped_weakenings))
    | Par_proof (head, e1, e2, tail, Weakening_proof (head_wk, formula, tail_wk, p))
        when List.length head_wk <> List.length head && List.length head_wk <> List.length head + 1 ->
        (* If one of par formulas is weakened just after: we can not commute down weakening (cf next cases). *)
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Par (e1, e2)) 2 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Par_proof (new_head, e1, e2, new_tail, p))) in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Par_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p)))
        when List.length head_wk2 <> List.length head ->
        (* If first weaken after par is on e1 or e2 and if second weaken is not on the other one,
            then we can commute second weaken with {par + first weaken}. *)
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        let second_weakening = Weakening_proof (new_head_wk1, f1, new_tail_wk1, p) in
        let new_head_wk2, new_tail_wk2, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail new_head_wk2 new_tail_wk2 (Par (e1, e2)) 2 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Par_proof (new_head, e1, e2, new_tail, second_weakening))) in
        true, Weakening_proof (new_head_wk2, f2, new_tail_wk2, new_proof)
    | Par_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, Weakening_proof (head_wk3, f3, tail_wk3, p)))) ->
        (* If first weaken after par is on e1 or e2 and second weaken is on the other one (cf cases before).
            We can commute third weaken with {par + first weaken + second weaken}. *)
        let new_head_wk3, new_tail_wk3, new_head_wk2, new_tail_wk2 =
            new_head_wk_tail_wk_head_tail head_wk2 tail_wk2 head_wk3 tail_wk3 (Whynot f2) 0 in
        let third_weakening = Weakening_proof (new_head_wk2, f2, new_tail_wk2, p) in
        let new_head_wk3, new_tail_wk3, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 new_head_wk3 new_tail_wk3 (Whynot f1) 0 in
        let second_weakening = Weakening_proof (new_head_wk1, f1, new_tail_wk1, third_weakening) in
        let new_head_wk3, new_tail_wk3, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail new_head_wk3 new_tail_wk3 (Par (e1, e2)) 2 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Par_proof (new_head, e1, e2, new_tail, second_weakening))) in
        true, Weakening_proof (new_head_wk3, f3, new_tail_wk3, new_proof)
    | With_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, p1), Weakening_proof (head_wk2, f2, tail_wk2, p2))
        when List.length head_wk1 = List.length head_wk2 && List.length head_wk1 <> List.length head ->
        (* f1 and f2 should be equal, we can commute down both weakenings below With_proof *)
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk1 tail_wk1 (With (e1, e2)) 1 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (With_proof (new_head, e1, e2, new_tail, p1, p2))) in
        true, Weakening_proof (new_head_wk, f1, new_tail_wk, new_proof)
    | With_proof (head, e1, e2, tail, p1, p2) -> begin
        (* p1 and p2 does not starts with same weakening, we look for a common swappable weakening on both side *)
        let sorted_p1 = sort_weakenings_by_head_size_desc (get_commuted_proof (rec_commute_down_weakenings p1)) in
        let sorted_p2 = sort_weakenings_by_head_size_desc (get_commuted_proof (rec_commute_down_weakenings p2)) in
        let l1 = get_weakening_head_size_list sorted_p1 in
        let l2 = get_weakening_head_size_list sorted_p2 in
        match get_first_common_position_with_exception l1 l2 (List.length head) with
            | None -> false, With_proof (head, e1, e2, tail, sorted_p1, sorted_p2)
            | Some n -> rec_commute_down_weakenings (With_proof (head, e1, e2, tail, get_weakening_with_head_size n sorted_p1, get_weakening_with_head_size n sorted_p2)) end
    | Plus_left_proof (head, e1, e2, tail, Weakening_proof (head_wk, formula, tail_wk, p))
        when List.length head_wk <> List.length head ->
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Plus (e1, e2)) 1 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Plus_left_proof (new_head, e1, e2, new_tail, p))) in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Plus_left_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))) ->
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        let second_weakening = Weakening_proof (new_head_wk1, f1, new_tail_wk1, p) in
        let new_head_wk2, new_tail_wk2, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail new_head_wk2 new_tail_wk2 (Plus (e1, e2)) 1 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Plus_left_proof (new_head, e1, e2, new_tail, second_weakening))) in
        true, Weakening_proof (new_head_wk2, f2, new_tail_wk2, new_proof)
    | Plus_right_proof (head, e1, e2, tail, Weakening_proof (head_wk, formula, tail_wk, p))
        when List.length head_wk <> List.length head ->
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Plus (e1, e2)) 1 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Plus_right_proof (new_head, e1, e2, new_tail, p))) in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Plus_right_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))) ->
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        let second_weakening = Weakening_proof (new_head_wk1, f1, new_tail_wk1, p) in
        let new_head_wk2, new_tail_wk2, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail new_head_wk2 new_tail_wk2 (Plus (e1, e2)) 1 in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Plus_right_proof (new_head, e1, e2, new_tail, second_weakening))) in
        true, Weakening_proof (new_head_wk2, f2, new_tail_wk2, new_proof)
    | Promotion_proof (head_without_whynot, e, tail_without_whynot, Weakening_proof (head_wk, formula, tail_wk, p))
        when List.length head_without_whynot <> List.length head_wk ->
        let head = Sequent.add_whynot head_without_whynot in
        let tail = Sequent.add_whynot tail_without_whynot in
        let new_head_wk, new_tail_wk, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Ofcourse e) 1 in
        let new_head_without_whynot = remove_whynot new_head in
        let new_tail_without_whynot = remove_whynot new_tail in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Promotion_proof (new_head_without_whynot, e, new_tail_without_whynot, p))) in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Promotion_proof (head_without_whynot, e, tail_without_whynot, Weakening_proof (head_wk1, f1, tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))) ->
        let new_head_wk2, new_tail_wk2, new_head_wk1, new_tail_wk1 =
            new_head_wk_tail_wk_head_tail head_wk1 tail_wk1 head_wk2 tail_wk2 (Whynot f1) 0 in
        let second_weakening = Weakening_proof (new_head_wk1, f1, new_tail_wk1, p) in
        let head = Sequent.add_whynot head_without_whynot in
        let tail = Sequent.add_whynot tail_without_whynot in
        let new_head_wk2, new_tail_wk2, new_head, new_tail =
            new_head_wk_tail_wk_head_tail head tail new_head_wk2 new_tail_wk2 (Ofcourse e) 1 in
        let new_head_without_whynot = remove_whynot new_head in
        let new_tail_without_whynot = remove_whynot new_tail in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Promotion_proof (new_head_without_whynot, e, new_tail_without_whynot, second_weakening))) in
        true, Weakening_proof (new_head_wk2, f2, new_tail_wk2, new_proof)
    | Dereliction_proof (head, e, tail, Weakening_proof (head_wk, formula, tail_wk, p)) ->
        if List.length head = List.length head_wk
        then
            (* derelected formula is weakened just after: we can weaken immediately *)
            let new_proof = get_commuted_proof (rec_commute_down_weakenings p) in
            true, Weakening_proof (head_wk, e, tail_wk, new_proof)
        else
            let new_head_wk, new_tail_wk, new_head, new_tail =
                new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Whynot e) 1 in
            let new_proof = get_commuted_proof (rec_commute_down_weakenings (Dereliction_proof (new_head, e, new_tail, p))) in
            true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Contraction_proof (head, e, tail, Weakening_proof (head_wk, formula, tail_wk, p)) ->
        if List.length head = List.length head_wk || List.length head + 1 = List.length head_wk
        then
            (* contracted formula is weakened just after: we can ignore the two operations *)
            true, get_commuted_proof (rec_commute_down_weakenings p)
        else
            let new_head_wk, new_tail_wk, new_head, new_tail =
                new_head_wk_tail_wk_head_tail head tail head_wk tail_wk (Whynot e) 2 in
            let new_proof = get_commuted_proof (rec_commute_down_weakenings (Contraction_proof (new_head, e, new_tail, p))) in
            true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | Exchange_proof (s, display_permutation, permutation, Weakening_proof (head_wk, formula, tail_wk, p)) ->
        let n_head_wk = List.length head_wk in
        let n_tail_wk = List.length tail_wk in
        let new_display_permutation = perm_minus_element n_head_wk display_permutation in
        let new_permutation = perm_minus_element n_head_wk permutation in
        let exchange_proof = if new_permutation = identity (n_head_wk + n_tail_wk) then p else Exchange_proof (head_wk @ tail_wk, new_display_permutation, new_permutation, p) in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings exchange_proof) in
        let conclusion = get_conclusion proof in
        let new_head_wk, _, new_tail_wk = head_formula_tail (position_in_list (List.length head_wk) permutation) conclusion in
        true, Weakening_proof (new_head_wk, formula, new_tail_wk, new_proof)
    | _ -> let commuted_premises = List.map rec_commute_down_weakenings (get_premises proof) in
        let new_proof = set_premises proof (List.map get_commuted_proof commuted_premises) in
        if List.exists has_commuted commuted_premises
        then rec_commute_down_weakenings new_proof
        else false, new_proof;;

let commute_down_weakenings proof =
    get_commuted_proof (rec_commute_down_weakenings proof);;
