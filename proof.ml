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
    | Exchange_proof of sequent * int list * proof
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
    | Exchange_proof (_, _, p) -> [p]
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
    | Exchange_proof (sequent, permutation, _), [p] -> Exchange_proof (sequent, permutation, p)
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
    | Exchange_proof (sequent, permutation, _) -> permute sequent permutation
    | Hypothesis_proof sequent -> sequent;;


(* SEQUENT & RULE_REQUEST -> PROOF *)

exception Rule_exception of bool * string;;

let rec head_formula_tail n = function
    | [] -> raise (Rule_exception (false, "Argument formula_positions[0] is greater than the number of given formulas"))
    | f :: formula_list -> if n = 0 then [], f, formula_list
        else let head, formula, tail = head_formula_tail (n - 1) formula_list
        in f::head, formula, tail;;

let from_sequent_and_rule_request sequent rule_request =
    match rule_request with
        | Axiom -> (
            match sequent with
            | e1 :: e2 :: [] -> (if dual e1 <> e2
                then raise (Rule_exception (true, "Can not apply 'axiom' rule: the two formulas are not orthogonal."))
                else Axiom_proof e1)
            | _ -> raise (Rule_exception (true, "Can not apply 'axiom' rule: the sequent must contain exactly two formulas."))
        )
        | One -> (
            match sequent with
            | One :: [] -> One_proof
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
        | Exchange permutation -> (
            if List.length sequent <> List.length permutation
            then raise (Rule_exception (false, "When applying exchange rule, formula_positions and sequent must have same size"))
            else if not (is_valid_permutation permutation)
            then raise (Rule_exception (false, "When applying exchange rule, formula_positions should be a permutation of the size of sequent formula list"))
            else let permuted_sequent = permute sequent permutation in
            Exchange_proof (permuted_sequent, permutation_inverse permutation, (Hypothesis_proof permuted_sequent))
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

let is_atomic_zero_selection_sequent sequent = 
  let n = get_formula_position is_whynot sequent in
  let head, formula, tail = head_formula_tail n sequent in
  List.for_all is_atomic_or_zero (head @ tail) &&
  (not (List.mem formula sequent))

let is_reversible_selection_sequent = function
  | [f1; f2; Whynot _] when dual f1 = f2 -> false
  | [f1; Whynot _; f2] when dual f1 = f2 -> false
  | [Whynot _; f1; f2] when dual f1 = f2 -> false
  | s -> is_atomic_zero_selection_sequent s

let try_rule_selection sequent n =
  let head, formula, tail = head_formula_tail n sequent in
  match formula with
    Whynot e -> Contraction_proof (head, e, tail,
                  (Dereliction_proof (head, e, Whynot e :: tail,
                     (Hypothesis_proof (head @ [e;  Whynot e] @ tail)))))
  | _ -> raise NotApplicable

(* selection tells if selection rule already applied *)
let apply_reversible_rule selection proof =
    let sequent = get_conclusion proof in
    try try_rule_request sequent (Top (get_formula_position is_top sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent (Bottom (get_formula_position is_bottom sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent (Par (get_formula_position is_par sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent (Dereliction (get_formula_position is_double_whynot sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent (With (get_formula_position is_with sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent (Promotion (get_formula_position is_ofcourse sequent)), selection
        with NotApplicable ->
    try try_rule_request sequent One, selection
        with NotApplicable ->
    try if List.length sequent = 1 then try_rule_request sequent (Tensor 0), selection
        else raise NotApplicable
        with NotApplicable ->
    try try_rule_request sequent Axiom, selection
        with NotApplicable ->
    try if not selection && is_reversible_selection_sequent sequent
        then try_rule_selection sequent (get_formula_position is_whynot sequent), true
        else raise NotApplicable
        with NotApplicable ->
    proof, selection;;

let rec rec_apply_reversible_rule selection proof =
    let new_proof, new_selection = apply_reversible_rule selection proof in
    match new_proof with
        | Hypothesis_proof _ -> new_proof
        | _ -> let premises = get_premises new_proof in
            let new_premises = List.map (rec_apply_reversible_rule new_selection) premises in
            set_premises new_proof new_premises;;

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
    | Exchange_proof (_, permutation, _) -> Exchange (permutation_inverse permutation)
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
    Printf.sprintf "apply %s.\n" coq_rule;;

let coq_apply_with_args coq_rule args =
    let args_as_string = (String.concat " " args) in
    Printf.sprintf "apply (%s %s).\n" coq_rule args_as_string;;

let permutation_to_coq permutation =
    Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int permutation));;

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
    | Exchange_proof (sequent, permutation, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "ex_perm_r" [permutation_to_coq permutation; formula_list_to_coq sequent] ^ s, n, hyps
    | Hypothesis_proof sequent -> coq_apply ("Hyp" ^ string_of_int i), i + 1, [Sequent.sequent_to_coq sequent];;

let to_coq_with_hyps = to_coq_with_hyps_increment 0


(* PROOF -> LATEX *)

let latex_apply latex_rule conclusion =
    Printf.sprintf "  \\%s{%s}\n" latex_rule conclusion

let rec to_latex proof =
    let conclusion = sequent_to_latex (get_conclusion proof) in
    match proof with
    | Axiom_proof _ -> latex_apply "axv" conclusion
    | One_proof -> latex_apply "onev" conclusion
    | Top_proof _ -> latex_apply "topv" conclusion
    | Bottom_proof (_, _, p) -> to_latex p ^ (latex_apply "botv" conclusion)
    | Tensor_proof (_, _, _, _, p1, p2) -> to_latex p1 ^ (to_latex p2) ^ (latex_apply "tensorv" conclusion)
    | Par_proof (_, _, _, _, p) -> to_latex p ^ (latex_apply "parrv" conclusion)
    | With_proof (_, _, _, _, p1, p2) -> to_latex p1 ^ (to_latex p2) ^ (latex_apply "withv" conclusion)
    | Plus_left_proof (_, _, _, _, p) -> to_latex p ^ (latex_apply "pluslv" conclusion)
    | Plus_right_proof (_, _, _, _, p) -> to_latex p ^ (latex_apply "plusrv" conclusion)
    | Promotion_proof (_, _, _, p) -> to_latex p ^ (latex_apply "ocv" conclusion)
    | Dereliction_proof (_, _, _, p) -> to_latex p ^ (latex_apply "dev" conclusion)
    | Weakening_proof (_, _, _, p) -> to_latex p ^ (latex_apply "wkv" conclusion)
    | Contraction_proof (_, _, _, p) -> to_latex p ^ (latex_apply "cov" conclusion)
    | Exchange_proof (_, _, p) -> to_latex p ^ (latex_apply "exv" conclusion)
    | Hypothesis_proof _ -> latex_apply "hypv" conclusion;;


(* SIMPLIFY *)

let rec commute_permutations proof current_permutation =
    let n = List.length (get_conclusion proof) in
    let perm = if current_permutation = [] then identity n else current_permutation in
    let is_identity = perm = identity n in
    match proof with
    | Axiom_proof _ when is_identity -> proof
    | Axiom_proof e -> Axiom_proof (dual e)
    | One_proof -> proof
    | Top_proof (_, _) when is_identity -> proof
    | Top_proof (_, _) -> Exchange_proof (get_conclusion proof, perm, proof)
    | Bottom_proof (_, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Tensor_proof (head, _, _, tail, p1, p2) ->
        let new_proof = set_premises proof [commute_permutations p1 []; commute_permutations p2 []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Par_proof (_, _, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | With_proof (_, _, _, _, p1, p2) ->
        let new_proof = set_premises proof [commute_permutations p1 []; commute_permutations p2 []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Plus_left_proof (_, _, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Plus_right_proof (_, _, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Promotion_proof (_, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Dereliction_proof (_, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Weakening_proof (_, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Contraction_proof (_, _, _, p) ->
        let new_proof = set_premises proof [commute_permutations p []] in
        if is_identity then new_proof else Exchange_proof (get_conclusion proof, perm, new_proof)
    | Exchange_proof (_, permutation, p) -> commute_permutations p (permute permutation perm)
    | Hypothesis_proof s -> Hypothesis_proof (permute s perm);;
