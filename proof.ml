open Sequent
open Rule_request

(* PERMUTATIONS *)

let is_valid_permutation l =
    let sorted_l = List.sort Int.compare l in
    let identity = List.init (List.length l) (fun n -> n) in
    sorted_l = identity;;

let permute l =
    List.map (List.nth l);;

let rec position_in_list a = function
    | [] -> raise (Failure "Not found")
    | h :: tl -> if a = h then 0 else 1 + position_in_list a tl

let permutation_inverse perm =
    let identity = List.init (List.length perm) (fun n -> n) in
    List.map (fun x -> position_in_list x perm) identity


(* PROOF *)

type proof =
    | Axiom_left of formula
    | Axiom_right of formula
    | One
    | Top of formula list * formula list
    | Bottom of formula list * formula list * proof
    | Tensor of formula list * formula * formula * formula list * proof * proof
    | Par of formula list * formula * formula * formula list * proof
    | With of formula list * formula * formula * formula list * proof * proof
    | Plus_left of formula list * formula * formula * formula list * proof
    | Plus_right of formula list * formula * formula * formula list * proof
    | Promotion of formula list * formula * formula list * proof
    | Dereliction of formula list * formula * formula list * proof
    | Weakening of formula list * formula * formula list * proof
    | Contraction of formula list * formula * formula list * proof
    | Exchange of sequent * int list * proof
    | Hypothesis of sequent;;


(* GETTERS & SETTERS *)

let get_premises = function
    | Axiom_left _ -> []
    | Axiom_right _ -> []
    | One -> []
    | Top (_, _) -> []
    | Bottom (_, _, p) -> [p]
    | Tensor (_, _, _, _, p1, p2) -> [p1; p2]
    | Par (_, _, _, _, p) -> [p]
    | With (_, _, _, _, p1, p2) -> [p1; p2]
    | Plus_left (_, _, _, _, p) -> [p]
    | Plus_right (_, _, _, _, p) -> [p]
    | Promotion (_, _, _, p) -> [p]
    | Dereliction (_, _, _, p) -> [p]
    | Weakening (_, _, _, p) -> [p]
    | Contraction (_, _, _, p) -> [p]
    | Exchange (_, _, p) -> [p]
    | Hypothesis s -> raise (Failure "Can not get premises of hypothesis");;

let set_premises proof premises = match proof, premises with
    | Axiom_left _, [] -> proof
    | Axiom_right _, [] -> proof
    | One, [] -> proof
    | Top (_, _), [] -> proof
    | Bottom (head, tail, _), [p] -> Bottom (head, tail, p)
    | Tensor (head, e1, e2, tail, _, _), [p1; p2] -> Tensor (head, e1, e2, tail, p1, p2)
    | Par (head, e1, e2, tail, _), [p]  -> Par (head, e1, e2, tail, p)
    | With (head, e1, e2, tail, _, _), [p1; p2] -> With (head, e1, e2, tail, p1, p2)
    | Plus_left (head, e1, e2, tail, _), [p] -> Plus_left (head, e1, e2, tail, p)
    | Plus_right (head, e1, e2, tail, _), [p] -> Plus_right (head, e1, e2, tail, p)
    | Promotion (head_without_whynot, e, tail_without_whynot, _), [p] ->
        Promotion (head_without_whynot, e, tail_without_whynot, p)
    | Dereliction (head, e, tail, _), [p] -> Dereliction (head, e, tail, p)
    | Weakening (head, e, tail, _), [p] -> Weakening (head, e, tail, p)
    | Contraction (head, e, tail, _), [p] -> Contraction (head, e, tail, p)
    | Exchange (sequent, permutation, _), [p] -> Exchange (sequent, permutation, p)
    | Hypothesis sequent, _ -> raise (Failure "Can not set premises of hypothesis")
    | _ -> raise (Failure "Number of premises mismatch with given proof");;

let get_conclusion = function
    | Axiom_left e -> {hyp=[]; cons=[e; orthogonal e]}
    | Axiom_right e -> {hyp=[]; cons=[orthogonal e; e]}
    | One -> {hyp=[]; cons=[Sequent.One]}
    | Top (head, tail) -> {hyp=[]; cons=head @ [Sequent.Top] @ tail}
    | Bottom (head, tail, _) -> {hyp=[]; cons=head @ [Sequent.Bottom] @ tail}
    | Tensor (head, e1, e2, tail, _, _) -> {hyp=[]; cons=head @ [Sequent.Tensor (e1, e2)] @ tail}
    | Par (head, e1, e2, tail, _) -> {hyp=[]; cons=head @ [Sequent.Par (e1, e2)] @ tail}
    | With (head, e1, e2, tail, _, _) -> {hyp=[]; cons=head @ [Sequent.With (e1, e2)] @ tail}
    | Plus_left (head, e1, e2, tail, _) -> {hyp=[]; cons=head @ [Plus (e1, e2)] @ tail}
    | Plus_right (head, e1, e2, tail, _) -> {hyp=[]; cons=head @ [Plus (e1, e2)] @ tail}
    | Promotion (head_without_whynot, e, tail_without_whynot, _) ->
        let head = Sequent.add_whynot head_without_whynot in
        let tail = Sequent.add_whynot tail_without_whynot in
        {hyp=[]; cons=head @ [Ofcourse e] @ tail}
    | Dereliction (head, e, tail, _) -> {hyp=[]; cons=head @ [Whynot e] @ tail}
    | Weakening (head, e, tail, _) -> {hyp=[]; cons=head @ [Whynot e] @ tail}
    | Contraction (head, e, tail, _) -> {hyp=[]; cons=head @ [Whynot e] @ tail}
    | Exchange (sequent, permutation, _) -> {hyp=[]; cons=permute sequent.cons permutation}
    | Hypothesis sequent -> sequent;;


(* SEQUENT & RULE_REQUEST -> PROOF *)

exception Technical_exception of string;;
exception Pedagogic_exception of string;;

let rec head_formula_tail n = function
    | [] -> raise (Technical_exception "Argument formula_positions[0] is greater than the number of given formulas")
    | f :: formula_list -> if n = 0 then [], f, formula_list
        else let head, formula, tail = head_formula_tail (n - 1) formula_list
        in f::head, formula, tail;;

let from_sequent_and_rule_request sequent rule_request =
    match rule_request with
        Axiom -> (
            match sequent.cons with
            | e1 :: e2 :: [] -> if orthogonal e1 <> e2
                then raise (Pedagogic_exception ("Can not apply 'axiom' rule: the two formulas are not orthogonal."))
                else (match e2 with
                | Orth e -> Axiom_left e
                | _ -> Axiom_right e2)
            | _ -> raise (Pedagogic_exception ("Can not apply 'axiom' rule: the sequent must contain exactly two formulas."))
        )
        | One -> (
            match sequent.cons with
            | One :: [] -> One
            | _ -> raise (Pedagogic_exception ("Can not apply 'one' rule: the sequent must be reduced to the single formula '1'."))
        )
        | Bottom n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            | Bottom -> Bottom (head, tail, (Hypothesis {hyp=[]; cons=(head @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply bottom rule on this formula"))
        )
        | Top n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            | Top -> Top (head, tail)
            | _ -> raise (Technical_exception ("Cannot apply top rule on this formula"))
        )
        | Zero n -> raise (Pedagogic_exception ("Can not apply 'zero' rule: there is no rule for introducing '0'."))
        | Tensor n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Tensor (e1, e2) -> Tensor (head, e1, e2, tail, (Hypothesis {hyp=[]; cons=(head @ [e1])}), (Hypothesis {hyp=[]; cons=([e2] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply tensor rule on this formula"))
        )
        | Par n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Par (e1, e2) -> Par (head, e1, e2, tail, (Hypothesis {hyp=[]; cons=(head @ [e1; e2] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply par rule on this formula"))
        )
        | With n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            With (e1, e2) -> With (head, e1, e2, tail, (Hypothesis {hyp=[]; cons=(head @ [e1] @ tail)}), (Hypothesis {hyp=[]; cons=(head @ [e2] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply with rule on this formula"))
        )
        | Plus_left n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Plus (e1, e2) -> Plus_left (head, e1, e2, tail, (Hypothesis {hyp=[]; cons=(head @ [e1] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply plus_left rule on this formula"))
        )
        | Plus_right n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Plus (e1, e2) -> Plus_right (head, e1, e2, tail, (Hypothesis {hyp=[]; cons=(head @ [e2] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply plus_right rule on this formula"))
        )
        | Promotion n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Ofcourse e -> (try
                let head_without_whynot = Sequent.remove_whynot head in
                let tail_without_whynot = remove_whynot tail in
                Promotion (head_without_whynot, e, tail_without_whynot, (Hypothesis {hyp=[]; cons=(head @ [e] @ tail)}))
                with Sequent.Not_whynot -> raise (Pedagogic_exception ("Can not apply 'promotion' rule: the context must contain formulas starting by '?' only.")))
            | _ -> raise (Technical_exception ("Cannot apply promotion rule on this formula"))
        )
        | Dereliction n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Whynot e -> Dereliction (head, e, tail, (Hypothesis {hyp=[]; cons=(head @ [e] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply dereliction rule on this formula"))
        )
        | Weakening n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Whynot e -> Weakening (head, e, tail, (Hypothesis {hyp=[]; cons=(head @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply weakening rule on this formula"))
        )
        | Contraction n -> (
            let head, formula, tail = head_formula_tail n sequent.cons in
            match formula with
            Whynot e -> Contraction (head, e, tail, (Hypothesis {hyp=[]; cons=(head @ [Whynot e; Whynot e] @ tail)}))
            | _ -> raise (Technical_exception ("Cannot apply contraction rule on this formula"))
        )
        | Exchange permutation -> (
            if List.length sequent.cons <> List.length permutation
            then raise (Technical_exception ("When applying exchange rule, formula_positions and sequent must have same size"))
            else if not (is_valid_permutation permutation)
            then raise (Technical_exception ("When applying exchange rule, formula_positions should be a permutation of the size of sequent formula list"))
            else let permuted_sequent = {hyp=[]; cons=permute sequent.cons permutation} in
            Exchange (permuted_sequent, permutation_inverse permutation, (Hypothesis permuted_sequent))
        );;

let from_sequent_and_rule_request_and_premises sequent rule_request premises =
    let proof = from_sequent_and_rule_request sequent rule_request in
    let expected_premises_conclusion = List.map get_conclusion (get_premises proof) in
    let given_premises_conclusion = List.map get_conclusion premises in
    if expected_premises_conclusion <> given_premises_conclusion then
    raise (Technical_exception ("Premises conclusion do not match expected premises conclusion after applying rule"))
    else set_premises proof premises;;


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
    let sequent_as_json = required_field json "sequentAsJson" in
    let sequent = Sequent.from_json sequent_as_json in
    let applied_ruled_as_json = optional_field json "appliedRule" in
    match applied_ruled_as_json with
        | `Null -> Hypothesis sequent
        | _ -> let rule_request_as_json = required_field applied_ruled_as_json "ruleRequest" in
            let rule_request = Rule_request.from_json rule_request_as_json in
            let premises_as_json = get_json_list applied_ruled_as_json "premises" in
            let premises = List.map from_json premises_as_json in
            from_sequent_and_rule_request_and_premises sequent rule_request premises;;


(* OPERATIONS *)

let rec is_complete = function
    | Hypothesis s -> false
    | proof -> let premises = get_premises proof in
        List.for_all is_complete premises;;


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
    | Axiom_left _ -> coq_apply "ax_r2_ext", i, []
    | Axiom_right f -> coq_apply_with_args "ax_r1_ext" ["(" ^ formula_to_coq f ^ ")"], i, []
    | One -> coq_apply "one_r_ext", i, []
    | Top (head, _) -> coq_apply_with_args "top_r_ext" [formula_list_to_coq head], i, []
    | Bottom (head, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "bot_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Tensor (head, _, _, _, p1, p2) -> 
        let s1, n1, hyps1 = to_coq_with_hyps_increment i p1 in
        let s2, n2, hyps2 = to_coq_with_hyps_increment n1 p2 in
        coq_apply_with_args "tens_r_ext" [formula_list_to_coq head] ^ add_indent_and_brace s1 ^ add_indent_and_brace s2, n2, hyps1 @ hyps2
    | Par (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "parr_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | With (head, _, _, _, p1, p2) ->
        let s1, n1, hyps1 = to_coq_with_hyps_increment i p1 in
        let s2, n2, hyps2 = to_coq_with_hyps_increment n1 p2 in
        coq_apply_with_args "with_r_ext" [formula_list_to_coq head] ^ add_indent_and_brace s1 ^ add_indent_and_brace s2, n2, hyps1 @ hyps2
    | Plus_left (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "plus_r1_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Plus_right (head, _, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "plus_r2_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Promotion (head_without_whynot, e, tail_without_whynot, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "oc_r_ext" [formula_list_to_coq head_without_whynot; "(" ^ formula_to_coq e ^ ")"; formula_list_to_coq tail_without_whynot] ^ s, n, hyps
    | Dereliction (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "de_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Weakening (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "wk_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Contraction (head, _, _, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "co_r_ext" [formula_list_to_coq head] ^ s, n, hyps
    | Exchange (sequent, permutation, p) ->
        let s, n, hyps = to_coq_with_hyps_increment i p in
        coq_apply_with_args "ex_perm_r" [permutation_to_coq permutation; formula_list_to_coq sequent.cons] ^ s, n, hyps
    | Hypothesis sequent -> coq_apply ("Hyp" ^ string_of_int i), i + 1, [Sequent.sequent_to_coq sequent];;

let to_coq_with_hyps = to_coq_with_hyps_increment 0
