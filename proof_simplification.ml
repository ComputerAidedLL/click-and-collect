open Sequent
open Proof
open Permutations

(* SIMPLIFY : COMMUTE UP PERMUTATIONS *)

let rec rec_commute_up_permutations proof perm =
    let conclusion = get_conclusion proof in
    match proof with
    | Axiom_proof e -> if perm = [0; 1] then proof else Axiom_proof (dual e)
    | One_proof -> proof
    | Top_proof (head, _tail) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Top_proof (permute conclusion head_perm, permute conclusion tail_perm)
    | Bottom_proof (head, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_minus_element (List.length head) perm in
        Bottom_proof (permute conclusion head_perm, permute conclusion tail_perm, rec_commute_up_permutations p new_perm)
    | Tensor_proof (head, _, _, tail, p1, p2) ->
        let new_proof = set_premises proof [rec_commute_up_permutations p1 (identity (List.length head + 1)); rec_commute_up_permutations p2 (identity (1 + List.length tail))] in
        if perm = identity (List.length conclusion) then new_proof else Exchange_proof (conclusion, perm, perm, new_proof)
    | Par_proof (head, e1, e2, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_plus_element (List.length head) perm in
        Par_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, rec_commute_up_permutations p new_perm)
    | With_proof (head, e1, e2, _tail, p1, p2) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        With_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, rec_commute_up_permutations p1 perm, rec_commute_up_permutations p2 perm)
    | Plus_left_proof (head, e1, e2, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Plus_left_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, rec_commute_up_permutations p perm)
    | Plus_right_proof (head, e1, e2, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Plus_right_proof (permute conclusion head_perm, e1, e2, permute conclusion tail_perm, rec_commute_up_permutations p perm)
    | Promotion_proof (head_without_whynot, formula, tail_without_whynot, p) ->
        let head_perm, tail_perm = head_tail_perm head_without_whynot perm in
        let conclusion_without_whynot = head_without_whynot @ [formula] @ tail_without_whynot in
        Promotion_proof (permute conclusion_without_whynot head_perm, formula, permute conclusion_without_whynot tail_perm, rec_commute_up_permutations p perm)
    | Dereliction_proof (head, formula, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        Dereliction_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, rec_commute_up_permutations p perm)
    | Weakening_proof (head, formula, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_minus_element (List.length head) perm in
        Weakening_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, rec_commute_up_permutations p new_perm)
    | Contraction_proof (head, formula, _tail, p) ->
        let head_perm, tail_perm = head_tail_perm head perm in
        let new_perm = perm_plus_element (List.length head) perm in
        Contraction_proof (permute conclusion head_perm, formula, permute conclusion tail_perm, rec_commute_up_permutations p new_perm)
    | Exchange_proof (_, _, permutation, p) -> rec_commute_up_permutations p (permute permutation perm)
    | Cut_proof (head, _, tail, p1, p2) ->
        let new_proof = set_premises proof [rec_commute_up_permutations p1 (identity (List.length head + 1)); rec_commute_up_permutations p2 (identity (1 + List.length tail))] in
        if perm = identity (List.length conclusion) then new_proof else Exchange_proof (conclusion, perm, perm, new_proof)
    (* TODO notations *)
    | Unfold_litt_proof _ -> raise (Failure "Unfold litt not implemented yet")
    | Unfold_dual_proof _ -> raise (Failure "Unfold dual not implemented yet")
    | Hypothesis_proof sequent -> Hypothesis_proof (permute sequent perm);;

let commute_up_permutations proof =
    let n = List.length (get_conclusion proof) in
    rec_commute_up_permutations proof (identity n)


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
    | [], _ -> None
    | _, [] -> None
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
    | Tensor_proof (_head, e1, e2, tail, Weakening_proof (head_wk, formula, e :: tail_wk, p), p2) ->
        (* Last element of tail_wk should be equal to e1. If e1 is weakened just after, we can not commute down weakening (cf next case).*)
        let new_tail_wk = head_n (e :: tail_wk) (List.length tail_wk) in
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Tensor_proof (head_wk @ new_tail_wk, e1, e2, tail, p, p2))) in
        true, Weakening_proof (head_wk, formula, new_tail_wk @ [Sequent.Tensor (e1,e2)] @ tail, new_proof)
    | Tensor_proof (head, e1, e2, tail, Weakening_proof (_head_wk1, f1, [], Weakening_proof (head_wk2, f2, tail_wk2, p)), p2) ->
        (* e1 should be equal to Whynot f1. We swap the two weakenings. *)
        let swapped_weakenings = Weakening_proof (head_wk2, f2, tail_wk2 @ [e1], Weakening_proof (head_wk2 @ tail_wk2, f1, [], p)) in
        rec_commute_down_weakenings (Tensor_proof (head, e1, e2, tail, swapped_weakenings, p2))
    | Tensor_proof (head, e1, e2, _, p1, Weakening_proof (_e :: head_wk, formula, tail_wk, p)) ->
        (* _e should be equal to e2. If e2 is weakened just after, we can not commute down weakening (cf next case).*)
        let new_proof = get_commuted_proof (rec_commute_down_weakenings (Tensor_proof (head, e1, e2, head_wk @ tail_wk, p1, p))) in
        true, Weakening_proof (head @ [Sequent.Tensor (e1,e2)] @ head_wk, formula, tail_wk, new_proof)
    | Tensor_proof (head, e1, e2, tail, p1, Weakening_proof ([], f1, _tail_wk1, Weakening_proof (head_wk2, f2, tail_wk2, p))) ->
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
    | With_proof (head, e1, e2, tail, Weakening_proof (head_wk1, f1, tail_wk1, p1), Weakening_proof (head_wk2, _f2, _tail_wk2, p2))
        when List.length head_wk1 = List.length head_wk2 && List.length head_wk1 <> List.length head ->
        (* f1 and _f2 should be equal, we can commute down both weakenings below With_proof *)
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
    | Exchange_proof (_sequent, display_permutation, permutation, Weakening_proof (head_wk, formula, tail_wk, p)) ->
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

(* SIMPLIFY : REMOVE LOOP *)

let get_proof short_proof =
    let p, _, _ = short_proof in p

let get_size short_proof =
    let _, size, _ = short_proof in size

let get_has_simplified short_proof =
    let _, _, has_simplified = short_proof in has_simplified

let rec sum = function
  | [] -> 0
  | n :: l -> n + (sum l)

let rec min = function
    | [] -> None
    | e :: [] -> Some e
    | e :: tail -> match min tail with None -> assert false | Some m -> if (get_size e) <= (get_size m) then Some e else Some m

let get_first_size proofs_of_sequent =
    let _, _, size, _ = List.hd proofs_of_sequent in size

let rec get_proofs_of_all_sequents proof =
    let s = get_conclusion proof in
    let proofs_by_premises = List.map get_proofs_of_all_sequents (get_premises proof) in
    let size = 1 + sum (List.map get_first_size proofs_by_premises) in
    let all_proofs_of_premises = List.concat proofs_by_premises in
    (s, sort s, size, proof) :: all_proofs_of_premises

let rec get_sequents_and_weakenings head = function
    | [] -> [[], []]
    | e :: tail ->
        let sequents_with_e = get_sequents_and_weakenings (head @ [e]) tail in
        let without_weakening = List.map (fun (s, w) -> (e :: s, w)) sequents_with_e in
        match e with
        | Whynot f ->
            let sequents_without_e = get_sequents_and_weakenings head tail in
            let with_weakening = List.map (fun (s, w) -> (s, (head, f, tail) :: w)) sequents_without_e in
            without_weakening @ with_weakening
        | _ ->  without_weakening

let rec find_shorter_proof original_sequent sorted_sequent max_size l =
    match l with
    | [] -> None
    | (original, sorted, size, p) :: tail ->
        if size < max_size && original = original_sequent
            then Some (p, size, true)
        else if size < max_size && sorted = sorted_sequent
            then Some (permute_proof p original_sequent, size + 1, true)
        else find_shorter_proof original_sequent sorted_sequent max_size tail

let rec add_weakenings l = function
    | [] -> l
    | (h, f, t) :: tail ->
        let short_proof = add_weakenings l tail in
        let p = get_proof short_proof in
        let size = get_size short_proof in
        Weakening_proof (h, f, t, p), size + 1, true

let find_shorter_proof_up_to_weakening sequent weakenings size sorted_proofs =
    let sorted = sort sequent in
    match find_shorter_proof sequent sorted (size - 1 - List.length weakenings) sorted_proofs with
    | Some l -> Some (add_weakenings l weakenings)
    | None -> None

let rec get_shortest_proof sorted_proofs proof =
    let premises_shortest_proof = List.map (get_shortest_proof sorted_proofs) (get_premises proof)  in
    let size = 1 + sum (List.map get_size premises_shortest_proof) in
    let sequents_and_weakenings = get_sequents_and_weakenings [] (get_conclusion proof) in
    let shorter_proofs = List.filter_map (fun (s, w) -> find_shorter_proof_up_to_weakening s w size sorted_proofs) sequents_and_weakenings in
    match min shorter_proofs with
    | Some l -> l
    | None ->
        let p = set_premises proof (List.map get_proof premises_shortest_proof) in
        let has_simplified = List.exists get_has_simplified premises_shortest_proof in
        p, size, has_simplified

let rec remove_loop proof =
    let commuted_proof = commute_down_weakenings (commute_up_permutations proof) in
    let proofs_of_all_sequents = get_proofs_of_all_sequents commuted_proof in
    let sorted_proofs = List.sort (fun (_,_,s1,_) (_,_,s2,_) -> s1 - s2) proofs_of_all_sequents in
    let shortest_proof = get_shortest_proof sorted_proofs commuted_proof in
    let new_proof = get_proof shortest_proof in
    if get_has_simplified shortest_proof then remove_loop new_proof else new_proof