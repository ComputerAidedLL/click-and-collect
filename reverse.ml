open Sequent
open Proof


(* REVERSE *)

(* try to systematically apply reversible rules in the following order:
   Top, Bot, Par, Dereliction (for ??), With, Oc (with ? context),
   One (with empty context), Tensor (with empty context), Axiom *)
let rec invertible_proof_part sequent =
    auto_apply_top_context [] sequent
and auto_apply_top_context left_context = function
    (* search for Top and apply Top_proof with left context left_context *)
    | [] -> auto_apply_bottom_context [] left_context
    | Top :: context -> Top_proof (left_context, context)
    | f :: context -> auto_apply_top_context (left_context @ [f]) context
and auto_apply_bottom_context left_context = function
    (* search for Bottom and apply Bottom_proof with left context left_context *)
    | [] -> auto_apply_par_context [] left_context
    | Bottom :: context -> let subproof = invertible_proof_part (left_context @ context) in
                           Bottom_proof (left_context, context, subproof)
    | f :: context -> auto_apply_bottom_context (left_context @ [f]) context
and auto_apply_par_context left_context = function
    (* search for Par and apply Par_proof with left context left_context *)
    | [] -> auto_apply_de_wn_context [] left_context
    | Par (f1, f2) :: context -> let subproof = invertible_proof_part (left_context @ f1 :: f2 :: context) in
                                 Par_proof (left_context, f1, f2, context, subproof)
    | f :: context -> auto_apply_par_context (left_context @ [f]) context
and auto_apply_de_wn_context left_context = function
    (* search for Whynot (Whynot _) and apply Dereliction_proof with left context left_context *)
    | [] -> auto_apply_with_context [] left_context
    | Whynot (Whynot f) :: context -> let subproof = invertible_proof_part (left_context @ Whynot f :: context) in
                                      Dereliction_proof (left_context, f, context, subproof)
    | f :: context -> auto_apply_de_wn_context (left_context @ [f]) context
and auto_apply_with_context left_context = function
    (* search for With and apply With_proof with left context left_context *)
    | [] -> auto_apply_oc_context [] left_context
    | With (f1, f2) :: context -> let subproof1 = invertible_proof_part (left_context @ f1 :: context) in
                                  let subproof2 = invertible_proof_part (left_context @ f2 :: context) in
                                  With_proof (left_context, f1, f2, context, subproof1, subproof2)
    | f :: context -> auto_apply_with_context (left_context @ [f]) context
and auto_apply_oc_context left_context = function
    (* search for Ofcourse and apply Promotion_proof with left context (add_whynot left_context) *)
    | [] -> Hypothesis_proof (add_whynot left_context)
    | Ofcourse f :: context -> 
       (try 
         let right_context = remove_whynot context in
         let subproof = invertible_proof_part (add_whynot left_context @ f :: context) in
         Promotion_proof (left_context, f, right_context, subproof)
        with Not_whynot -> auto_apply_unary_tensor_one_ax (add_whynot left_context @ Ofcourse f :: context))
    | Whynot f :: context -> auto_apply_oc_context (left_context @ [f]) context
    | context -> auto_apply_unary_tensor_one_ax (add_whynot left_context @ context)
and auto_apply_unary_tensor_one_ax = function
    (* search for One or Tensor in singleton sequent or pair of dual formulas
       and apply associated rule which is then reversible *)
    | [One] -> One_proof
    | [Tensor (f1, f2)] -> let subproof1 = invertible_proof_part [f1] in
                           let subproof2 = invertible_proof_part [f2] in
                           Tensor_proof ([], f1, f2, [], subproof1, subproof2)
    | [f1; f2] -> if dual f1 = f2 then Axiom_proof f1 else Hypothesis_proof [f1; f2]
    | context -> Hypothesis_proof context

