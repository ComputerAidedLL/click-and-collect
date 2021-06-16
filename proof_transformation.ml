open Sequent
open Proof
open Proof_with_notations
open Transform_request

exception Transform_exception of string;;

let expand_axiom notations = function
    | One -> Bottom_proof ([One], [], One_proof)
    | Bottom -> Bottom_proof ([], [One], One_proof)
    | Top -> Top_proof ([], [Zero])
    | Zero -> Top_proof ([Zero], [])
    | Litt s when List.mem_assoc s notations ->
        let f = Raw_sequent.to_formula (List.assoc s notations) in
        Unfold_litt_proof ([], s, [Dual s], Unfold_dual_proof ([f], s, [], Axiom_proof f))
    | Dual s when List.mem_assoc s notations ->
        let f = Raw_sequent.to_formula (List.assoc s notations) in
        Unfold_dual_proof ([], s, [Litt s], Unfold_litt_proof ([dual f], s, [], Axiom_proof (dual f)))
    | Litt s -> Axiom_proof (Litt s)
    | Dual s -> Axiom_proof (Dual s)
    | Tensor (e1, e2) ->
         let e1' = dual e1 in
         let e2' = dual e2 in
         let tensor_proof = Tensor_proof ([e1'], e1, e2, [e2'], Axiom_proof e1', Axiom_proof e2) in
         let permutation = [1; 0; 2] in
         let exchange_proof = Exchange_proof ([e1'; Tensor (e1, e2); e2'], permutation, permutation, tensor_proof) in
         Par_proof ([Tensor (e1, e2)], e1', e2', [], exchange_proof)
    | Par (e1, e2) ->
        let e1' = dual e1 in
        let e2' = dual e2 in
        let tensor_proof = Tensor_proof ([e1], e1', e2', [e2], Axiom_proof e1, Axiom_proof e2') in
        let permutation = [0; 2; 1] in
        let exchange_proof = Exchange_proof ([e1; Tensor (e1', e2'); e2], permutation, permutation, tensor_proof) in
        Par_proof ([], e1, e2, [Tensor (e1', e2')], exchange_proof)
    | With (e1, e2) ->
        let e1' = dual e1 in
        let e2' = dual e2 in
        let plus_left_proof = Plus_left_proof ([e1], e1', e2', [], Axiom_proof e1) in
        let plus_right_proof = Plus_right_proof ([e2], e1', e2', [], Axiom_proof e2) in
        With_proof ([], e1, e2, [Plus (e1', e2')], plus_left_proof, plus_right_proof)
    | Plus (e1, e2) ->
       let e1' = dual e1 in
       let e2' = dual e2 in
       let plus_left_proof = Plus_left_proof ([], e1, e2, [e1'], Axiom_proof e1) in
       let plus_right_proof = Plus_right_proof ([], e1, e2, [e2'], Axiom_proof e2) in
       With_proof ([Plus (e1, e2)], e1', e2', [], plus_left_proof, plus_right_proof)
    | Ofcourse e ->
       let e' = dual e in
       Promotion_proof ([], e, [e'], Dereliction_proof ([e], e', [], Axiom_proof e))
    | Whynot e ->
        let e' = dual e in
        Promotion_proof ([e], e', [], Dereliction_proof ([], e, [e'], Axiom_proof e))
    ;;

let rec expand_axiom_full notations proof =
    let new_proof = match proof with
        | Axiom_proof f -> expand_axiom notations f
        | _ -> proof in
    set_premises new_proof (List.map (expand_axiom_full notations) (get_premises new_proof))

(* OPERATIONS *)

let get_transformation_options_json proof notations =
    Proof.to_json ~transform_options:true ~notations:notations proof;;

let apply_transformation_with_exceptions proof cyclic_notations acyclic_notations = function
    | Expand_axiom -> begin match proof with
        | Axiom_proof f -> expand_axiom (cyclic_notations @ acyclic_notations) f
        | _ -> raise (Transform_exception ("Can only expand axiom on Axiom_proof"))
    end
    | Expand_axiom_full -> expand_axiom_full acyclic_notations proof;;

(* HANDLERS *)

let get_proof_transformation_options request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let proof_with_transformation_options = get_transformation_options_json proof_with_notations.proof proof_with_notations.notations in
        true, `Assoc ["proofWithTransformationOptions", proof_with_transformation_options]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad request: " ^ m);;

let apply_transformation request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let transform_request_as_json = Request_utils.get_key request_as_json "transformRequest" in
        let transform_request = Transform_request.from_json transform_request_as_json in
        let cyclic_notations, acyclic_notations = Notations.split_cyclic_acyclic proof_with_notations.notations None in
        let proof = apply_transformation_with_exceptions proof_with_notations.proof cyclic_notations acyclic_notations transform_request in
        let proof_with_transformation_options = get_transformation_options_json proof proof_with_notations.notations in
        true, `Assoc ["proofWithTransformationOptions", proof_with_transformation_options]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad proof with notations: " ^ m)
        | Request_utils.Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Transform_request.Json_exception m -> false, `String ("Bad transformation request: " ^ m)
        | Transform_exception m -> false, `String ("Transform exception: " ^ m);;