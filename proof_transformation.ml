open Sequent
open Proof
open Proof_with_notations
open Transform_request

exception Transform_exception of string;;

let expand_axiom formula =
    match formula with
    | One -> Axiom_proof formula (* TODO apply Bottom then One ? *)
    | Bottom -> Axiom_proof formula (* TODO apply Bottom then One ? *)
    | Top -> Axiom_proof formula (* TODO apply Top ? *)
    | Zero -> Axiom_proof formula (* TODO apply Top ? *)
    | Litt _s -> Axiom_proof formula (* TODO notations *)
    | Dual _s -> Axiom_proof formula (* TODO notations *)
    | Tensor (e1, e2) -> begin
         let e1' = dual e1 in
         let e2' = dual e2 in
         let tensor_proof = Tensor_proof ([e1'], e1, e2, [e2'], Axiom_proof e1', Axiom_proof e2) in
         let permutation = [1; 0; 2] in
         let exchange_proof = Exchange_proof ([e1'; Tensor (e1, e2); e2'], permutation, permutation, tensor_proof) in
         Par_proof ([Tensor (e1, e2)], e1', e2', [], exchange_proof)
     end
    | Par (e1, e2) -> begin
        let e1' = dual e1 in
        let e2' = dual e2 in
        let tensor_proof = Tensor_proof ([e1], e1', e2', [e2], Axiom_proof e1, Axiom_proof e2') in
        let permutation = [0; 2; 1] in
        let exchange_proof = Exchange_proof ([e1; Tensor (e1', e2'); e2], permutation, permutation, tensor_proof) in
        Par_proof ([], e1, e2, [Tensor (e1', e2')], exchange_proof)
    end
    | With (e1, e2) -> begin
        let e1' = dual e1 in
        let e2' = dual e2 in
        let plus_left_proof = Plus_left_proof ([e1], e1', e2', [], Axiom_proof e1) in
        let plus_right_proof = Plus_right_proof ([e2], e1', e2', [], Axiom_proof e2) in
        With_proof ([], e1, e2, [Plus (e1', e2')], plus_left_proof, plus_right_proof)
    end
    | Plus (e1, e2) -> begin
       let e1' = dual e1 in
       let e2' = dual e2 in
       let plus_left_proof = Plus_left_proof ([], e1, e2, [e1'], Axiom_proof e1) in
       let plus_right_proof = Plus_right_proof ([], e1, e2, [e2'], Axiom_proof e2) in
       With_proof ([Plus (e1, e2)], e1', e2', [], plus_left_proof, plus_right_proof)
    end
    | Ofcourse e -> begin
       let e' = dual e in
       Promotion_proof ([], e, [e'], Dereliction_proof ([e], e', [], Axiom_proof e))
    end
    | Whynot e -> begin
        let e' = dual e in
        Promotion_proof ([e], e', [], Dereliction_proof ([], e, [e'], Axiom_proof e))
    end;;

let rec expand_axiom_full proof =
    let new_proof = match proof with
        | Axiom_proof f -> expand_axiom f
        | _ -> proof in
    set_premises new_proof (List.map expand_axiom_full (get_premises new_proof))

(* OPERATIONS *)

let get_transformation_options_json proof =
    Proof.to_json ~transform_options:true proof;;

let apply_transformation_with_exceptions proof_with_notations = function
    | Expand_axiom -> begin match proof_with_notations.proof with
        | Axiom_proof f -> expand_axiom f
        | _ -> raise (Transform_exception ("Can only expand axiom on Axiom_proof"))
    end
    | Expand_axiom_full -> expand_axiom_full proof_with_notations.proof;;

(* HANDLERS *)

let get_proof_transformation_options request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let proof_with_transformation_options = get_transformation_options_json proof_with_notations.proof in
        true, `Assoc ["proofWithTransformationOptions", proof_with_transformation_options]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad request: " ^ m);;

let apply_transformation request_as_json =
    try let proof_with_notations = Proof_with_notations.from_json request_as_json in
        let transform_request_as_json = Request_utils.get_key request_as_json "transformRequest" in
        let transform_request = Transform_request.from_json transform_request_as_json in
        let proof = apply_transformation_with_exceptions proof_with_notations transform_request in
        let proof_with_transformation_options = get_transformation_options_json proof in
        true, `Assoc ["proofWithTransformationOptions", proof_with_transformation_options]
    with Proof_with_notations.Json_exception m -> false, `String ("Bad proof with notations: " ^ m)
        | Request_utils.Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Transform_request.Json_exception m -> false, `String ("Bad transformation request: " ^ m)
        | Transform_exception m -> false, `String ("Transform exception: " ^ m);;