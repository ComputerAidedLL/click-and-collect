open Sequent

type rule_request =
    | Axiom
    | One
    | Bottom of int
    | Top of int
    | Zero of int
    | Tensor of int
    | Par of int
    | With of int
    | Plus_left of int
    | Plus_right of int
    | Promotion of int
    | Dereliction of int
    | Weakening of int
    | Contraction of int
    | Exchange of int list;;


(* JSON -> RULE *)

exception Json_exception of string;;

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("rule_request must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required field '" ^ k ^ "' is missing"))
    else value;;

let get_string d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ k ^ "' must be a string"));;

let get_int_list d k =
    let value = get_key d k in
    try List.map Yojson.Basic.Util.to_int (Yojson.Basic.Util.to_list value)
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ k ^ "' must be a list of int"));;

let get_first = function
    | [] -> raise (Json_exception ("formula_positions is empty"))
    | n :: [] -> n
    | _ -> raise (Json_exception ("formula_positions must contain exactly one element for this rule"));;

let from_json rule_request_as_json =
    let rule = get_string rule_request_as_json "rule" in
    let formula_positions = get_int_list rule_request_as_json "formulaPositions" in
    match rule with
        | "axiom" -> Axiom
        | "one" -> One
        | "bottom" -> Bottom (get_first formula_positions)
        | "top" -> Top (get_first formula_positions)
        | "zero" -> Zero (get_first formula_positions)
        | "tensor" -> Tensor (get_first formula_positions)
        | "par" -> Par (get_first formula_positions)
        | "with" -> With (get_first formula_positions)
        | "plus_left" -> Plus_left (get_first formula_positions)
        | "plus_right" -> Plus_right (get_first formula_positions)
        | "promotion" -> Promotion (get_first formula_positions)
        | "dereliction" -> Dereliction (get_first formula_positions)
        | "weakening" -> Weakening (get_first formula_positions)
        | "contraction" -> Contraction (get_first formula_positions)
        | "exchange" -> Exchange formula_positions
        | _ -> raise (Json_exception ("unknown rule '" ^ rule ^ "'"));;


(* RULE -> COQ *)
let coq_apply coq_rule =
    Printf.sprintf "apply %s.\n" coq_rule;;

let coq_change new_sequent =
    Printf.sprintf "change (%s).\n" (Sequent.sequent_to_coq new_sequent);;

let rule_to_coq rule sequent formula_positions =
    match rule with
    Axiom -> (match sequent.cons with
        | Orth e1 :: e2 :: [] -> coq_apply "ax_exp"
        | e1 :: Orth e2 :: [] -> coq_apply "ax_exp2"
        | e1 :: e2 :: [] -> let new_sequent = {hyp=sequent.hyp; cons=[Orth e2; e2]} in
            coq_change new_sequent  ^ coq_apply "ax_exp"
        | _ -> "error")
    | _ -> "not implemented";;