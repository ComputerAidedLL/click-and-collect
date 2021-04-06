open Linear_logic

type rule =
    | Axiom
    | One
    | Bottom
    | Top
    | Zero
    | Tensor
    | Par
    | With
    | Plus_left
    | Plus_right
    | Promotion
    | Dereliction
    | Weakening
    | Contraction
    | Exchange;;

exception Bad_rule_string_exception of string;;

let string_to_rule rule_as_string = match rule_as_string with
    | "axiom" -> Axiom
    | "one" -> One
    | "bottom" -> Bottom
    | "top" -> Top
    | "zero" -> Zero
    | "tensor" -> Tensor
    | "par" -> Par
    | "with" -> With
    | "plus_left" -> Plus_left
    | "plus_right" -> Plus_right
    | "promotion" -> Promotion
    | "dereliction" -> Dereliction
    | "weakening" -> Weakening
    | "contraction" -> Contraction
    | "exchange" -> Exchange
    | _ -> raise (Bad_rule_string_exception ("unknown rule '" ^ rule_as_string ^ "'"));;

let rule_to_string = function
    | Axiom -> "axiom"
    | One -> "one"
    | Bottom -> "bottom"
    | Top -> "top"
    | Zero -> "zero"
    | Tensor -> "tensor"
    | Par -> "par"
    | With -> "with"
    | Plus_left -> "plus_left"
    | Plus_right -> "plus_right"
    | Promotion -> "promotion"
    | Dereliction -> "dereliction"
    | Weakening -> "weakening"
    | Contraction -> "contraction"
    | Exchange -> "exchange";;


exception Apply_rule_technical_exception of string;;
exception Apply_rule_logic_exception of string;;

let rec head_formula_tail_with_int formula_position = function
    | [] -> raise (Apply_rule_technical_exception "Argument formula_positions[0] is greater than the number of given formulas")
    | f :: formula_list -> if formula_position = 0
        then [], f, formula_list
        else let head, formula, tail = head_formula_tail_with_int (formula_position - 1) formula_list
        in f::head, formula, tail;;

let head_formula_tail = function
    | [] -> raise (Apply_rule_technical_exception "Argument formula_positions contains too few integer")
    | formula_position :: [] -> head_formula_tail_with_int formula_position
    | _ -> raise (Apply_rule_technical_exception "Argument formula_positions contains too many integers");;

let rec permute l = function
    | [] -> []
    | n :: tail -> (List.nth l n) :: (permute l tail);;

let is_valid_permutation l =
    let sorted_l = List.sort Int.compare l in
    let identity = List.init (List.length l) (fun n -> n) in
    sorted_l = identity;;

let apply_rule rule sequent formula_positions =
    (* Applying rule on sequent with non empty hypotheses is not implemented yet *)
    if sequent.hyp != [] then raise (Apply_rule_technical_exception ("This API can apply rule only on monolatery sequent for the moment"))

    else match rule with
    Axiom -> (
        match sequent.cons with
        | e1 :: e2 :: [] -> if orthogonal e1 = e2 then []
            else raise (Apply_rule_logic_exception ("Can not apply 'axiom' rule: the two formulas are not orthogonal."))
        | _ -> raise (Apply_rule_logic_exception ("Can not apply 'axiom' rule: the sequent must contain exactly two formulas."))
    )
    | One -> (
        match sequent.cons with
        | One :: [] -> []
        | _ -> raise (Apply_rule_logic_exception ("Can not apply 'one' rule: the sequent must be reduced to the single formula '1'."))
    )
    | Bottom -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        | Bottom -> [{hyp=[]; cons=(head @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Top -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        | Top -> []
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Zero -> raise (Apply_rule_logic_exception ("Can not apply 'zero' rule: there is no rule for introducing '0'."))
    | Tensor -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Tensor (e1, e2) -> [{hyp=[]; cons=(head @ [e1])}; {hyp=[]; cons=([e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Par -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Par (e1, e2) -> [{hyp=[]; cons=(head @ [e1; e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | With -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        With (e1, e2) -> [{hyp=[]; cons=(head @ [e1] @ tail)}; {hyp=[]; cons=(head @ [e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Plus_left -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Plus (e1, e2) -> [{hyp=[]; cons=(head @ [e1] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Plus_right -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Plus (e1, e2) -> [{hyp=[]; cons=(head @ [e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Promotion -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Ofcourse e -> if List.for_all is_whynot head && List.for_all is_whynot tail then [{hyp=[]; cons=(head @ [e] @ tail)}]
            else raise (Apply_rule_logic_exception ("Can not apply 'promotion' rule: the context must contain formulas starting by '?' only."))
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Dereliction -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ [e] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Weakening -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Contraction -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ [Whynot e; Whynot e] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ (rule_to_string rule) ^ "' on this formula"))
    )
    | Exchange -> (
        if List.length sequent.cons <> List.length formula_positions
        then raise (Apply_rule_technical_exception ("When applying exchange rule, formula_positions and sequent must have same size"))
        else if not (is_valid_permutation formula_positions)
        then raise (Apply_rule_technical_exception ("When applying exchange rule, formula_positions should be a permutation of the size of sequent formula list"))
        else [{hyp=[]; cons=permute sequent.cons formula_positions}]
    );;