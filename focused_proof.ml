(*** Backward proof search in LL (LLF) ***)

open Sequent
open Sequent_with_notations
open Proof
open Permutations

type llf_rule =
  | One_intro
  | Top_intro
  | Bottom_intro
  | Par_intro
  | With_intro
  | Tensor_intro of formula list * formula list
  | Plus_left_intro
  | Plus_right_intro
  | Ofcourse_intro
  | Whynot_intro
  | Axiom_central
  | Axiom_exponential
  | Focusing_central of formula * formula list
  | Focusing_exponential of formula
  | Async_on_pos
  | Sync_on_neg

module Set_formula =
  Set.Make(struct type t = formula let compare = compare end)

type llf_sequent =
  | Async of Set_formula.t * formula list * formula list
  | Sync of Set_formula.t * formula list * formula

type llf_proof =
  | Node of llf_sequent * llf_rule * llf_proof list
  | Null

(* [is_async f] checks if [f] is asynchronous. *)
let is_async = function
  | With _ | Par _ | Top | Bottom | Whynot _ -> true
  | _ -> false

(* [is_neg f] checks if [f] is a negative atom. *)
let is_dual = function
  | Dual _ -> true
  | _ -> false

(* [is_atom f] checks if [f] is an atom. *)
let is_atom = function
  | Litt _ | Dual _ -> true
  | _ -> false

(* [is_atom f] checks if the top-level connective of [f] is binary. *)
let is_binop = function
  | Tensor _ | Plus _ | With _ | Par _ -> true
  | _ -> false

(* [left_sync f] checks if [f] is left-synchronous (for ILLF). *)
let left_sync = function
  | With _ | Top | Litt _ -> true
  | _ -> false

(* [right_sync f] checks if [f] is right-synchronous (for ILLF). *)
let right_sync = function
  | Tensor _ | Zero | One | Ofcourse _ | Litt _ | Plus _ -> true
  | _ -> false

(* [left_async f] checks if [f] is left-asynchronous (for ILLF). *)
let left_async = function
  | Tensor _ | Zero | One | Ofcourse _ | Plus _ -> true
  | _ -> false

(* [right_async f] checks if [f] is right-asynchronous (for ILLF). *)
let right_async = function
  | With _ | Top -> true
  | _ -> false

(* [whynot_height f] returns the whynot-height of [f]. *)
let rec whynot_height = function
  | Litt _ | Dual _ | One | Zero | Top | Bottom -> 0
  | Ofcourse f -> whynot_height f
  | Whynot f -> whynot_height f + 1
  | Tensor (f, g) | Plus (f, g) | With (f, g) | Par (f, g) ->
      max (whynot_height f) (whynot_height g)

(** Functions for manipulating LLF sequents **)

let map_wn = List.map (fun x -> Whynot x)
let map_wn_set theta_set = map_wn (Set_formula.elements theta_set)

(** Manipulation of the type ['a option] **)

exception NoValue

let get_op = function
  | Some x -> x
  | None -> raise NoValue

(** Functions for splitting contexts **)

let rec split_list_aux (acc1, acc2) l k = match l with
  | [] -> acc1, acc2
  | hd :: tl ->
      if k mod 2 = 0 then split_list_aux (acc1, hd :: acc2) tl (k / 2)
      else split_list_aux (hd :: acc1, acc2) tl (k / 2)

let split_list l k =
  split_list_aux ([], []) l k

let rec fast_exp_aux acc m k =
    if k = 0 then acc
    else
      if k = 1 then m * acc
      else
        if k mod 2 = 1 then
          fast_exp_aux (acc * m) (m*m) (k / 2)
        else
          fast_exp_aux acc (m*m) (k/2)

let fast_exp_2 k =
  fast_exp_aux 1 2 k

let fast_exp m k =
  fast_exp_aux 1 m k

let rec choose_kth_from_list k l = match l with
  | [] -> assert false
  | [x] -> x, []
  | hd :: tl ->
      if k = 0 then hd, tl
      else
        let x, tl' = choose_kth_from_list (k - 1) tl in
        x, hd :: tl'

(* [sort_whynot l] sorts the list of formulas [l] in increasing order using
   [whynot_height f] as the key of [f]. *)
let sort_whynot l =
  List.sort (fun x y -> whynot_height x - whynot_height y) l

exception Ttl_exceeded
let has_reached_exponential_bound = ref false

(* [prove sequent select_d2 ttl] attempts to prove the sequent [sequent]
   where [select_d2] contains the candidates for the Focusing_exponential rule,
   [max_d2] is a (pseudo-)bound on the number of applications of the Focusing_exponential rule,
   and [ttl] is a time to leave. *)
let rec prove sequent select_d2 max_d2 ttl =
  (if Sys.time () > ttl then raise Ttl_exceeded);
  match sequent with
  | Async (theta, gamma, l) -> begin match l with
      | [] ->
          let rec apply_d1 k =
            if k = List.length gamma then None
            else
              let f, gamma' = choose_kth_from_list k gamma in
              if is_dual f then apply_d1 (k + 1)
              else
                try
                  let p = get_op (prove (Sync (theta, gamma', f)) select_d2 max_d2 ttl) in
                  Some (Node (sequent, Focusing_central (f, gamma'), [p]))
                with NoValue -> apply_d1 (k + 1) in
          begin try
            Some (get_op (apply_d1 0))
          with NoValue ->
            let rec apply_d2 select_d2 max_d2 =
              let f = List.hd select_d2 in
              try
                let p = get_op (prove (Sync (theta, gamma, f)) (List.tl select_d2) max_d2 ttl) in
                Some (Node (sequent, Focusing_exponential f, [p]))
              with NoValue ->
                apply_d2' (List.tl select_d2) max_d2
            and apply_d2' select_d2 max_d2 =
              if select_d2 = [] then begin
                (if max_d2 = 0 then (has_reached_exponential_bound := true; raise NoValue));
                let select_d2' =
                  sort_whynot (List.filter (fun x -> not (is_dual x))
                  (Set_formula.elements theta)) in
                if select_d2' = [] then None
                else
                  apply_d2 select_d2' (max_d2 - 1) end
              else
                apply_d2 select_d2 max_d2
            in
            begin try
              if Set_formula.for_all is_dual theta then None
              else
                Some (get_op (apply_d2' select_d2 max_d2))
            with NoValue -> None end end
      | hd :: tl ->
          begin match hd with
          | Bottom ->
              begin try
                let p = get_op (prove (Async (theta, gamma, tl)) select_d2 max_d2 ttl) in
                Some (Node (sequent, Bottom_intro, [p]))
              with NoValue -> None end
          | Top -> Some (Node (sequent, Top_intro, [Null]))
          | With (f, g) ->
              if whynot_height f > whynot_height g then
                try
                  let pg = get_op (prove (Async (theta, gamma, g :: tl)) select_d2 max_d2 ttl) in
                  let pf = get_op (prove (Async (theta, gamma, f :: tl)) select_d2 max_d2 ttl) in
                  Some (Node (sequent, With_intro, [pf; pg]))
                with NoValue -> None
              else
                begin try
                  let pf = get_op (prove (Async (theta, gamma, f :: tl)) select_d2 max_d2 ttl) in
                  let pg = get_op (prove (Async (theta, gamma, g :: tl)) select_d2 max_d2 ttl) in
                  Some (Node (sequent, With_intro, [pf; pg]))
              with NoValue -> None end
          | Par (f, g) ->
              begin try
                let p = get_op (prove (Async (theta, gamma, f :: g :: tl)) select_d2 max_d2 ttl) in
                Some (Node (sequent, Par_intro, [p]))
              with NoValue -> None end
          | Whynot g ->
              begin try
                let p =
                  get_op
                    (prove (Async (Set_formula.add g theta, gamma, tl))
                    select_d2 max_d2 ttl) in
                Some (Node (sequent, Whynot_intro, [p]))
              with NoValue ->
                None end
          | _ ->
              try
                let p = get_op (prove (Async (theta, hd :: gamma, tl)) select_d2 max_d2 ttl) in
                Some (Node (sequent, Async_on_pos, [p]))
              with NoValue -> None
           end end
  | Sync (theta, gamma, f) ->
      match f with
      | _ when is_async f || is_dual f ->
          begin try
            let p = get_op (prove (Async (theta, gamma, [f])) select_d2 max_d2 ttl) in
            Some (Node (sequent, Sync_on_neg, [p]))
          with NoValue -> None end
      | One ->
          if List.length gamma = 0 then
            Some (Node (sequent, One_intro, [Null]))
          else
            None
      | Plus (g, h) ->
          if whynot_height g > whynot_height h then
            try
              let p = get_op (prove (Sync (theta, gamma, h)) select_d2 max_d2 ttl) in
              Some (Node (sequent, Plus_right_intro, [p]))
            with NoValue ->
              try
                let p = get_op (prove (Sync (theta, gamma, g)) select_d2 max_d2 ttl) in
                Some (Node (sequent, Plus_left_intro, [p]))
              with NoValue -> None
          else
            begin try
              let p = get_op (prove (Sync (theta, gamma, g)) select_d2 max_d2 ttl) in
              Some (Node (sequent, Plus_left_intro, [p]))
            with NoValue ->
              try
                let p = get_op (prove (Sync (theta, gamma, h)) select_d2 max_d2 ttl) in
                Some (Node (sequent, Plus_right_intro, [p]))
              with NoValue -> None end
      | Tensor (g, h) ->
          let rec split_gamma k =
            if k = -1 then None
            else
              let gamma1, gamma2 = split_list gamma k in
              try
                if whynot_height g > whynot_height h then
                  let ph =
                    get_op (prove (Sync (theta, gamma2, h)) select_d2 max_d2 ttl) in
                  let pg =
                    get_op (prove (Sync (theta, gamma1, g)) select_d2 max_d2 ttl) in
                  Some (Node (sequent, Tensor_intro (gamma1, gamma2), [pg; ph]))
                else
                  let pg =
                    get_op (prove (Sync (theta, gamma1, g)) select_d2 max_d2 ttl) in
                  let ph =
                    get_op (prove (Sync (theta, gamma2, h)) select_d2 max_d2 ttl) in
                  Some (Node (sequent, Tensor_intro (gamma1, gamma2), [pg; ph]))
              with NoValue ->
                split_gamma (k - 1) in
          let k = fast_exp_2 (List.length gamma) - 1 in
          split_gamma k
      | Ofcourse g ->
          if gamma = [] then
            try
              let p = get_op (prove (Async (theta, gamma, [g])) select_d2 max_d2 ttl) in
              Some (Node (sequent, Ofcourse_intro, [p]))
            with NoValue -> None
          else
            None
      | Litt atom ->
          if gamma = [Dual atom] then
            Some (Node (sequent, Axiom_central, [Null]))
          else
            if gamma = [] && Set_formula.mem (Dual atom) theta then
              Some (Node (sequent, Axiom_exponential, [Null]))
            else
              None
      | _ -> None

(* FOCUSED <-> NOT FOCUSED *)
exception NotFound

let rec double_list = function
    | [] -> []
    | e :: tail -> e :: e :: double_list tail

let rec unfocus_proof = function
    | Null -> raise (Failure "Focused proof is null")
    | Node (focused_sequent, rule, focused_premises) ->
        match rule with
            | One_intro -> begin match focused_sequent with
                | Sync (theta_set, [], One) ->
                    let theta = Set_formula.elements theta_set in
                    weaken One_proof [] [One] theta
                | _ -> raise (Failure "sync formula with only one expected") end
            | Top_intro -> begin match focused_sequent with
                | Async (theta_set, gamma, Top :: tail) ->
                    Top_proof (map_wn_set theta_set @ gamma, tail)
                | _ -> raise (Failure "async with top expected") end
            | Bottom_intro -> begin match focused_sequent with
                | Async (theta_set, gamma, Bottom :: tail) ->
                    Bottom_proof (map_wn_set theta_set @ gamma, tail, unfocus_proof (List.hd focused_premises))
                | _ -> raise (Failure "async with bottom expected") end
            | Par_intro -> begin match focused_sequent with
                | Async (theta_set, gamma, Par (e1, e2) :: tail) ->
                    let premise = unfocus_proof (List.hd focused_premises) in
                    Par_proof (map_wn_set theta_set @ gamma, e1, e2, tail, premise)
                | _ -> raise (Failure "async with par expected") end
            | With_intro -> begin match focused_sequent with
                | Async (theta_set, gamma, With (e1, e2) :: tail) ->
                    let premise1 = unfocus_proof (List.hd focused_premises) in
                    let premise2 = unfocus_proof (List.nth focused_premises 1) in
                    With_proof (map_wn_set theta_set @ gamma, e1, e2, tail, premise1, premise2)
                | _ -> raise (Failure "async with with expected") end
            | Tensor_intro (gamma1, gamma2) -> begin match focused_sequent with
                | Sync (theta_set, gamma, Tensor (e1,e2)) ->
                    let theta = Set_formula.elements theta_set in
                    let premise1 = unfocus_proof (List.hd focused_premises) in
                    let premise2 = unfocus_proof (List.nth focused_premises 1) in
                    let exchanged_premise2 = move_left 0 0 premise2 in
                    let tensor_proof = Tensor_proof (map_wn theta @ gamma1, e1, e2, map_wn theta @ gamma2, premise1, exchanged_premise2) in
                    let theta_by_pair = permute_proof tensor_proof ((map_wn (double_list theta)) @ gamma @ [Tensor (e1,e2)]) in
                    contract theta_by_pair [] (gamma @ [Tensor (e1,e2)]) theta
                | _ -> raise (Failure "sync tensor expected") end
            | Plus_left_intro -> begin match focused_sequent with
                | Sync (theta_set, gamma, Plus (e1, e2)) ->
                    Plus_left_proof (map_wn_set theta_set @ gamma, e1, e2, [], unfocus_proof (List.hd focused_premises))
                | _ -> raise (Failure "async with plus expected") end
            | Plus_right_intro -> begin match focused_sequent with
                | Sync (theta_set, gamma, Plus (e1, e2)) ->
                    Plus_right_proof (map_wn_set theta_set @ gamma, e1, e2, [], unfocus_proof (List.hd focused_premises))
                | _ -> raise (Failure "async with plus expected") end
            | Ofcourse_intro -> begin match focused_sequent with
                | Sync (theta_set, [], Ofcourse e) ->
                    let theta = Set_formula.elements theta_set in
                    Promotion_proof (theta, e, [], unfocus_proof (List.hd focused_premises))
                | _ -> raise (Failure "sync with ofcourse expected") end
            | Whynot_intro -> begin match focused_sequent with
                | Async (theta_set, gamma, (Whynot e) :: tail) ->
                    let premise = unfocus_proof (List.hd focused_premises) in
                    if Set_formula.mem e theta_set then
                        Weakening_proof (map_wn_set theta_set @ gamma, e, tail, premise)
                    else
                        let premise_theta = Set_formula.elements (Set_formula.add e theta_set) in
                        let theta_head, _theta_tail = head_tail e premise_theta in
                        move_right (List.length theta_head) (List.length tail) premise
                | _ -> raise (Failure "async formula with whynot expected") end
            | Axiom_central -> begin match focused_sequent with
                | Sync (theta_set, [Dual s], Litt t) when s = t ->
                    let theta = Set_formula.elements theta_set in
                    weaken (Axiom_proof (Dual s)) [] [Dual s; Litt s] theta
                | _ -> raise (Failure "sync formula expected") end
            | Axiom_exponential -> begin match focused_sequent with
                | Sync (theta_set, [], Litt s) ->
                    let theta = Set_formula.elements theta_set in
                    let axiom_proof = Axiom_proof (Dual s) in
                    let dereliction = Dereliction_proof ([], Dual s, [Litt s], axiom_proof) in
                    let head, tail = head_tail (Dual s) theta in
                    let weakening_tail_proof = weaken dereliction [Whynot (Dual s)] [Litt s] tail in
                    weaken weakening_tail_proof [] ([Whynot (Dual s)] @ map_wn tail @ [Litt s]) head
                | _ -> raise (Failure "sync formula with empty gamma expected") end
            | Focusing_central (formula, _) -> begin match focused_sequent with
                | Async (theta_set, gamma, []) ->
                    let premise = unfocus_proof (List.hd focused_premises) in
                    let head, _tail = head_tail formula gamma in
                    move_left (Set_formula.cardinal theta_set + List.length head) 0 premise
                | _ -> raise (Failure "async empty expected") end
            | Focusing_exponential formula -> begin match focused_sequent with
                | Async (theta_set, gamma, []) ->
                    let premise = unfocus_proof (List.hd focused_premises) in
                    let dereliction = Dereliction_proof (map_wn_set theta_set @ gamma, formula, [], premise) in
                    let head, tail = head_tail (Whynot formula) (map_wn_set theta_set) in
                    let exchange = move_left (List.length head + 1) 0 dereliction in
                    Contraction_proof (head, formula, tail @ gamma, exchange)
                | _ -> raise (Failure "async empty expected") end
            | Async_on_pos -> begin match focused_sequent with
                | Async (theta_set, _gamma, _e :: tail) ->
                    let premise = unfocus_proof (List.hd focused_premises) in
                    move_right (Set_formula.cardinal theta_set) (List.length tail) premise
                | _ -> raise (Failure "async formula expected") end
            | Sync_on_neg -> unfocus_proof (List.hd focused_premises)

let sequent_to_focused_sequent sequent =
    Async (Set_formula.empty, [], sequent)

let proof_from_focused_proof focused_proof cyclic_notations acyclic_notations original_sequent =
    let proof = unfocus_proof focused_proof in
    let simplified_proof = Proof_simplification.remove_loop proof in
    Proof.from_fully_replaced_proof cyclic_notations acyclic_notations original_sequent simplified_proof

let rec iterate_on_notations_list original_sequent cyclic_notations acyclic_notations exponential_bound ttl = function
    | [] -> None
    | replaced_sequent :: tail ->
        let focused_sequent = sequent_to_focused_sequent replaced_sequent in
        match prove focused_sequent [] exponential_bound ttl with
           | Some focused_proof -> Some (proof_from_focused_proof focused_proof cyclic_notations acyclic_notations original_sequent)
           | None -> iterate_on_notations_list original_sequent cyclic_notations acyclic_notations exponential_bound ttl tail

let rec replace_notation_in_formula_or_not ttl notation_name notation_formula formula =
    (if Sys.time () > ttl then raise Ttl_exceeded);
    match formula with
    | Litt s when s = notation_name -> [notation_formula; Litt s]
    | Dual s when s = notation_name -> [dual notation_formula; Dual s]
    | Tensor (e1, e2) ->
        let l1 = replace_notation_in_formula_or_not ttl notation_name notation_formula e1 in
        let l2 = replace_notation_in_formula_or_not ttl notation_name notation_formula e2 in
        List.concat_map (fun f1 -> List.map (fun f2 -> Tensor (f1, f2)) l2) l1
    | Par (e1, e2) ->
        let l1 = replace_notation_in_formula_or_not ttl notation_name notation_formula e1 in
        let l2 = replace_notation_in_formula_or_not ttl notation_name notation_formula e2 in
        List.concat_map (fun f1 -> List.map (fun f2 -> Par (f1, f2)) l2) l1
    | With (e1, e2) ->
        let l1 = replace_notation_in_formula_or_not ttl notation_name notation_formula e1 in
        let l2 = replace_notation_in_formula_or_not ttl notation_name notation_formula e2 in
        List.concat_map (fun f1 -> List.map (fun f2 -> With (f1, f2)) l2) l1
    | Plus (e1, e2) ->
        let l1 = replace_notation_in_formula_or_not ttl notation_name notation_formula e1 in
        let l2 = replace_notation_in_formula_or_not ttl notation_name notation_formula e2 in
        List.concat_map (fun f1 -> List.map (fun f2 -> Plus (f1, f2)) l2) l1
    | Ofcourse e ->
        let l = replace_notation_in_formula_or_not ttl notation_name notation_formula e in
        List.map (fun f -> Ofcourse f) l
    | Whynot e ->
        let l = replace_notation_in_formula_or_not ttl notation_name notation_formula e in
        List.map (fun f -> Whynot f) l
    | _ -> [formula];;

let rec replace_notation_in_sequent_or_not ttl notation_name notation_formula = function
    | [] -> [[]]
    | e :: tail ->
        let formulas = replace_notation_in_formula_or_not ttl notation_name notation_formula e in
        let tails = replace_notation_in_sequent_or_not ttl notation_name notation_formula tail in
        List.concat_map (fun f -> List.map (fun t -> f::t) tails) formulas;;

let replace_notation_in_sequents ttl sequents notation =
    let notation_name, rf = notation in
    let notation_formula = Raw_sequent.to_formula rf in
    List.concat_map (replace_notation_in_sequent_or_not ttl notation_name notation_formula) sequents

let rec prove_with_increasing_bound original_sequent cyclic_notations acyclic_notations exponential_bound ttl replaced_sequents =
    has_reached_exponential_bound := false;
    match iterate_on_notations_list original_sequent cyclic_notations acyclic_notations exponential_bound ttl replaced_sequents with
        | None ->
            let new_replaced_sequents = List.sort_uniq compare (replaced_sequents @
                List.concat_map (replace_notation_in_sequents ttl replaced_sequents) cyclic_notations) in
            if !has_reached_exponential_bound || new_replaced_sequents <> replaced_sequents
            then prove_with_increasing_bound original_sequent cyclic_notations acyclic_notations (exponential_bound + 1) ttl new_replaced_sequents
            else (None, false)
        | Some proof -> (Some proof, true)

(* [prove_sequent sequent_with_notations] attempts to prove [sequent_with_notations.sequent]
   and returns the result [(proof opt, is_provable)].
   If proof option is None, then:
   * is_provable is false if auto_prover performed an exhaustive research
   * is_provable is true if max execution time was reached. *)
let prove_sequent sequent_with_notations =
    let sequent_variables = Sequent.get_unique_variable_names sequent_with_notations.sequent in
    let cyclic_notations, acyclic_notations = Notations.split_cyclic_acyclic sequent_with_notations.notations (Some sequent_variables) in
    let max_execution_time_in_seconds = 3. in
    let ttl = Sys.time () +. max_execution_time_in_seconds in
    let replaced_sequents = [replace_all_notations_in_sequent sequent_with_notations.sequent acyclic_notations] in
    try prove_with_increasing_bound sequent_with_notations.sequent cyclic_notations acyclic_notations 0 ttl replaced_sequents
    with Ttl_exceeded -> (None, true)
