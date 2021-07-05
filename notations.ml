open Raw_sequent

type notations = (string * raw_formula) list;;

exception Json_exception of string;;

let pair_from_json pair_as_json =
    try let pair_as_list = Yojson.Basic.Util.to_list pair_as_json in
        (if List.length pair_as_list <> 2 then raise (Json_exception "a notation pair must be a list of exactly two elements"));
        let notation_name = Yojson.Basic.Util.to_string (List.hd pair_as_list) in
        let raw_formula = json_to_raw_formula (List.nth pair_as_list 1) in
        notation_name, raw_formula
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception "a notation pair must be a list of a string and a formula")

let pair_to_json ?stringify:(stringify=false) pair =
    let notation_name, formula = pair in
    `List [`String notation_name; if stringify then `String (raw_formula_to_ascii false formula) else raw_formula_to_json formula]

let from_json notations_as_json =
    try List.map pair_from_json (Yojson.Basic.Util.to_list notations_as_json)
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception "notations must be a list of notation pair")

let to_json ?stringify:(stringify=false) notations =
    `List (List.map (pair_to_json ~stringify:stringify) notations)

(* VARIABLES *)

let get_variable_names notations =
    List.concat_map (fun (_, f) -> get_variable_names f) notations;;

let replace_in_notations alias raw_formula notations =
    List.map (fun (s, rf) -> s, Raw_sequent.replace_in_raw_formula alias raw_formula rf) notations;;

(* GET CYCLIC / ACYCLIC NOTATIONS *)

let rec position_of_notation notation_name n = function
    | [] -> None
    | (s, _f) :: tail -> if s = notation_name then Some n else position_of_notation notation_name (n+1) tail

let stack_variable notations stack notation_position variable =
    match position_of_notation variable 0 notations with
    | Some n -> Stack.push (notation_position, n) stack
    | None -> ()

let get_affected_indices notations matrix variable =
    let all_indices = List.init (List.length notations) (fun i -> i) in
    match position_of_notation variable 0 notations with
    | Some n -> n :: List.filter (fun i -> matrix.(n).(i)) all_indices
    | None -> []

let stack_variables notations stack notation =
    let notation_name, raw_formula = notation in
    let notation_position =
        match position_of_notation notation_name 0 notations with
        | Some n -> n
        | None -> raise (Failure "Notation not found") in
    let variable_names = get_unique_variable_names [raw_formula] in
    List.iter (stack_variable notations stack notation_position) variable_names

(* Get a notation list and returns two lists of notations: *)
(* Notations which contain at least one recursive definition (called "cyclic_notations") *)
(* Notations which does not contain any recursive definition, sorted in a way that occurences precede definitions. *)
(* Example: [A:=B, B:=A+C, C:=D, E:=C, D:=F] will return [A:=B, B:=A+C] (cyclic) and [E:=C, C:=D, D:=F] (sorted acyclic) *)
let split_cyclic_acyclic notations variables =
    (* Init a matrix n x n with false *)
    (* matrix.(x).(y) will be true iff there is a path from x to y *)
    let n = List.length notations in
    let matrix = Array.make_matrix n n false in

    (* Init a stack with all edges *)
    (* We add an edge (x,y) if y appears in the definition of x *)
    let stack = Stack.create () in
    List.iter (stack_variables notations stack) notations;

    while not (Stack.is_empty stack) do
        let x, y = Stack.pop stack in
        if not matrix.(x).(y) then
            matrix.(x).(y) <- true;
            for z = 0 to (n-1) do
                (* For all z, we check if there is a path x -> y -> z *)
                (if matrix.(y).(z) && not matrix.(x).(z) then Stack.push (x, z) stack);
                (* For all z, we check if there is a path z -> x -> y *)
                (if matrix.(z).(x) && not matrix.(z).(y) then Stack.push (z, y) stack);
            done
    done;

    let affected_indices = match variables with
        (* If no variable list is given, we don't filter notations *)
        | None -> List.init n (fun x -> x)
        (* We filter only notations that appear, directly or indirectly, in variable list *)
        | Some variable_list -> List.sort_uniq compare (List.concat_map (get_affected_indices notations matrix) variable_list) in

    let cyclic_indices = List.filter (fun x -> matrix.(x).(x)) affected_indices in
    let acyclic_indices = List.filter (fun x -> not matrix.(x).(x)) affected_indices in
    let sorted_acyclic_indices = List.sort (fun x y -> if matrix.(x).(y) then -1 else 1) acyclic_indices in
    let cyclic_notations = List.map (List.nth notations) cyclic_indices in
    let sorted_acyclic_notations = List.map (List.nth notations) sorted_acyclic_indices in
    cyclic_notations, sorted_acyclic_notations
