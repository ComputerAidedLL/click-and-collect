open Sequent
open Notations

type proof_with_notations = {sequent: sequent; notations: notations}

exception Json_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let from_json sequent_with_notations_as_json =
    try
        let notations_as_json = get_key sequent_with_notations_as_json "notations" in
        let notations = Notations.from_json notations_as_json in
        let sequent_as_json = get_key sequent_with_notations_as_json "sequent" in
        let sequent = Raw_sequent.sequent_from_json sequent_as_json in
        {sequent=sequent; notations=notations}
    with Raw_sequent.Json_exception m -> raise (Json_exception ("bad sequent json: " ^ m))
         | Notations.Json_exception m -> raise (Json_exception ("bad notations: " ^ m))

(* GET CYCLIC / ACYCLIC NOTATIONS *)

let rec position_of_notation notation_name n = function
    | [] -> None
    | (s, f) :: tail -> if s = notation_name then Some n else position_of_notation notation_name (n+1) tail

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
    let notation_name, formula = notation in
    let notation_position =
        match position_of_notation notation_name 0 notations with
        | Some n -> n
        | None -> raise (Failure "Notation not found") in
    let variable_names = get_unique_variable_names [formula] in
    List.iter (stack_variable notations stack notation_position) variable_names

(* Get notations and a sequent and returns two lists of notations: *)
(* Notations which contain at least one recursive definition (called "cyclic_notations") *)
(* Notations which does not contain any recursive definition, sorted in a way that occurences precede definitions. *)
(* Example: [A:=B, B:=A+C, C:=D, E:=C, D:=F] will return [A:=B, B:=A+C] (cyclic) and [E:=C, C:=D, D:=F] (sorted acyclic) *)
let split_cyclic_acyclic sequent_with_notations =
    let notations = sequent_with_notations.notations in

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

    (* We get sequent variable names *)
    let sequent_variable_names = Sequent.get_unique_variable_names sequent_with_notations.sequent in
    (* We filter only notations that appear in sequent *)
    let affected_indices = List.concat (List.map (get_affected_indices notations matrix) sequent_variable_names) in

    let cyclic_indices = List.filter (fun x -> matrix.(x).(x)) affected_indices in
    let acyclic_indices = List.filter (fun x -> not matrix.(x).(x)) affected_indices in
    let sorted_acyclic_indices = List.sort (fun x y -> if matrix.(x).(y) then -1 else 1) acyclic_indices in
    let cyclic_notations = List.map (List.nth notations) cyclic_indices in
    let sorted_acyclic_notations = List.map (List.nth notations) sorted_acyclic_indices in
    cyclic_notations, sorted_acyclic_notations

let rec replace_all_notations_in_sequent sequent = function
    | [] -> sequent
    | (s, formula) :: tail -> replace_all_notations_in_sequent (replace_in_sequent (Litt s) formula (replace_in_sequent (Dual s) (dual formula) sequent)) tail