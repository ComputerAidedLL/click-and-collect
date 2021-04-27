open Sequent

(* Balanced additive slices: provability check *)

let prod_map f l1 l2 =
  List.concat (List.map (fun x1 -> List.map (f x1) l2) l1)

let rec slices = function
  | One -> [[[]]]
  | Bottom -> [[[]]]
  | Top -> []
  | Zero -> [[]]
  | Litt x -> [[[(x, true)]]]
  | Dual x -> [[[(x, false)]]]
  | Tensor (f1, f2) -> prod_map (prod_map List.append) (slices f1) (slices f2)
  | Par (f1, f2) -> prod_map (prod_map List.append) (slices f1) (slices f2)
  | With (f1, f2) -> slices f1 @ (slices f2)
  | Plus (f1, f2) -> prod_map List.append (slices f1) (slices f2)
  | Ofcourse f -> slices f
  | Whynot f -> []

let rec remove x = function
  | [] -> raise Not_found
  | h :: tl -> if x = h then tl
               else h :: (remove x tl)

let rec valid_slice = function
  | [] -> true
  | (x, b) :: xs -> try valid_slice (remove (x, not b) xs)
                    with Not_found -> false

let litteral_compare (x1, b1) (x2, b2) =
  let cx = compare x1 x2 in
  if cx = 0 then compare b1 b2 else cx

let sorted_slices f =
  List.map (List.map (List.sort litteral_compare)) (slices f)

let valid_slices f =
  List.for_all (List.exists valid_slice) (sorted_slices f)

let provable_sequent_as_slices sequent =
  valid_slices (sequent_to_formula sequent)
