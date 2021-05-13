From NanoYalla Require Export macroll.

(* Read-back: get a readable proof from a term [pi : ll l]
     Opaque dual ax_r_ext.
     Eval cbv in pi.
*)

From Coq Require Extraction ExtrOcamlBasic ExtrOcamlNatInt.

Extraction Inline CMorphisms.reflexive_proper CRelationClasses.flip_Reflexive CMorphisms.reflexive_eq_dom_reflexive.
Extraction Inline CRelationClasses.arrow_Reflexive CRelationClasses.arrow_Reflexive_obligation_1.
Extract Inductive nat => int [ "0" "succ" ]
  "(fun fO fS n -> if n = 0 then fO () else fS (n - 1))".
Extraction Inline Nat.add.
Extract Inlined Constant pred => "(fun n -> max 0 (n - 1))".
Extract Inlined Constant Nat.eqb => "(=)".
Extract Inlined Constant Atom => "string".
Extract Inlined Constant app => "List.append".
Extract Inlined Constant map => "List.map".
Extract Inlined Constant length => "List.length".
Extract Inductive formula => "formula"
  [ "Litt" "Dual" "One" "Bottom" "Tensor" "Par" "Zero" "Top" "Plus" "With" "Ofcourse" "Whynot" ].
Extract Inlined Constant dual => "dual".
Extract Inductive ll => "proof"
  [ "(fun a -> Axiom_proof (Litt a))"
    "(fun (l1, l2, a, b, pi) ->
       let n1 = List.length l1 in
       let permutation = List.init n1 (fun k -> k) @ [n1 + 1; n1]
                       @ List.init (List.length l2) (fun k -> n1 + 2 + k) in
       Exchange_proof (l1 @ [a; b] @ l2, permutation, permutation, pi))"
    "One_proof"
    "(fun (l, pi) -> Bottom_proof ([], l, pi))"
    "(fun (a, b, l1, l2, pi1, pi2) -> Tensor_proof (l1, a, b, l2, pi1, pi2))"
    "(fun (a, b, l, pi) -> Par_proof ([], a, b, l, pi))"
    "(fun l -> Top_proof ([], l))"
    "(fun (a, b, l, pi) -> Plus_left_proof ([], a, b, l, pi))"
    "(fun (a, b, l, pi) -> Plus_right_proof ([], a, b, l, pi))"
    "(fun (a, b, l, pi1, pi2) -> With_proof ([], a, b, l, pi1, pi2))"
    "(fun (a, l, pi) -> Promotion_proof ([], a, l, pi))"
    "(fun (a, l, pi) -> Dereliction_proof ([], a, l, pi))"
    "(fun (a, l, pi) -> Weakening_proof ([], a, l, pi))"
    "(fun (a, l, pi) -> Contraction_proof ([], a, l, pi))" ].
Extraction Inline one_r_ext.

(* Extraction: get an OCaml term from a term [pi : ll l]
     Extraction "file.ml" pi.
*)
