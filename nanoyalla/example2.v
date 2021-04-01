Require Import macroll.

Section TheProof.

Variable A B : formula.

Goal ll [wn (aplus (dual A) (dual B)); tens (oc A) (oc B)].
Proof.
apply co_r.
apply (ex_transp 1 0 ([wn (aplus (dual A) (dual B))] ++ tens (oc A) (oc B) :: [wn (aplus (dual A) (dual B))])).
apply tens_r_ext.
- change (ll (map wn [aplus (dual A) (dual B)] ++ oc A :: map wn nil)).
  apply oc_r_ext; cbn.
  apply de_r.
  apply plus_r1.
  apply ax_exp.
- change (ll (oc B :: map wn [aplus (dual A) (dual B)])).
  apply oc_r.
  apply (de_r_ext _ [_]).
  apply (plus_r2_ext _ _ [_]); cbn.
  apply ax_exp2.
Qed.

End TheProof.
