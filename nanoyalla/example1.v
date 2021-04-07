From NanoYalla Require Import macroll.

Section TheProof.

Variable A B : formula.

Goal ll [wn (aplus (dual A) (dual B)); tens (oc A) (oc B)].
Proof.
apply co_r.
apply (ex_transp 0 1 (tens (oc A) (oc B) :: [wn (aplus (dual A) (dual B))] ++ [wn (aplus (dual A) (dual B))])).
apply tens_r.
- change (ll (oc A :: map wn [aplus (dual A) (dual B)])).
  apply oc_r.
  apply (ex_t_r [] [] _ _).
  apply de_r.
  apply plus_r1.
  apply ax_exp.
- change (ll (oc B :: map wn [aplus (dual A) (dual B)])).
  apply oc_r.
  apply (ex_t_r [] [] _ _).
  apply de_r.
  apply plus_r2.
  apply ax_exp.
Qed.

End TheProof.
