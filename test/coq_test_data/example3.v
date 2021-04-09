From NanoYalla Require Import macroll.

Section TheProof.

Variable A B C : formula.

Goal ll [C; dual B] -> ll [wn (aplus (dual A) (dual B)); tens (oc A) (oc C)].
Proof.
intros Hyp0.
apply (co_r_ext []).
apply (ex_perm_r [2;0;1]
                 [wn (aplus (dual A) (dual B)); tens (oc A) (oc C); wn (aplus (dual A) (dual B))]).
apply (tens_r_ext [wn (aplus (dual A) (dual B))] ).
{ apply (oc_r_ext [aplus (dual A) (dual B)] A []).
  apply (de_r_ext []).
  apply (plus_r1_ext []).
  apply ax_r1_ext. }
{ apply (oc_r_ext [] C [aplus (dual A) (dual B)]).
  apply (de_r_ext [C]).
  apply (plus_r2_ext [C]).
  apply Hyp0. }
Qed.

End TheProof.
