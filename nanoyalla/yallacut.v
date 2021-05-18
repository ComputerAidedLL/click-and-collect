From Yalla Require Import List_more Permutation_Type_more ll_fragments.
From NanoYalla Require Export macroll.

Lemma ex_Permutation_Type l1 l2 : Permutation_Type l1 l2 -> ll l1 -> ll l2.
Proof.
now apply (Permutation_Type_rect_transp (fun l1 l2 => ll l1 -> ll l2));
  intuition (apply ex_t_r).
Qed.

Fixpoint nll2ll A :=
match A with
| var x     => formulas.var x
| covar x   => formulas.covar x
| one       => formulas.one
| bot       => formulas.bot
| tens A B  => formulas.tens (nll2ll B) (nll2ll A)
| parr A B  => formulas.parr (nll2ll A) (nll2ll B)
| zero      => formulas.zero
| top       => formulas.top
| awith A B => formulas.awith (nll2ll A) (nll2ll B)
| aplus A B => formulas.aplus (nll2ll A) (nll2ll B)
| oc A      => formulas.oc (nll2ll A)
| wn A      => formulas.wn (nll2ll A)
end.

Lemma nll2ll_dual A : formulas.dual (nll2ll A) = nll2ll (dual A).
Proof.
induction A as [ | | | | ? IHA1 ? IHA2 | ? IHA1 ? IHA2 |
                       | | ? IHA1 ? IHA2 | ? IHA1 ? IHA2 | ? IHA1 | ? IHA1 ]; cbn;
rewrite ? IHA1, ? IHA2; reflexivity.
Qed.

Lemma nll2ll_map_wn l : map nll2ll (map wn l) = map formulas.wn (map nll2ll l).
Proof.
induction l as [|? ? IHl]; trivial.
now cbn; rewrite IHl.
Qed.

Lemma nll2ll_map_wn_inv l1 l2 : map formulas.wn l1 = map nll2ll l2 ->
  { l2' | l2 = map wn l2' /\ l1 = map nll2ll l2' }.
Proof.
revert l2; induction l1 as [|a l1 IHl1]; intros [|b l2] Heq;
  inversion Heq as [[Heqf Heq']].
- now exists nil.
- apply IHl1 in Heq' as [l2' [? ?]]; subst.
  destruct b as [ | | | | | | | | | | | b ]; inversion_clear Heqf.
  now exists (b :: l2').
Qed.

Lemma nll2llfrag l : ll l -> ll_fragments.ll_ll (map nll2ll l).
Proof.
intros pi.
induction pi as [ | l1 l2 A B IH | | | A B l1 l2 ? IH1 ? IH2 | | | | | | | | | ];
  try (now constructor); rewrite ? map_app.
- apply (ll_def.ex_r _ (map nll2ll (l1 ++ A :: B :: l2))); [assumption | ].
  rewrite map_app.
  apply Permutation_Type_app_head, Permutation_Type_swap.
- cbn; rewrite map_app.
  apply (ll_def.tens_r _ _ _ _ _ IH2 IH1).
- cbn; rewrite nll2ll_map_wn.
  apply ll_def.oc_r.
  rewrite <- nll2ll_map_wn; assumption.
Qed.

Lemma llfrag2nll l : ll_fragments.ll_ll (map nll2ll l) -> ll l.
Proof.
remember (map nll2ll l) as l0 eqn:Heql0.
intros pi; revert l Heql0;
induction pi as [ | l1 l2 pi IH HP | l1 lw lw' l2 pi IH HP | Hf | Hf | | l1 pi IH
                | A B l1 l2 pi1 IH1 pi2 IH2 | A B l1 pi IH |
                | A B l1 pi IH | A B l1 pi IH | A B l1 pi1 IH1 pi2 IH2
                | A l1 pi IH | A l1 pi IH | A l1 pi IH | A l1 pi IH | Hf | Hf ];
intros l' Heql0; subst; try (inversion Hf; fail).
- destruct l' as [|C l']; inversion Heql0 as [[H1 H2]].
  destruct l' as [|D l']; inversion H2 as [[H3 _]].
  destruct l'; inversion H3 as [[H4 _]].
  destruct C; inversion H1.
  destruct D; inversion H4; subst.
  apply ax_r.
- apply Permutation_Type_map_inv in HP as [l'' Heq HP].
  apply Permutation_Type_sym in HP.
  now apply (ex_Permutation_Type _ _ HP), IH.
- change map with List.map in Heql0.
  symmetry in Heql0; decomp_map_inf Heql0; subst; symmetry in Heql0.
  cbn in Heql0; apply nll2ll_map_wn_inv in Heql0; destruct Heql0 as [l [-> ->]].
  apply Permutation_Type_map_inv in HP as [l' -> HP].
  apply (ex_Permutation_Type (l0 ++ map wn l' ++ l5));
    [ | now apply IH; rewrite <- nll2ll_map_wn, <- ? map_app ].
  apply Permutation_Type_app_head, Permutation_Type_app_tail.
  now apply Permutation_Type_map.
- destruct l' as [|C l']; inversion Heql0 as [[H1 Hn]].
  destruct C; inversion H1.
  destruct l'; inversion Hn.
  apply one_r.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1.
  now apply bot_r, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 H2]].
  destruct C; inversion H1; subst.
  change map with List.map in H2; symmetry in H2; decomp_map_inf H2; subst.
  now apply tens_r; [ apply IH2 | apply IH1 ].
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply parr_r, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1.
  apply top_r.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply plus_r1, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply plus_r2, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply with_r; [ apply IH1 | apply IH2 ].
- destruct l' as [|C l']; inversion Heql0 as [[H1 H2]].
  destruct C; inversion H1; subst.
  apply nll2ll_map_wn_inv in H2 as [l'' [-> ->]].
  apply oc_r, IH.
  now cbn; rewrite nll2ll_map_wn.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply de_r, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply wk_r, IH.
- destruct l' as [|C l']; inversion Heql0 as [[H1 ->]].
  destruct C; inversion H1; subst.
  now apply co_r, IH.
Qed.

Lemma cut_r A l1 l2 : ll (A :: l1) -> ll (dual A :: l2) -> ll (l1 ++ l2).
Proof.
intros pi1 pi2.
apply llfrag2nll.
rewrite map_app.
eapply ll_cut.cut_r_axfree.
- intros a ; destruct a.
- apply nll2llfrag in pi2.
  cbn in pi2; rewrite <- nll2ll_dual in pi2; eassumption.
- apply nll2llfrag in pi1; eassumption.
Qed.
