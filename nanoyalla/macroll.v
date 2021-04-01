From Coq Require Export List.
From Coq Require Import PeanoNat Lia.
Require Export nanoll.

Export ListNotations.


(** * Exchange *)

(* Transpose elements of index n and S n in l *)
Fixpoint transpS {A} n (l : list A) :=
match n, l with
| 0, x :: y :: r => y :: x :: r
| S n, x :: r => x :: transpS n r
| _, r => r
end.

(* Transpose elements of index n and S (n + m) in l *)
Fixpoint transp {A} n m (l : list A) :=
match m with
| 0 => transpS n l
| S m => transpS n (transp (S n) m (transpS n l))
end.

(* Apply list of transpositions *)
Definition transpL {A} s (l : list A) :=
  fold_left (fun l m => transp 0 m l) s l.

Lemma transpS_lt {A} n (l : list A) : S n < length l ->
 {'(l1,l2,a,b) | (l = l1 ++ a :: b :: l2 /\ length l1 = n) & transpS n l = l1 ++ b :: a :: l2}.
Proof.
revert l; induction n; cbn; intros l Hl.
- destruct l as [|a [|b l]]; try (cbn in Hl; lia).
  now exists ([],l,a,b).
- destruct l as [|a l]; cbn in Hl; try lia.
  destruct (IHn l) as [[[[l1 l2] b] c] [-> Hl1] ->]; [lia|].
  now exists (a :: l1, l2, b, c); split; cbn; subst.
Defined.

Lemma transpS_overflow {A} n (l : list A) :
  length l <= S n -> transpS n l = l.
Proof.
revert l; induction n; cbn; intros l Hl.
- destruct l as [|a [|b l]]; auto.
  cbn in Hl; lia.
- destruct l as [|a l]; auto.
  now rewrite <- (IHn l) at 2; [|cbn in Hl; lia].
Defined.

Lemma transpS_compute {A} l1 (a b : A) l2 :
  transpS (length l1) (l1 ++ a :: b :: l2) = l1 ++ b :: a :: l2.
Proof.
destruct (transpS_lt (length l1) (l1 ++ a :: b :: l2)) as [[[[l1' l2'] c] d] [H Hl] ->].
- rewrite app_length; cbn; lia.
- assert (l1 = l1') as ->.
  { revert l1' H Hl; induction l1; cbn; intros.
    - now apply length_zero_iff_nil in Hl.
    - destruct l1'; inversion Hl; subst.
      inversion H; subst; f_equal.
      apply IHl1; auto. }
  f_equal.
  apply app_inv_head_iff in H.
  now inversion H; subst.
Defined.

Lemma transp_compute {A} l1 (a : A) l2 b l3 :
  transp (length l1) (length l2) (l1 ++ a :: l2 ++ b :: l3) = l1 ++ b :: l2 ++ a :: l3.
Proof.
remember (length l2) as m.
revert a b l1 l2 Heqm; induction m; cbn; intros a b l1 l2 Heqm; subst.
- symmetry in Heqm.
  apply length_zero_iff_nil in Heqm; subst; cbn.
  apply transpS_compute.
- destruct l2; [cbn in Heqm; lia|].
  cbn; rewrite transpS_compute.
  cbn in Heqm; injection Heqm; intros Hl2.
  replace (l1 ++ a0 :: a :: l2 ++ b :: l3)
     with ((l1 ++ [a0]) ++ a :: l2 ++ b :: l3)
    by now rewrite <- app_assoc.
  specialize (IHm a b (l1 ++ [a0]) _ Hl2).
  rewrite app_length in IHm.
  rewrite Nat.add_comm in IHm.
  cbn in IHm; rewrite IHm.
  rewrite <- app_assoc; cbn.
  apply transpS_compute.
Defined.

Lemma ex_transpS n l : ll l -> ll (transpS n l).
Proof.
intros pi.
destruct (Compare_dec.le_lt_dec (length l) (S n)).
- now rewrite transpS_overflow.
- apply transpS_lt in l0 as [[[[l1 l2] b] c] [-> _] ->].
  now apply (ex_t_r l1 l2 b c).
Defined.

Lemma ex_transp n m l : ll l -> ll (transp n m l).
Proof.
revert n l; induction m; intros n l pi.
- now apply ex_transpS.
- now apply ex_transpS, IHm, ex_transpS.
Defined.

Lemma ex_transp_middle1 l1 l2 l3 A :
  ll (l1 ++ A :: l2 ++ l3) -> ll (l1 ++ l2 ++ A :: l3).
Proof.
revert l1; induction l2; cbn; intros l1; auto.
intros pi.
replace (l1 ++ a :: l2 ++ A :: l3)
   with ((l1 ++ [a]) ++ l2 ++ A :: l3)
  by now rewrite <- app_assoc.
apply IHl2; rewrite <- app_assoc; cbn.
rewrite <- transpS_compute.
now apply (ex_transpS (length l1)).
Defined.

Lemma ex_transp_middle2 l1 l2 l3 A :
  ll (l1 ++ l2 ++ A :: l3) -> ll (l1 ++ A :: l2 ++ l3).
Proof.
revert l1; induction l2; cbn; intros l1; auto.
intros pi.
replace (l1 ++ a :: l2 ++ A :: l3)
   with ((l1 ++ [a]) ++ l2 ++ A :: l3) in pi
  by now rewrite <- app_assoc.
apply IHl2 in pi; rewrite <- app_assoc in pi; cbn in pi.
rewrite <- transpS_compute.
now apply (ex_transpS (length l1)).
Defined.

Lemma ex_transpL s l : ll l -> ll (transpL s l).
Proof.
revert l; induction s; cbn; intros l pi; auto.
now apply IHs, ex_transp.
Defined.


(** * Axiom Expansion *)

Fixpoint dual A :=
match A with
| var x     => covar x
| covar x   => var x
| one       => bot
| bot       => one
| tens A B  => parr (dual A) (dual B)
| parr A B  => tens (dual A) (dual B)
| zero      => top
| top       => zero
| aplus A B => awith (dual A) (dual B)
| awith A B => aplus (dual A) (dual B)
| oc A      => wn (dual A)
| wn A      => oc (dual A)
end.

Lemma ax_exp A : ll [dual A; A].
Proof.
induction A; cbn.
- apply ax_r.
- apply (ex_t_r [] []), ax_r.
- apply bot_r, one_r.
- apply (ex_t_r [] []), bot_r, one_r.
- apply parr_r.
  apply (ex_t_r [dual A1] []).
  apply (ex_t_r [] [dual A2]).
  change (ll (tens A1 A2 :: [dual A1] ++ [dual A2])).
  now apply tens_r; apply (ex_t_r [] []).
- apply (ex_t_r [] []).
  apply parr_r.
  apply (ex_t_r [A1] []).
  apply (ex_t_r [] [A2]).
  change (ll (tens (dual A1) (dual A2) :: [A1] ++ [A2])).
  now apply tens_r.
- apply top_r.
- apply (ex_t_r [] []), top_r.
- apply with_r; apply (ex_t_r [] []).
  + now apply plus_r1, (ex_t_r [] []).
  + now apply plus_r2, (ex_t_r [] []).
- apply (ex_t_r [] []); apply with_r; apply (ex_t_r [] []).
  + now apply plus_r1.
  + now apply plus_r2.
- apply (ex_t_r [] []).
  change (ll (oc A :: map wn [dual A])).
  apply oc_r; cbn.
  apply (ex_t_r [] []).
  now apply de_r.
- change (ll (oc (dual A) :: map wn [A])).
  apply oc_r; cbn.
  now apply (ex_t_r [] []), de_r, (ex_t_r [] []).
Defined.

Lemma ax_exp2 A : ll [A; dual A].
Proof.
apply (ex_t_r [] []), ax_exp.
Defined.


(** * Extended rules *)

Lemma bot_r_ext l1 l2 :
  ll (l1 ++ l2) -> ll (l1 ++ bot :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
now apply bot_r.
Defined.

Lemma top_r_ext l1 l2 :
  ll (l1 ++ top :: l2).
Proof.
apply (ex_transp_middle1 []); cbn.
now apply top_r.
Defined.

Lemma tens_r_ext A B l1 l2 :
  ll (l1 ++ [A]) -> ll (B :: l2) -> ll (l1 ++ tens A B :: l2).
Proof.
intros pi1 pi2.
apply (ex_transp_middle1 []); cbn.
apply tens_r; auto.
now apply (ex_transp_middle2 []) in pi1; rewrite app_nil_r in pi1.
Defined.

Lemma parr_r_ext A B l1 l2 :
  ll (l1 ++ A :: B :: l2) -> ll (l1 ++ parr A B :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply parr_r.
now apply (ex_transp_middle2 [A]), (ex_transp_middle2 [] l1).
Defined.

Lemma with_r_ext A B l1 l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ B :: l2) -> ll (l1 ++ awith A B :: l2).
Proof.
intros pi1 pi2.
apply (ex_transp_middle1 []); cbn.
now apply with_r; apply (ex_transp_middle2 []).
Defined.

Lemma plus_r1_ext A B l1 l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus A B :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply plus_r1.
now apply (ex_transp_middle2 []).
Defined.

Lemma plus_r2_ext A B l1 l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus B A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply plus_r2.
now apply (ex_transp_middle2 []).
Defined.

Lemma oc_r_ext A l1 l2 :
  ll (map wn l1 ++ A :: map wn l2) -> ll (map wn l1 ++ oc A :: map wn l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
rewrite <- map_app.
apply oc_r.
rewrite map_app.
now apply (ex_transp_middle2 []).
Defined.

Lemma de_r_ext A l1 l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply de_r.
now apply (ex_transp_middle2 []).
Defined.

Lemma wk_r_ext A l1 l2 :
  ll (l1 ++ l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
now apply wk_r.
Defined.

Lemma co_r_ext A l1 l2 :
  ll (l1 ++ wn A :: wn A :: l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply co_r.
now apply (ex_transp_middle2 [wn A]), (ex_transp_middle2 [] l1).
Defined.
