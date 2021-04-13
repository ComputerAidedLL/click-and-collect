From Coq Require Export List.
From Coq Require Import PeanoNat Lia.
From NanoYalla Require Export nanoll.

Export ListNotations.


(** * Permutations *)

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

(* Apply list of transpositions described as a [list (option nat)]:
   [Some k0;...; Some kn] means (n, n + S kn) o ... o (0, S k0)
   and None is "no permutation at this position" *)
Fixpoint transpL {A} s (l : list A) :=
  match s with
  | Some k :: s => match (transp 0 k l) with
                   | x :: r => x :: transpL s r
                   | nil => l
                   end
  | None :: s => match l with
                 | x :: r => x :: transpL s r
                 | nil => l
                 end
  | nil => l
  end.

(* Transparent version of [map_length] *)
Lemma map_length_transparent {A B} (f : A -> B) l : length (map f l) = length l.
Proof. induction l; cbn; auto. Defined.

(* Map a permutation of n elements
   described as a [list nat] of distinct elements between 0 and n-1
   into a list of transpositions as used by [transpL] *)
Definition permL_of_perm : list nat -> list (option nat).
Proof.
intros p.
remember (length p) as n eqn:Heqn.
revert p Heqn; induction n as [|n IHn]; intros p Heqn.
- exact nil.
- destruct p as [|[|x] p]; inversion Heqn as [Hn].
  + rewrite <- (map_length_transparent pred) in Hn.
    exact (None :: IHn _ Hn).
  + rewrite <- (map_length_transparent (fun k => if k =? 0 then x else pred k) p) in Hn.
    exact (Some x :: IHn _ Hn).
Defined.

(* Properties *)

Lemma transpS_lt {A} n (l : list A) : S n < length l ->
 {'(l1,l2,a,b) | (l = l1 ++ a :: b :: l2 /\ length l1 = n) & transpS n l = l1 ++ b :: a :: l2}.
Proof.
revert l; induction n as [|n IHn]; cbn; intros l Hl.
- destruct l as [|a [|b l]]; cbn in Hl; try (exfalso; lia).
  now exists ([],l,a,b).
- destruct l as [|a l]; cbn in Hl; try (exfalso; lia).
  destruct (IHn l) as [[[[l1 l2] b] c] [-> Hl1] ->]; [lia|].
  now exists (a :: l1, l2, b, c); split; cbn; subst.
Defined.

Lemma transpS_overflow {A} n (l : list A) :
  length l <= S n -> transpS n l = l.
Proof.
revert l; induction n as [|n IHn]; cbn; intros l Hl.
- destruct l as [|a [|b l]]; auto.
  cbn in Hl; lia.
- destruct l as [|a l]; auto.
  now rewrite <- (IHn l) at 2; [|cbn in Hl; lia].
Defined.

Lemma transpS_compute {A} l1 (a b : A) l2 :
  transpS (length l1) (l1 ++ a :: b :: l2) = l1 ++ b :: a :: l2.
Proof.
destruct (transpS_lt (length l1) (l1 ++ a :: b :: l2)) as [[[[l1' l2'] c] d] [Heq Hl] ->].
- rewrite app_length; cbn; lia.
- assert (l1 = l1') as ->.
  { revert l1' Heq Hl; induction l1 as [|h l1 IHl1]; cbn; intros l1' Heq Hl.
    - now apply length_zero_iff_nil in Hl.
    - destruct l1'; inversion Hl; subst.
      inversion Heq; subst; f_equal.
      apply IHl1; auto. }
  f_equal.
  apply app_inv_head in Heq.
  now inversion Heq; subst.
Defined.

Lemma transp_cons {A} a b x (l : list A) :
  transp (S a) b (x :: l) = x :: transp a b l.
Proof.
revert a l; induction b as [|b IHb]; intros a l; auto.
now cbn; rewrite (IHb (S a)).
Defined.

Lemma transp_app_tl {A} l0 a b (l : list A) :
  transp (length l0 + a) b (l0 ++ l) = l0 ++ transp a b l.
Proof.
revert a l; induction l0 as [|x l0 IHl0]; cbn; intros a l; auto.
now rewrite transp_cons, <- IHl0.
Defined.

(* Extended exchange rules *)

Lemma ex_transpS n l : ll l -> ll (transpS n l).
Proof.
intros pi.
destruct (Compare_dec.le_lt_dec (length l) (S n)) as [Hle|Hlt].
- now rewrite transpS_overflow.
- apply transpS_lt in Hlt as [[[[l1 l2] b] c] [-> _] ->].
  now apply ex_t_r.
Defined.

Lemma ex_transp n m l : ll l -> ll (transp n m l).
Proof.
revert n l; induction m as [|m IHm]; intros n l pi.
- now apply ex_transpS.
- now apply ex_transpS, IHm, ex_transpS.
Defined.

Lemma ex_transp_middle1 l1 l2 l3 A :
  ll (l1 ++ A :: l2 ++ l3) -> ll (l1 ++ l2 ++ A :: l3).
Proof.
revert l1; induction l2 as [|a l2 IHl2]; cbn; intros l1; auto.
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
revert l1; induction l2 as [|a l2 IHl2]; cbn; intros l1; auto.
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
enough (forall l0, ll (l0 ++ l) -> ll (l0 ++ transpL s l)) as Hs
  by now intros pi; apply (Hs []).
revert l; induction s as [|[n|] s IHs]; cbn; intros l l0 pi; auto.
- remember (transp 0 n l) as lt eqn:Heqlt.
  destruct lt as [|f lt]; auto.
  replace (l0 ++ f :: transpL s lt) with ((l0 ++ f :: nil) ++ transpL s lt)
    by now rewrite <- app_assoc.
  apply IHs.
  rewrite <- app_assoc; cbn.
  rewrite Heqlt, <- transp_app_tl.
  now apply ex_transp.
- destruct l as [|f l]; auto.
  replace (l0 ++ f :: transpL s l) with ((l0 ++ f :: nil) ++ transpL s l)
    by now rewrite <- app_assoc.
  apply IHs.
  now rewrite <- app_assoc.
Defined.


(** * Extended rules *)

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

Lemma bidual A : dual (dual A) = A.
Proof.
induction A as [ X | X
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | A1 IHA1 | A1 IHA1 ];
cbn; rewrite ?IHA1, ?IHA2; reflexivity.
Defined.

Lemma ax_r_ext A : ll [dual A; A].
Proof.
induction A as [ X | X
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | A1 IHA1 | A1 IHA1 ]; cbn.
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
  change (ll (oc A1 :: map wn [dual A1])).
  apply oc_r; cbn.
  apply (ex_t_r [] []).
  now apply de_r.
- change (ll (oc (dual A1) :: map wn [A1])).
  apply oc_r; cbn.
  now apply (ex_t_r [] []), de_r, (ex_t_r [] []).
Defined.

Ltac ax_expansion :=
  now cbn;
  let Hd := fresh "Hd" in
  match goal with
  | |- ll (?A :: ?B :: nil) =>
         assert (A = dual B) as Hd by (now cbn; rewrite ?bidual);
         (try rewrite Hd at 1); apply ax_r_ext
  end.

Definition ex_perm_r p := ex_transpL (permL_of_perm p).

Definition one_r_ext := one_r.

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

Lemma tens_r_ext l1 A B l2 :
  ll (l1 ++ [A]) -> ll (B :: l2) -> ll (l1 ++ tens A B :: l2).
Proof.
intros pi1 pi2.
apply (ex_transp_middle1 []); cbn.
apply tens_r; auto.
now apply (ex_transp_middle2 []) in pi1; rewrite app_nil_r in pi1.
Defined.

Lemma parr_r_ext l1 A B l2 :
  ll (l1 ++ A :: B :: l2) -> ll (l1 ++ parr A B :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply parr_r.
now apply (ex_transp_middle2 [A]), (ex_transp_middle2 [] l1).
Defined.

Lemma with_r_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ B :: l2) -> ll (l1 ++ awith A B :: l2).
Proof.
intros pi1 pi2.
apply (ex_transp_middle1 []); cbn.
now apply with_r; apply (ex_transp_middle2 []).
Defined.

Lemma plus_r1_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus A B :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply plus_r1.
now apply (ex_transp_middle2 []).
Defined.

Lemma plus_r2_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus B A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply plus_r2.
now apply (ex_transp_middle2 []).
Defined.

Lemma oc_r_ext l1 A l2 :
  ll (map wn l1 ++ A :: map wn l2) -> ll (map wn l1 ++ oc A :: map wn l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
rewrite <- map_app; apply oc_r; rewrite map_app.
now apply (ex_transp_middle2 []).
Defined.

Lemma de_r_ext l1 A l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply de_r.
now apply (ex_transp_middle2 []).
Defined.

Lemma wk_r_ext l1 A l2 :
  ll (l1 ++ l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
now apply wk_r.
Defined.

Lemma co_r_ext l1 A l2 :
  ll (l1 ++ wn A :: wn A :: l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 []); cbn.
apply co_r.
now apply (ex_transp_middle2 [wn A]), (ex_transp_middle2 [] l1).
Defined.
