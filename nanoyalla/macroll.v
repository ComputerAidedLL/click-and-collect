From Coq Require Import Lia.
From NanoYalla Require Export nanoll.

Export List.ListNotations.

(** * Informative / Transparent versions of list operations *)

Lemma app_assoc_inf {A} (l m n : list A) : l ++ m ++ n = (l ++ m) ++ n.
Proof. now induction l; cbn; f_equal. Defined.

Lemma map_length_inf {A B} (f : A -> B) l : length (map f l) = length l.
Proof. induction l; cbn; auto. Defined.


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
  | Some k :: s => match transp 0 k l with
                   | x :: r => x :: transpL s r
                   | nil => l
                   end
  | None :: s => match l with
                 | x :: r => x :: transpL s r
                 | nil => l
                 end
  | nil => l
  end.

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
  + rewrite <- (map_length_inf pred) in Hn.
    exact (None :: IHn _ Hn).
  + rewrite <- (map_length_inf (fun k => if Nat.eqb k 0 then x else pred k) p) in Hn.
    exact (Some x :: IHn _ Hn).
Defined.

(* Properties *)

Lemma transpS_lt {A} n (l : list A) : S n < length l ->
 {'(l1, l2, a, b) | (l = l1 ++ a :: b :: l2 /\ length l1 = n) & transpS n l = l1 ++ b :: a :: l2}.
Proof.
revert l; induction n as [|n IHn]; cbn; intros l Hl.
- destruct l as [|a [|b l]]; cbn in Hl; try (now exfalso; inversion Hl).
  now exists (nil, l, a, b).
- destruct l as [|a l]; cbn in Hl; try (now exfalso; inversion Hl).
  destruct (IHn l) as [[[[l1 l2] b] c] [-> Hl1] ->]; [lia|].
  now exists (a :: l1, l2, b, c); split; cbn; subst.
Defined.

Lemma transpS_overflow {A} n (l : list A) :
  length l <= S n -> transpS n l = l.
Proof.
revert l; induction n as [|n IHn]; cbn; intros l Hl.
- destruct l as [|a [|b l]]; auto.
  now exfalso; inversion Hl.
- destruct l as [|a l]; auto.
  now rewrite <- (IHn l) at 2; [|cbn in Hl; lia].
Defined.

Lemma transpS_compute {A} l1 (a b : A) l2 :
  transpS (length l1) (l1 ++ a :: b :: l2) = l1 ++ b :: a :: l2.
Proof.
destruct (transpS_lt (length l1) (l1 ++ a :: b :: l2)) as [[[[l1' l2'] c] d] [Heq Hl] ->].
- induction l1; cbn; lia.
- revert l1' Heq Hl; induction l1 as [|h l1 IHl1]; cbn; intros [|a' l1'] Heq Hl; inversion Heq;
    try easy.
  inversion Hl; subst.
  now cbn; f_equal; apply IHl1.
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
   with ((l1 ++ a :: nil) ++ l2 ++ A :: l3)
  by now rewrite <- app_assoc_inf.
apply IHl2; rewrite <- app_assoc_inf; cbn.
rewrite <- transpS_compute.
now apply (ex_transpS (length l1)).
Defined.

Lemma ex_transp_middle2 l1 l2 l3 A :
  ll (l1 ++ l2 ++ A :: l3) -> ll (l1 ++ A :: l2 ++ l3).
Proof.
revert l1; induction l2 as [|a l2 IHl2]; cbn; intros l1; auto.
intros pi.
replace (l1 ++ a :: l2 ++ A :: l3)
   with ((l1 ++ a :: nil) ++ l2 ++ A :: l3) in pi
  by now rewrite <- app_assoc_inf.
apply IHl2 in pi; rewrite <- app_assoc_inf in pi; cbn in pi.
rewrite <- transpS_compute.
now apply (ex_transpS (length l1)).
Defined.

Lemma ex_transpL s l : ll l -> ll (transpL s l).
Proof.
enough (forall l0, ll (l0 ++ l) -> ll (l0 ++ transpL s l)) as Hs
  by now intros pi; apply (Hs nil).
revert l; induction s as [|[n|] s IHs]; cbn; intros l l0 pi; auto.
- remember (transp 0 n l) as lt eqn:Heqlt.
  destruct lt as [|f lt]; auto.
  replace (l0 ++ f :: transpL s lt) with ((l0 ++ f :: nil) ++ transpL s lt)
    by now rewrite <- app_assoc_inf.
  apply IHs.
  rewrite <- app_assoc_inf; cbn.
  rewrite Heqlt, <- transp_app_tl.
  now apply ex_transp.
- destruct l as [|f l]; auto.
  replace (l0 ++ f :: transpL s l) with ((l0 ++ f :: nil) ++ transpL s l)
    by now rewrite <- app_assoc_inf.
  apply IHs.
  now rewrite <- app_assoc_inf.
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

Definition ex_perm_r p := ex_transpL (permL_of_perm p).

Lemma ax_r_ext A : ll (dual A :: A :: nil).
Proof.
induction A as [ X | X
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | | | A1 IHA1 A2 IHA2|A1 IHA1 A2 IHA2
               | A1 IHA1 | A1 IHA1 ]; cbn.
- apply ax_r.
- apply (ex_t_r nil), ax_r.
- apply bot_r, one_r.
- apply (ex_t_r nil), bot_r, one_r.
- apply parr_r.
  apply (ex_t_r (dual A1 :: nil)).
  apply (ex_t_r nil).
  change (ll (tens A1 A2 :: (dual A1 :: nil) ++ dual A2 :: nil)).
  now apply tens_r; apply (ex_t_r nil).
- apply (ex_t_r nil).
  apply parr_r.
  apply (ex_t_r (A1 :: nil)).
  apply (ex_t_r nil).
  change (ll (tens (dual A1) (dual A2) :: (A1 :: nil) ++ A2 :: nil)).
  now apply tens_r.
- apply top_r.
- apply (ex_t_r nil), top_r.
- apply with_r; apply (ex_t_r nil).
  + now apply plus_r1, (ex_t_r nil).
  + now apply plus_r2, (ex_t_r nil).
- apply (ex_t_r nil); apply with_r; apply (ex_t_r nil).
  + now apply plus_r1.
  + now apply plus_r2.
- apply (ex_t_r nil).
  change (ll (oc A1 :: map wn (dual A1 :: nil))).
  apply oc_r; cbn.
  apply (ex_t_r nil).
  now apply de_r.
- change (ll (oc (dual A1) :: map wn (A1 :: nil))).
  apply oc_r; cbn.
  now apply (ex_t_r nil), de_r, (ex_t_r nil).
Defined.

Ltac ax_expansion :=
  now cbn;
  let Hd := fresh "Hd" in
  match goal with
  | |- ll (?A :: ?B :: nil) =>
         apply (ax_r_ext B) ||
         apply (ex_perm_r (1 :: 0 :: nil)%nat (B :: A :: nil)), (ax_r_ext A) ||
         (assert (A = dual B) as Hd by (now cbn; rewrite ?bidual);
          (try rewrite Hd at 1); apply ax_r_ext)
  end.

Definition one_r_ext := one_r.

Lemma bot_r_ext l1 l2 : ll (l1 ++ l2) -> ll (l1 ++ bot :: l2).
Proof. now intros; apply (ex_transp_middle1 nil), bot_r. Defined.

Lemma top_r_ext l1 l2 : ll (l1 ++ top :: l2).
Proof. now apply (ex_transp_middle1 nil), top_r. Defined.

Lemma tens_r_ext l1 A B l2 :
  ll (l1 ++ A :: nil) -> ll (B :: l2) -> ll (l1 ++ tens A B :: l2).
Proof.
intros pi1 pi2.
apply (ex_transp_middle1 nil); cbn.
apply tens_r; auto.
apply (ex_transp_middle2 nil) in pi1.
replace (l1 ++ nil) with l1 in pi1; trivial.
induction l1; intuition.
Defined.

Lemma parr_r_ext l1 A B l2 :
  ll (l1 ++ A :: B :: l2) -> ll (l1 ++ parr A B :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 nil), parr_r.
now apply (ex_transp_middle2 (A :: nil)); cbn; apply (ex_transp_middle2 nil).
Defined.

Lemma with_r_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ B :: l2) -> ll (l1 ++ awith A B :: l2).
Proof.
now intros; apply (ex_transp_middle1 nil), with_r; apply (ex_transp_middle2 nil).
Defined.

Lemma plus_r1_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus A B :: l2).
Proof. now intros; apply (ex_transp_middle1 nil), plus_r1, (ex_transp_middle2 nil). Defined.

Lemma plus_r2_ext l1 A B l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ aplus B A :: l2).
Proof. now intros; apply (ex_transp_middle1 nil), plus_r2, (ex_transp_middle2 nil). Defined.

Lemma oc_r_ext l1 A l2 :
  ll (map wn l1 ++ A :: map wn l2) -> ll (map wn l1 ++ oc A :: map wn l2).
Proof.
intros pi.
apply (ex_transp_middle2 nil) in pi.
apply (ex_transp_middle1 nil); cbn.
replace (map wn l1 ++ map wn l2) with (map wn (l1 ++ l2)) in *; [now apply oc_r| ].
now clear; induction l1 as [|? ? IHl1]; cbn; rewrite ? IHl1.
Defined.

Lemma de_r_ext l1 A l2 :
  ll (l1 ++ A :: l2) -> ll (l1 ++ wn A :: l2).
Proof. now intros; apply (ex_transp_middle1 nil), de_r, (ex_transp_middle2 nil). Defined.

Lemma wk_r_ext l1 A l2 :
  ll (l1 ++ l2) -> ll (l1 ++ wn A :: l2).
Proof. now intros; apply (ex_transp_middle1 nil), wk_r. Defined.

Lemma co_r_ext l1 A l2 :
  ll (l1 ++ wn A :: wn A :: l2) -> ll (l1 ++ wn A :: l2).
Proof.
intros pi.
apply (ex_transp_middle1 nil), co_r.
now apply (ex_transp_middle2 (wn A :: nil)), (ex_transp_middle2 nil l1).
Defined.

Declare Scope ll_scope.
Bind Scope ll_scope with formula.
Open Scope ll_scope.

Module LLNotations.
  Notation "⊢" := (ll nil) (format "⊢") : ll_scope.
  Notation "⊢ x" := (ll (cons x nil)) (at level 85) : ll_scope.
  Notation "⊢ x , y , .. , z" := (ll (cons x (cons y .. (cons z nil) ..))) (at level 85) : ll_scope.
  Infix "⊗" := tens (at level 40) : ll_scope.
  Infix "⊕" := aplus (at level 40) : ll_scope.
  Infix "⅋" := parr (at level 40) : ll_scope.
  Infix "＆" := awith (at level 40) : ll_scope.
  Notation "? A" := (wn A) (format "? A", at level 31, right associativity) : ll_scope.
  Notation "! A" := (oc A) (format "! A", at level 31, right associativity) : ll_scope.
  Notation "1" := one : ll_scope.
  Notation "⟂" := bot : ll_scope.
  Notation "0" := zero : ll_scope.
  Notation "⊤" := top : ll_scope.
  Notation "A ^" := (dual A) (at level 12, format "A ^") : ll_scope.
End LLNotations.
