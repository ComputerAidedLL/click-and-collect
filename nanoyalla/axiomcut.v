From NanoYalla Require Export macroll.

Axiom cut_r : forall A l1 l2, ll (A :: l1) -> ll (dual A :: l2) -> ll (l1 ++ l2).
