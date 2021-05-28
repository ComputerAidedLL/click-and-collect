From NanoYalla Require Import macrollcut.

Section TheProof.

Variable A B : formula.
Variable X Y : Atom.

Goal ll [tens A (dual B); parr (dual A) B].
Proof. ax_expansion. Qed.

Goal ll [parr (dual A) B; tens A (dual B)].
Proof. ax_expansion. Qed.

Goal ll [tens (dual A) (dual B); parr A B].
Proof. ax_expansion. Qed.

Goal ll [parr A B; tens (dual A) (dual B)].
Proof. ax_expansion. Qed.

Goal ll [covar X; var X].
Proof. ax_expansion. Qed.

Goal ll [var X; covar X].
Proof. ax_expansion. Qed.

Goal ll [tens (covar X) (var Y); parr (var X) (covar Y)].
Proof. ax_expansion. Qed.

End TheProof.
