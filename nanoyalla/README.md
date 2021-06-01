# This is NanoYalla version 1.1.2

Two different installations are possible which differ only on the treatement of cuts.
If you have the [Yalla library v2.0.2](https://perso.ens-lyon.fr/olivier.laurent/yalla/) installed, you can use the *Yalla installation* below.
If you do not want to rely on Yalla, you can use the *Yalla-free installation* which provides cut admissibility as an axiom.

## Yalla Installation

*requires Yalla v2.0.2*

    $ ./configure yalla
    $ make
    $ make install

## Yalla-free Installation

*tested with Coq >= 8.10*

    $ ./configure
    $ make
    $ make install

## Files description

  * `nanoll.v`: minimal trusted base defining LL proofs as the `ll` type
  * `macroll.v`: derivable rules for the `ll` type
  * `axiomcut.v`: axiom-based version of `cut_r`
  * `yallacut.v`: `cut_r` admissibility based on Yalla
  * `macrollcut.v`: derivable rules for the `ll` type with cut
