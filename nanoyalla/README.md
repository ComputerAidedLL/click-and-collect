**This is NanoYalla version 1.0.3**

*tested with Coq >= 8.8*

Compile and install using:

    $ ./configure
    $ make
    $ make install

Files description:

  * `nanoll.v`: minimal trusted base defining LL proofs as the `ll` type
  * `macroll.v`: derivable rules for the `ll` type
  * `extractionll.v`: configuration for proof extraction to OCaml