two-hour-tetris
===============

At Brown, there are three intro CS sequences, CS 15 (which teaches Java), CS 17 (Racket, OCaml), and CS 19 (Racket). These are (in theory) in order of difficulty.

To demonstrate to my friend in CS 15 why I think it's a bad course, I wrote this Tetris in two hours while she was working on hers, which is a two week project in CS 15. The functionaliy is not entirely equivalent, but it is pretty close, and I chose to leave the code as is to demonstrate how easy it is to build simple games in Racket.

plai-typed
----------
This project is written in the `plai-typed` dialect of Racket, which was what we used in CS 19. It has a simple ML-like type system with Hindley-Milner type inference. To install `plai-typed`, run:

    raco pkg install plai-typed

This is also a good example for how to use `big-bag` in `plai-typed`. It requires a fair amount of type hackery in the imports.
