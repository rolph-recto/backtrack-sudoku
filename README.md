## Sudoku solver

A backtracking sudoku solver with constraint propagation. Inspired by
Peter Norvig's post (http://norvig.com/sudoku.html), though I haven't
implemented everything he talked about.

Mostly written to flex my `Reader` monad knowledge. Note the awkward use
of `EitherT` to break out of a loop in `solveBoard` -- there's probably
a more functional way of doing so, but alas I haven't found it (I think
using `Cont` would be as ungainly, but I'll have to read up on CPS first
and try to reimplement later...)

