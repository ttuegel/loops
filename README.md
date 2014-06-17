loops
==========

[![Build Status](https://travis-ci.org/ttuegel/loops.svg?branch=master)](https://travis-ci.org/ttuegel/loops)

**Academic Summary**

Loops have the structure of a monad. Bind (`>>=`) nests loops and `return x` is
a loop with a single iteration over a value `x`.

**Features**

* Fast, imperative-style loops with a clean syntax. Bind (`>>=`) nests loops, so
  in `do`-notation, each subsequent line is nested inside loops that appear
  above it.
* Iteration over common data structures, like lists and vectors.
* Robust performance because there is no reliance on fusion.
* Loop-unrolling to arbitrary depth. Unrollable loop combinators are
  provided in `Control.Monad.Loop.Unroll`. (The simple, "rolled" interface is
  still provided in `Control.Monad.Loop`.) The unrolling depth set at the call
  site at compile time. My benchmarks show that folding over unrolled loops is
  up to 25% faster than folding over unboxed vectors!
* **NEW!** Arbitrary, named continuations (breakpoints). `breaking` and
  `breaking_` each create a continuation at the current point and pass that
  continuation to a subloop. The named continuation is existentially quantified
  to prevent it from escaping its scope. Only one continuation/breakpoint can be
  active at a time.  `unbreakable` masks continuations, preventing any external
  breakpoints from being invoked in a subloop.

**Performance**

For best performance, please compile your code with `-O2`. You should also use
GHC's LLVM backend if possible; it generally produces faster executables.

A silly example
---------------

At first, the statement that "bind nests loops" may seem strange, but can be
motivated by the `Monad` instance for lists. Consider the following
`do`-notation for a list:

~~~ {.haskell}
module Example where

import Control.Monad.Loop
import Data.Foldable (toList)

-- A list of pairs (i, j) where 0 <= i <= 3 and 0 <= j <= i
nestedList :: [(Int, Int)]
nestedList = do
    i <- [0..3]
    j <- [0..i]
    return (i, j)
~~~

If you're not familiar with this use of lists, load up this file in ghci
with `ghci -isrc -pgmL markdown-unlit README.lhs`. (You need to have
[markdown-unlit](https://github.com/sol/markdown-unlit) installed first.)
Enter `nestedList` at the prompt and see:

~~~
>>> nestedList
[(0,0),(1,0),(1,1),(2,0),(2,1),(2,2),(3,0),(3,1),(3,2),(3,3)]
~~~

Now let's do something really silly: let's build the same list with a
`Loop`!

~~~ {.haskell}
nestedList' :: [(Int, Int)]
nestedList' = toList $ loop $ do  -- 'loop' is just an aid to type inference
    i <- for 0 (<= 3) (+ 1)
    j <- for 0 (<= i) (+ 1)
    return (i, j)
~~~

You would never actually want to do this. This example is simply to
illustrate what "bind nests loops" means in a context most Haskellers are
familiar with.

The correspondence between the list monad and the loop monad is not a
coincidence! GHC uses stream fusion to reduce (some) uses of lists to simple
loops so that the evaluated list is never held in memory. Unfortunately, using
lists as loops is dangerous in performance-sensitive code because the fusion
rules may fail to fire, leaving you with a fully-evaluated list on the heap! A
`Loop` can only evaluate one iteration at a time, so there is no larger data
structure that needs to be fused. Consequently, performance is less fragile.

You might complain that this style of programming does not fit Haskell very
well, but I would contend just the opposite. As I mentioned above, lists are the
more general case of loops: a list can be just a plain loop (fused), or it can
be all the iterations of the loop held in memory at once.  In fact, lists admit
some operations (like `reverse`) that prevent fusion, but `Loop` has a refined
type that only allows construction of fusible operations!  This is exactly where
Haskell shines: the type system prevents incorrect (or in this case,
undesirable) programs from being written. I see this as part of a (relatively
recent) trend in Haskell toward using the type system to guarantee performance
in addition to correctness.
