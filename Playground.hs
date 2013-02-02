{-# LANGUAGE RebindableSyntax #-}
-- | This is as small space for our various experiments with extending Nikola.

module Data.Array.Nikola.Playground where

import Data.Array.Nikola.Repr.Global
--import Data.Array.Nikola.Exp as E
import Data.Int(Int32)
import Data.Array.Nikola.Backend.CUDA
import qualified Data.Array.Nikola.Language.Syntax as S
import Prelude hiding (fromIntegral)
import Data.Array.Nikola.Combinators

{-

We want something like :

unfoldn :: Exp Int -> (Exp a -> Exp a) -> Array r sh a -> Array r (sh :. Exp Int) a

Where @unfoldn n f xs@ adds a dimension to @xs@ of length @n@ by iterating @f@
@n@ times over each cell in @xs@.

Such a function cannot feasibly return a delayed array, as the results are
sequentially dependent on each other. (Alright, except for the case where the
results are being consumed in a sequentially compatible manner, such as by a
@map@. But consumption via 'index' is a big no-no)

So, let's see just how Nikola fails when we try to do data-dependent array
allocation. (Or maybe even more interesting, how we are going to deal with it.)

CUDA has no in-kernel malloc(), and doesn't tolerate array declarations of data
dependent sizes. So, it seems that we either need to join program control back
to CPU to handle memory, or have some sort of memory management in place.

(Really, I think all the high performance issues should be secondary to
expressive power. It just seems wrong to start out with only 'what is known to
perform' and then trying to work your way towards more expressive power within
those limits, rather than starting out with 'what kind of programs do we want
to run' and then working your way to more effective compilation schemes. And it
seems to me that the 'what is known to perform' is what is limiting both
Accelerate and Nikola)

(Maybe it especially seems wrong because working to match resource limitations
is usually done as a programing task in a concrete problem context, while
designing a programming language is usually about defining a  concise
description a problem domain in general (i.e. outside any particular context))

It would probably prove interesting as well just managing the memory allocation
problem, and possibly using that to implement support for function recursion
(if only just tail calls with arrays, and provided that we may accurately
detect recursive calls in the DSL encoding).

i.e.:
f x = if p x then g x
             else f (h x)
(where 'f not in FV(g) or FV(h)', but may have @x :: Array sh r a@)

-}

-- @allocFoo n@ computes the sum of the numbers from 1 to n naively by
-- allocating an n-long array, filling it from 0 to n and summing up.
-- allocFoo :: E.Exp t Int -> E.Exp t Int
-- allocFoo n =

-- Experiment with recursion detection
fooRec :: Exp Int32 -> Exp Int32
fooRec x = let fix x' = if x' ==* 1 then 1 else x + ({- fooRec -}(x' - 1))
            in fix x

-- ^ loops forever. I don't know exactly whether or not Nikola discovers the
-- sharing but fails to act on it properly, or ..

-----------------------------------------------------------------------------

-- Could be used to implement fast binomial:

-- | Destructively update a global array n times with a function.
-- unsafeIterateN :: Exp Int -> (Exp Int -> Array r sh a -> Array D sh a) -> Array G sh a -> Array G sh a

-- | Could be used to implement fast jumping sobol sequence generation:
-- unfoldN :: Exp Int -> (Exp Int -> Exp a -> Exp a) -> Array r sh a -> Array G　(sh :. Exp Int) a

-- ^ It is really ugly that we can't adequately express the above as @map
-- unfold@ because of arrays with shapes (as we can't feasibly represent
-- dimension extents as a type we can't guarantee that unfold won't produce
-- irregularly shaped outputs)

-- ParSeq notes: the output of unfoldN could be fused with a parseq map

-- Maybe we should just go ahead and try in the P monad?

{- (proper experiment in Repr/Push.hs and Shape.hs)
unfoldN ::
  Source r =>
  Exp Int32 -> (Exp Int32 -> Exp a -> Exp a) -> Array r sh a -> Array PSH (sh :. Exp Int32) a

unfoldN n f xs =
-}

{-

* tests are in ghciscript.hs

-}
