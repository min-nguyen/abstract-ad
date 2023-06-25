module Example.ReverseAD where

{- | This file defines a simple reverse-mode version of AD.
-}

import Data.Map
import Data.Number.Erf

{-- | An expression e evaluates to the nagata number N v (dx :: d -> Map v d).
      The tangent is a continuation that takes the adjoint of e (i.e. the rate-of-change
      of the *overall function* wrt e), and recursively decomposes this into the
      rates-of-change of each sub-expression in e. These results can then be unioned
      together, summing the rates-of-change that correspond to the same variable x_i
      when occuring in multiple positions in e.
--}
data Nagata v d = N { primal  :: d
                    , tangent :: d -> Map v d
                    }

instance (Ord v, Num d) => Num (Nagata v d) where
  fromInteger n   = N (fromInteger n) (const empty)
  N x dx + N y dy = N (x + y) (\d -> unionWith (+) (dx d)       (dy d))
  N x dx - N y dy = N (x - y) (\d -> unionWith (+) (dx d)       (dy (negate d)))
  N x dx * N y dy = N (x * y) (\d -> unionWith (+) (dx (y * d)) (dy (x * d)))

instance (Ord v, Num d) => Fractional (Nagata v d) where
instance (Ord v, Floating d) => Floating (Nagata v d) where
  sin (N x dx) = N (sin x) (\d -> (dx (cos x * d)))

nagata :: d -> v -> Nagata v d
nagata x v = N x (\d -> singleton v d)

example1 :: Map String Double
example1 = tangent (prog (nagata 5 "x")) 1
  where prog x = x * (x + 1) * (x + x)

example2 :: Map String Double
example2 = tangent (prog (nagata 4 "x1") (nagata 7 "x2")) 1
  where prog x1 x2 = x1 * x2 + sin x2
