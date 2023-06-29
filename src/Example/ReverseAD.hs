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
data Nagata v d = N { primal  :: d, tangent :: d -> Map v d }

-- | Set the tangent to scale the trivial derivative dx/dx = 1 by the supplied adjoint.
nagata :: Num d => d -> v -> Nagata v d
nagata x v = N x (\d -> singleton v (1 * d))

-- | Provide the tangent dy/dy of the overall expression a seed value of 1 as the initial adjoint.
runReverse :: Num d => Nagata v d -> Map v d
runReverse = ($ 1) . tangent

instance (Ord v, Ord d, Num d) => Num (Nagata v d) where
  fromInteger n   = N (fromInteger n) (const empty)
  N x dx + N y dy = N (x + y)    (\d -> unionWith (+) (dx d)       (dy d))
  N x dx - N y dy = N (x - y)    (\d -> unionWith (+) (dx d)       (dy (negate d)))
  N x dx * N y dy = N (x * y)    (\d -> unionWith (+) (dx (y * d)) (dy (x * d)))
  negate (N x dx) = N (negate x) (\d -> dx (negate d))
  abs (N x dx)    = N (abs x)    (\d -> if x >= 0 then dx d else dx (negate d))
  signum          = error "undefined"

instance (Ord v, Ord d, Fractional d) => Fractional (Nagata v d) where
  fromRational r  = N (fromRational r) (const empty)
  recip (N x dx)  = N (recip x) (\d -> dx ((recip (-x * x)) * d))
  N x dx / N y dy = N (x / y) (\d -> let z = y * y
                                     in unionWith (+) (dx ((y / z) * d)) (dy ((-x / z) * d))  )

instance (Ord v, Ord d, Floating d) => Floating (Nagata v d) where
  pi = N pi (const empty)
  exp (N x dx)  = N (exp x)  (\d -> dx $ (exp x) * d)
  log (N x dx)  = N (log x)  (\d -> dx $ (recip x) * d)
  sqrt (N x dx) = N (sqrt x) (\d -> dx $ (recip (2 * sqrt x)) * d)
  sin (N x dx)  = N (sin x)  (\d -> dx $ (cos x) * d)
  cos (N x dx)  = N (cos x)  (\d -> dx $ (negate $ sin x) * d )
  tan (N x dx)  = N (tan x)  (\d -> dx $ (recip $ cos x ** 2) * d)
  asin  = error "undefined"
  acos  = error "undefined"
  atan  = error "undefined"
  sinh  = error "undefined"
  cosh  = error "undefined"
  asinh = error "undefined"
  acosh = error "undefined"
  atanh = error "undefined"

example1 :: Map String Double
example1 = runReverse (prog (nagata 5 "x"))
  where prog x = x * (x + 1) * (x + x)

example2 :: Map String Double
example2 = runReverse (prog (nagata 4 "x1") (nagata 7 "x2"))
  where prog x1 x2 = x1 * x2 + x2 * sin x2
