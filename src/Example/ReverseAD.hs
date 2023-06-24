module Example.ReverseAD where

{- | This file defines a simple reverse-mode version of AD.
-}

import Data.Map
import Data.Number.Erf

{-- | The tangent (dx :: d -> Map v d) is a continuation that takes an
        - Adjoint, representing the rate-of-change of the *overall function* wrt the sub-expression that computes the current Nagata number

--}
data Nagata v d = N { primal  :: d
                    , tangent :: d -> Map v d
                    }


instance (Ord v, Ord d, Num d) => Num (Nagata v d) where
  fromInteger n   = N (fromInteger n) (const empty)
  N x dx + N y dy = N (x + y) (\d -> unionWith (+) (dx d)       (dy d))
  N x dx - N y dy = N (x - y) (\d -> unionWith (+) (dx d)       (dy (negate d)))
  N x dx * N y dy = N (x * y) (\d -> unionWith (+) (dx (y * d)) (dy (x * d)))
  -- negate (N x dx) = N (negate x) (fmap negate dx)
  -- abs (N x dx) = N (abs x) (if x >= 0 then dx else fmap negate dx)
  -- signum = error "undefined"

nagata :: d -> v -> Nagata v d
nagata x v = N x (\d -> singleton v d)

prog :: Num d => d -> d
prog x = x * (x + 1) * (x + x)

f :: (Double, Map String Double)
f = let N v d = prog (nagata 5 "x")
    in  (v, d 1)

-- instance (Ord v, Ord d, Fractional d) => Fractional (Nagata v d) where
--   fromRational r  = N (fromRational r) empty
--   recip (N x dx)  = N (recip x) (fmap (recip (-x * x)*) dx)
--   N x dx / N y dy = N (x / y) (let z = y * y in unionWith (+) (fmap ((y / z) *) dx) (fmap ((-x / z) *) dy))

-- instance (Ord v, Ord d, Floating d) => Floating (Nagata v d) where
--    pi = N pi empty
--    exp (N x dx) = N (exp x) (fmap ((exp x) *) dx)
--    log (N x dx) = N (log x) (fmap ((recip x) *) dx)
--    sqrt (N x dx) = N (sqrt x) (fmap ((recip (2 * sqrt x)) *) dx)
--    N x dx ** N y dy  = error "undefined"
--    logBase x y = error "undefined"
--    sin (N x dx) = N (sin x) (fmap ((cos x) *) dx)
--    cos (N x dx) = N (cos x) (fmap ((negate $ sin x) *) dx)
--    tan (N x dx) = N (tan x) (fmap ((recip $ cos x ** 2) *) dx)
--    asin = error "undefined"
--    acos = error "undefined"
--    atan = error "undefined"
--    sinh = error "undefined"
--    cosh = error "undefined"
--    tanh = error "undefined"
--    asinh = error "undefined"
--    acosh = error "undefined"
--    atanh = error "undefined"

-- -- Standard Normal PDF
-- normpdf :: Floating d => d -> d
-- normpdf x = exp (negate (x * x) / 2) / (sqrt (2 * pi))

-- -- Probit function (inv cdf of normal)
-- instance (Ord v, Ord d, Floating d, InvErf d) => InvErf (Nagata v d) where
--   invnormcdf (N x dx) = N (invnormcdf x) (fmap (/ (normpdf (invnormcdf x))) dx)
