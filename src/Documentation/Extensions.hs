{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Documentation.Extensions where

import Documentation.AbstractAD
import Documentation.Forward
import Documentation.Symbolic hiding (forwardAD_Dense)

{-- | Second-order derivatives can be naively computed by applying symbolic differentiation twice, producing
      a symbolic expression "(dy/dx)/dx :: Expr V", and then evaluating this at a concrete point.
        (Reminder: Symbolic diff uses "Var :: v -> Expr v" to evaluate variables into the symbolic semiring "Expr V"
        rather than into concrete numeric values.)
--}
derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
derive2nd var e = Dense $ \x -> Dense $ \y ->
  let Nagata _ (Dense sym_de)   = symbolic_Dense e
      dedx   = sym_de x          -- | Differentiate e wrt x
      Nagata _ (Dense sym_dedx) = symbolic_Dense dedx
      dedxdy = sym_dedx y        -- | Differentiate e wrt x wrt y
  in  eval var dedxdy            -- | Evaluate the symbolic derivative of dedxdy at point "var"

-- derive2nd':: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
-- derive2nd' var e = Dense $ \x -> Dense $ \y -> let
--       gen x = Nagata (var x) (delta x)
--       y = tangent (forwardAD_Dense gen e)) x
--   in undefined