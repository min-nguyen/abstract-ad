{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Documentation.Extensions where

import Documentation.AbstractAD
import Documentation.Forward

{-- | Symbolic AD is simply Forward-mode AD, the dual number Semiring d => (d, v -> d),
      but where we use "Var :: v -> Expr v" to evaluate variables into the symbolic
      semiring "Expr V" rather than into concrete numeric values.
--}
symbolicAD :: (Eq v, Semiring d) => Expr v -> Expr v ⋉ (Dense v (Expr v))
symbolicAD = abstractAD Var

derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
derive2nd var e =
  let f = forwardAD_Dense var

  in undefined -- fmap (fmap (eval var) . tanN . forwardADDense Var) . tanN .
