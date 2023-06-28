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
      The basic implementation is:
        derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
        derive2nd var e = Dense $ \x -> Dense $ \y ->
          let Nagata _ (Dense sym_de)   = symbolic_Dense e
              dedx   = sym_de x          -- | Differentiate e wrt x
              Nagata _ (Dense sym_dedx) = symbolic_Dense dedx
              dedxdy = sym_dedx y        -- | Differentiate e wrt x wrt y
          in  eval var dedxdy            -- | Evaluate the symbolic derivative of dedxdy at point "var"
--}

{-- | SECOND-ORDER FORWARD-MODE AD specialises the Abstract AD to work with nested Nagata numbers "(D ⋉ (V -> D)) ⋉ (V -> (D ⋉ (V -> D)))".
      For a given expression e:
        1. Their primals are themselves dual numbers, (e, \x -> dedx) :: (D ⋉ (V -> D)), who tangents are functions to the first-order derivatives.
        2. Their tangents are functions to dual numbers, (\x -> (dedx, \y -> dedxdy)) :: (V -> (D ⋉ (V -> D))), whose tangents are functions to second-order derivatives.
--}
derive2nd :: forall v d e. (Eq v, Kronecker v d e) => (v -> d) -> Expr v -> Dense v (Dense v d)
derive2nd var e = Dense $ \x -> Dense $ \y ->
  let -- Define a generator that instantiates variables in Expr v to Nagata numbers (d ⋉ Dense v d).
      gen    :: v -> (d ⋉ Dense v d)
      gen z  = Nagata (var z) (delta z)
      -- Providing the generator to "abstractAD" will have it instantiate variables to Nagata numbers (d ⋉ Dense v d) ⋉ (d ⋉ Dense v (d ⋉ Dense v d)).
      de     :: Dense v (d ⋉ (Dense v d))
      de     = tangent (abstractAD gen e :: (d ⋉ (Dense v d)) ⋉ (Dense v (d ⋉ (Dense v d))))
      dedx   :: Dense v d
      dedx   = tangent (runDense de x)
      dedxdy :: d
      dedxdy = runDense dedx y
  in  dedxdy

{--  | To avoid redundant recomputation of the same terms in Second-order AD above
                   e
                /      \
              /          \
              e        dedx
            /   \     /    \
           e  dedx  dedx  dedxdy
      :
--}

