{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Documentation.HigherOrder where

import Documentation.AbstractAD
import Documentation.Forward
import Documentation.Symbolic hiding (forwardAD_Dense)
import Prelude hiding (Monoid)

{-- | SECOND-ORDER DERIVATIVES can be naively computed by applying symbolic differentiation twice, producing
      a symbolic expression "(dy/dx)/dx :: Expr V", and then evaluating this at a concrete point.
        (Reminder: Symbolic diff uses "Var :: v -> Expr v" to evaluate variables into the symbolic semiring "Expr V"
        rather than into concrete numeric values.)
      The basic implementation is:
        derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
        derive2nd var e = Dense $ λx -> Dense $ λy ->
          let Nagata _ (Dense sym_de)   = symbolic_Dense e
              dedx   = sym_de x          -- | Differentiate e wrt x
              Nagata _ (Dense sym_dedx) = symbolic_Dense dedx
              dedxdy = sym_dedx y        -- | Differentiate e wrt x wrt y
          in  eval var dedxdy            -- | Evaluate the symbolic derivative of dedxdy at point "var"
--}

{-- | SECOND-ORDER DENSE FORWARD-MODE AD specialises the Abstract AD to use nested Nagata numbers (D ⋉ (V -> D)) ⋉ (V -> (D ⋉ (V -> D))):
           Nagata (e, λx -> dedx) (λx -> (dedx, λy -> dedxdy))
      For a given expression e:
        1. The primals are themselves first-order dual numbers:
                 (e, λx -> dedx) :: (D ⋉ (V -> D))
           whose primals are the result e, and tangents are functions to the first-order derivatives.
        2. The tangents are functions to second-order dual numbers:
                 (λx -> (dedx, λy -> dedxdy)) :: (V -> (D ⋉ (V -> D)))
           whose primals are first-order derivatives, and tangents are functions to second-order derivatives.

        .                 e                -- 0th order
        .              /     \
        .            e        dedx         -- 1st order
        .          /   \     /    \
        .         e  dedx  dedx  dedxdy    -- 2nd order
--}
forwardAD_NaiveDense2ndOrd :: forall v d. (Eq v, Semiring d) => (v -> d) -> Expr v -> (d ⋉ (Dense v d)) ⋉ (Dense v (d ⋉ (Dense v d)))
forwardAD_NaiveDense2ndOrd var expr = abstractAD gen expr where
  -- Define a generator that instantiates V to first-order Nagata numbers (d ⋉ Dense v d).
  -- Providing this to "abstractAD" will then instantiate V to second-order Nagata numbers (d ⋉ Dense v d) ⋉ (d ⋉ Dense v (d ⋉ Dense v d)).
  gen    :: v -> (d ⋉ Dense v d)
  gen z  = Nagata (var z) (delta z)

forwardAD_NaiveDense2ndOrd_example :: XY -> XY -> (Double, Double, Double)
forwardAD_NaiveDense2ndOrd_example x y =
  let Nagata (Nagata e _) (Dense ded)   = forwardAD_NaiveDense2ndOrd (\X -> 5 :: Double) example
      Nagata dedx         (Dense dedxd) = ded x              :: Double ⋉ (Dense XY Double)
      dedxdy                            = dedxd y            :: Double
  in  (e, dedx, dedxdy)

{--  | COMPACT SECOND-ORDER NAGATA NUMBERS (⋉⋉)
       We define (⋉⋉) for the compact representation of Second-Order Nagata numbers (D ⋉ (V -> (D ⋉ (V -> D))):
           Nagata e (λx -> (dedx, λy -> dedxdy))

        .                 e                -- 0th order
        .              /     \
        .            e        dedx         -- 1st order
        .          /         /    \
        .         e        dedx  dedxdy    -- 2nd order
--}
type v ⋉⋉ d = d ⋉ (Dense v (d ⋉ (Dense v d)))

instance {-# OVERLAPPING #-} (Semiring d) => Semiring (d ⋉ (Dense v (d ⋉ (Dense v d)))) where
  zero        =  Nagata zero mzero
  one         =  Nagata one  mzero
  Nagata f ddf ⊕ Nagata g ddg =  Nagata (f ⊕ g) (ddf ⊕ ddg)
  Nagata f ddf ⊗ Nagata g ddg
    =  Nagata (f ⊗ g)                                            -- Compute result (f * g)
          (Dense $ \x ->
              let  Nagata dfdx ddfdx = runDense ddf x             -- Get first-order partial derivative df/dx
                   Nagata dgdx ddgdx = runDense ddg x             -- Get first-order partial derivative dg/dx
              in   Nagata ((dfdx ⊗ g) ⊕ (f ⊗ dgdx))               -- Compute first-order derivative d(f * g)/dx
                          (Dense $ \y ->
                              let ddfdxdy = runDense ddfdx y      -- Get second-order partial derivative df/dx/dy
                                  ddgdxdy = runDense ddgdx y      -- Get second-order partial derivative dg/dx/dy
                              in (ddfdxdy ⊗ g) ⊕ (f ⊗ ddgdxdy)    -- Compute second-order derivative d(f * g)/dx/dy
                                 ⊕ (dfdx ⊗ dgdx) ⊕ (dgdx ⊗ dfdx)
                          )
          )

{-- |  COMPACT SECOND-ORDER DENSE AD is a variant of Abstract AD for Second-Order Nagata Numbers (⋉⋉),
          Nagata e (λx -> (dedx, λy -> dedxdy))
--}
forwardAD_Dense2ndOrd :: Eq v => Semiring d => (v -> d) -> Expr v -> v ⋉⋉ d
forwardAD_Dense2ndOrd var expr = Nagata e dde where
  gen x = Nagata (var x) (delta x)
  Nagata e dde = eval gen expr

forwardAD_Dense2ndOrd_example :: XY ⋉⋉ Double
forwardAD_Dense2ndOrd_example = forwardAD_Dense2ndOrd (\X -> 5 :: Double) example

instance Show d => Show (Dense XY d) where
  show f = "(\\X -> " ++ show (runDense f X) ++ ")"
deriving instance {-# OVERLAPPING #-} Show d => Show (XY ⋉⋉ d)
