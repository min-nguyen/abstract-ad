{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
forwardAD_Dense_2nd :: forall v d e. (Eq v, Kronecker v d e) => (v -> d) -> Expr v -> Dense v (Dense v d)
forwardAD_Dense_2nd var e = Dense $ \x -> Dense $ \y ->
  let -- Define a generator that instantiates variables in Expr v to first-order, Nagata numbers (d ⋉ Dense v d).
      gen    :: v -> (d ⋉ Dense v d)
      gen z  = Nagata (var z) (delta z)
      -- Providing the generator to "abstractAD" will then instantiate variables in Expr v to second-order Nagata numbers, (d ⋉ Dense v d) ⋉ (d ⋉ Dense v (d ⋉ Dense v d)).
      de     :: Dense v (d ⋉ (Dense v d))
      de     = tangent (abstractAD gen e :: (d ⋉ (Dense v d)) ⋉ (Dense v (d ⋉ (Dense v d))))
      dedx   :: Dense v d
      dedx   = tangent (runDense de x)
      dedxdy :: d
      dedxdy = runDense dedx y
  in  dedxdy

{--  | COMPACT SECOND-ORDER NAGATA NUMBERS (⋉⋉)
       We define (⋉⋉) for the compact representation of Second-Order Nagata numbers (D ⋉ (V -> (D ⋉ (V -> D))):
             Nagata2 e (λx -> (dedx, λy -> dedxdy))

        .                 e                -- 0th order
        .              /     \
        .            e        dedx         -- 1st order
        .          /         /    \
        .         e        dedx  dedxdy    -- 2nd order
--}
data v ⋉⋉ d = Nagata2 d (Dense v (d ⋉ (Dense v d)))

instance (Semiring d) => Semiring (v ⋉⋉ d) where
  zero        =  Nagata2 zero mzero
  one         =  Nagata2 one  mzero
  Nagata2 f ddf ⊕ Nagata2 g ddg =  Nagata2 (f ⊕ g) (ddf ⊕ ddg)
  Nagata2 f ddf ⊗ Nagata2 g ddg
    =  Nagata2 (f ⊗ g)                                            -- Compute result (f * g)
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

{-- |  COMPACT SECOND-ORDER DENSE AD is a variant of Abstract AD for Second-Order Nagata Numbers (⋉⋉)
--}
forwardAD_Dense_Compact2nd :: Eq v => Semiring d => (v -> d) -> Expr v -> v ⋉⋉ d
forwardAD_Dense_Compact2nd var = eval gen where
  gen x = Nagata2 (var x) (delta x)