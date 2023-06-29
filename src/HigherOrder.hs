{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module HigherOrder where

import AbstractAD
import Forward
import Symbolic hiding (forwardAD_Dense)
import Prelude hiding (Monoid)

{-- | SECOND-ORDER DERIVATIVES can be naively computed by applying symbolic differentiation twice, producing
      a symbolic expression "(dy/dx)/dx :: Expr V", and then evaluating this at a concrete point.
        (Reminder: Symbolic diff uses "Var :: v -> Expr v" to evaluate variables into the symbolic semiring "Expr V"
        rather than into concrete numeric values.)
      The basic implementation is:
        derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
        derive2nd var e = Dense $ λx -> Dense $ λy ->
          let Nagata _ (Dense sym_de)   = symbolic_Dense e
              sym_dedx   = sym_de x          -- | Differentiate e wrt x
              Nagata _ (Dense sym_dedx) = symbolic_Dense dedx
              sym_dedxdy = sym_dedx y        -- | Differentiate e wrt x wrt y
          in  eval var sym_dedxdy            -- | Evaluate the symbolic derivative of dedxdy at point "var"
--}

{-- | NAIVE SECOND-ORDER DENSE FORWARD-MODE AD specialises Abstract AD to use nested Nagata numbers "(D ⋉ (V -> D)) ⋉ (V -> (D ⋉ (V -> D)))"":
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
        .         e  dedx  dedx  dedxdy    -- 2nd order     (note: redundant duplicate computation of dedx)
--}
forwardAD_NaiveDense2 :: forall v d. (Eq v, Semiring d) => (v -> d) -> Expr v -> (d ⋉ (Dense v d)) ⋉ (Dense v (d ⋉ (Dense v d)))
forwardAD_NaiveDense2 var expr = abstractAD gen expr where
  -- | The generator instantiates V to first-order Nagata numbers (d ⋉ Dense v d).
  --   Providing this to abstractAD will instantiate V to second-order Nagata numbers (d ⋉ Dense v d) ⋉ (d ⋉ Dense v (d ⋉ Dense v d)).
  gen    :: v -> (d ⋉ Dense v d)
  gen z  = Nagata (var z) (delta z)

forwardAD_NaiveDense2_example :: (Double, Double, Double)
forwardAD_NaiveDense2_example =
  let Nagata (Nagata e _) (Dense ded)   = forwardAD_NaiveDense2 (\X -> 5 :: Double) example
      Nagata dedx         (Dense dedxd) = ded X              :: Double ⋉ (Dense XY Double)
      dedxdx                            = dedxd X            :: Double
  in  (e, dedx, dedxdx)

{--  | COMPACT SECOND-ORDER DENSE FORWARD-MODE AD specialises Abstract AD to use Nagata numbers "D ⋉ (V -> (D ⋉ (V -> D))":
           Nagata e (λx -> (dedx, λy -> dedxdy))

        .                 e                -- 0th order
        .              /     \
        .            e        dedx         -- 1st order
        .          /         /    \
        .         e        dedx  dedxdy    -- 2nd order
--}
type Dense2 v d = d ⋉ (Dense v (d ⋉ (Dense v d)))

instance {-# OVERLAPPING #-} (Semiring d) => Semiring (Dense2 v d) where
  zero        =  Nagata zero mzero
  one         =  Nagata one  mzero
  Nagata f dfd ⊕ Nagata g dgd = Nagata (f ⊕ g) (dfd ⊕ dgd)
  Nagata f dfd ⊗ Nagata g dgd
    =  Nagata (f ⊗ g)                                            -- Compute result (f * g)
          (Dense $ \x ->
              let  Nagata dfdx dfdxd = runDense dfd x             -- Get first-order partial derivative df/dx
                   Nagata dgdx dgdxd = runDense dgd x             -- Get first-order partial derivative dg/dx
              in   Nagata ((dfdx ⊗ g) ⊕ (f ⊗ dgdx))               -- Compute first-order derivative d(f * g)/dx
                          (Dense $ \y ->
                              let dfdxdy = runDense dfdxd y      -- Get second-order partial derivative df/dx/dy
                                  dgdxdy = runDense dgdxd y      -- Get second-order partial derivative dg/dx/dy
                              in (dfdxdy ⊗ g) ⊕ (f ⊗ dgdxdy)    -- Compute second-order derivative d(f * g)/dx/dy
                                 ⊕ (dfdx ⊗ dgdx) ⊕ (dgdx ⊗ dfdx)
                          )
          )

forwardAD_Dense2 :: forall v d. (Eq v, Semiring d) => (v -> d) -> Expr v -> d ⋉ (Dense v (d ⋉ (Dense v d)))
forwardAD_Dense2 var = eval gen where
  -- | The generator instantiates V to second-order Nagata numbers.
  --   This is passed directly to 'eval :: (v -> d) -> Expr v -> d' rather than 'abstractAD :: (v -> d) -> Expr v -> d ⋉ e'
  --   to avoid re-lifting into another Nagata number.
  gen :: v -> (Dense2 v d)
  gen x = Nagata (var x) (delta x)

forwardAD_Dense2_example :: (Double, Double, Double)
forwardAD_Dense2_example =
  let Nagata e    (Dense ded)   = forwardAD_Dense2 (\X -> 5) example :: Double ⋉ (Dense XY (Double ⋉ Dense XY Double))
      Nagata dedx (Dense dedxd) = ded X                              :: Double ⋉ (Dense XY Double)
      dedxdx                    = dedxd X                            :: Double
  in  (e, dedx, dedxdx)

instance Show d => Show (Dense XY d) where
  show f = "(\\X -> " ++ show (runDense f X) ++ ")"
deriving instance {-# OVERLAPPING #-} Show d => Show (Dense2 XY d)


{-- |  GENERIC HIGHER-ORDER DENSE FORWARD-MODE AD specialises Abstract AD to work with "NagataStream D (V -> NagataStream V D)"
          Nagata e (λx -> (dedx, λy -> dedxdy))
       The tangent is a stream:
        - the head is the current partial derivative
        - the tail is a function to a stream of higher-order partial derivatives.
--}
data NagataStream v d = d :< Dense v (NagataStream v d)
deriving instance Show d => Show (NagataStream XY d)

instance Semiring d => Semiring (NagataStream v d) where
  zero  = zero :< zero
  one   = one :< zero
  (f :< dfs) ⊕ (g :< dgs)
    = (f ⊕ g)  :< (dfs ⊕ dgs)
  (f :< dfs) ⊗ (g :< dgs)
    = (f ⊗ g) :< Dense (\x ->
      let dfdx = runDense dfs x
          dgdx = runDense dgs x
      in ((dfdx ⊗ (g :< dgs)) ⊕ ((f :< dfs) ⊗ dgdx)))

forwardAD_DenseHOrd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> NagataStream v d
forwardAD_DenseHOrd var = eval gen where
  gen y = var y :< delta y

forwardAD_DenseHOrd_example :: NagataStream XY Int
forwardAD_DenseHOrd_example = forwardAD_DenseHOrd (\X -> 5) example