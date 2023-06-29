{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Forward where

import AbstractAD
import Prelude hiding (Monoid)
import Data.Map

{-- | DENSE V D represents a Gradient Vector as a function mapping variables V to their partial derivatives (scalars) D.
--}
newtype Dense v d = Dense { runDense :: v -> d }

-- | Gradient Vectors are Monoids that are additive
instance Semiring d => Monoid (Dense v d) where
  mzero              = Dense (\v -> zero)
  Dense f <> Dense g = Dense (\v -> f v ⊕ g v)
-- | Gradient Vectors are Semirings under a specific variable V
instance Semiring d => Semiring (Dense v d) where
  zero = Dense $ \ _ -> zero
  one  = Dense $ \ _ -> one
  Dense f ⊕ Dense g = Dense $ \ v ->  f v ⊕ g v
  Dense f ⊗ Dense g = Dense $ \ v ->  f v ⊗ g v
-- | Gradient Vectors are Modules over the type of their elements i.e. vectors can be scaled by values of their vector elements
instance Semiring d => Module d (Dense v d) where
  a • Dense f = Dense (\v -> a ⊗ f v)
-- | Gradient Vectors have a Kronecker-delta function
instance (Semiring d, Eq v) => Kronecker v d (Dense v d) where
  delta v = Dense (\v' -> if v == v' then one else zero)

{-- | DENSE FORWARD-MODE AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Dense V D", where
       - Primals are scalars D
       - Tangents are gradient vectors (Dense V D) as total functions.

      Rather than computing a value in (essentially):
        "forwardAD :: ... -> V -> (D ⋉ D)",
        which must recompute the primal and tangent for each provided seed variable
      We now work with values of:
        "forwardAD_Dense :: D ⋉ (V -> D)",
        which computes the primal only once, and shares this among all the tangents (partial derivatives) of specified seed variables
--}
forwardAD_Dense :: (Semiring d, Eq v) => (v -> d) -> Expr v -> d ⋉ Dense v d
forwardAD_Dense = abstractAD

forwardAD_Dense_example :: (Double, Double)
forwardAD_Dense_example =
  let var :: XY -> Double
      var =  (\X -> 2.0)
      -- Double ⋉ Dense X Double
      Nagata result (Dense tangents) = forwardAD_Dense var (Times (Var X) (Plus (Var X) One))     -- x * (x + 1))
  in  (result, tangents X)

{-- | SPARSE V D represents a Gradient Vector as a Map from variables V to their partial derivatives (scalars) D.
       (This exploits that if a sub-expression e does not contain variable v, then its partial derivatives is zero.
        In particular, if that sub-expression is itself a variable v' s.t v' =/= v, then its partial derivative is zero.
        We use Sparse Maps that avoid explicitly representing these zeros.)
--}
type Sparse v d = Map v d

-- | Gradient Vectors are Monoids that are additive
instance (Ord v, Semiring d) => Monoid (Sparse v d) where
  mzero    = empty       -- Zero values are represented as missing map entries.
  f1 <> f2 = unionWith (⊕) f1 f2
-- | Gradient Vectors are Modules over the type of their elements
instance (Ord v, Semiring d) => Module d (Sparse v d) where
  a • f = fmap (a ⊗) f        -- Zero values are missing entries and so will not be fmapped over.
-- | Gradient Vectors have a Kronecker-delta function
instance (Ord v, Semiring d) => Kronecker v d (Sparse v d) where
  delta v = singleton v one          -- The only non-zero entry for delta x is for x.

{-- | SPARSE FORWARD-MODE AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Sparse V D", where
      - Primals are scalars D,
      - Tangents are gradient vectors (Sparse V D) as sparse maps.
--}
forwardAD_Sparse :: (Semiring d, Ord v) => (v -> d) -> Expr v -> d ⋉ Sparse v d
forwardAD_Sparse = abstractAD

forwardAD_Sparse_example :: (Double, Maybe Double)
forwardAD_Sparse_example =
  let var ::  XY -> Double
      var =  (\X -> 2.0)
      -- Double ⋉ Sparse X Double
      Nagata result tangents = forwardAD_Sparse var (Times (Var X) (Plus (Var X) One))     -- x * (x + 1))
  in  (result, Data.Map.lookup X tangents)


{-- | A Kronecker Homomorphism maps from one representation of Module E, i.e. tangent, to another.
         hom :: (Kronecker V D E1, Kronecker V D E2) => E1 -> E2
      A Kronecker Isomorphism (hom, hom_inv) is a pair of these Homomorphisms that invert each other.

      We treat Dense V D as a baseline for E.
      For each new tangent E', we also want to define a Kronecker isomorphism:
         rep(resentation) :: Dense V D -> E'
         abs(traction)    :: E' -> Dense V D
--}
