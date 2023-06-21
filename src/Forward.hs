{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Forward where

import AbstractAD
import Prelude hiding (Monoid)
import Data.Map

{-- | Dense V D represents a Gradient Vector as a function mapping variables V to their partial derivatives (scalars) D.
--}
newtype Dense v d = Dense (v -> d)

-- | Gradient Vectors are Monoids that are additive
instance Semiring d => Monoid (Dense v d) where
  mzero                = Dense (\v -> zero)
  Dense f1 <> Dense f2 = Dense (\v -> f1 v ⊕ f2 v)
-- | Gradient Vectors are Modules over the type of their elements
-- Intuitively, vectors can be scaled by values that match the same type as their vector elements
instance Semiring d => Module d (Dense v d) where
  a • Dense f = Dense (\v -> a ⊗ f v)
-- | Gradient Vectors have a Kronecker-delta function
instance (Semiring d, Eq v) => Kronecker v d (Dense v d) where
  delta v = Dense (\v' -> if v == v' then one else zero)

{- | Dense Forward AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Dense V D", where
       - Primals are scalars D
       - Tangents are gradient vectors (Dense V D) as total functions.

      Rather than computing a value in (essentially):
        "forwardAD :: ... -> V -> (D ⋉ D)",
        which must recompute the primal and tangent for each provided seed variable
      We now work with values of:
        "forwardAD_Dense :: D ⋉ (V -> D)",
        which computes the primal only once, and shares this among all the tangents (partial derivatives) of specified seed variables
-}
forwardAD_Dense :: (Semiring d, Eq v) => (v -> d) -> Expr v -> d ⋉ Dense v d
forwardAD_Dense = abstractAD


{-- | Sparse V D represents a Gradient Vector as a Map from variables V to their partial derivatives (scalars) D.
This exploits that if a sub-expression e does not contain variable v, then its partial derivatives is zero.
In particular, if that sub-expression is itself a variable v' s.t v' =/= v, then its partial derivative is zero.
We use Sparse Maps that avoid explicitly representing these zeros.
--}
newtype Sparse v d = Sparse (Map v d)

-- | Gradient Vectors are Monoids that are additive
instance (Ord v, Semiring d) => Monoid (Sparse v d) where
  mzero                  = Sparse empty       -- Zero values are represented as missing map entries.
  Sparse f1 <> Sparse f2 = Sparse (unionWith (⊕) f1 f2)
-- | Gradient Vectors are Modules over the type of their elements
instance (Ord v, Semiring d) => Module d (Sparse v d) where
  a • Sparse f = Sparse (fmap (a ⊗) f)        -- Zero values are missing entries and so will not be fmapped over.
-- | Gradient Vectors have a Kronecker-delta function
instance (Ord v, Semiring d) => Kronecker v d (Sparse v d) where
  delta v = Sparse (singleton v one)          -- The only non-zero entry for delta x is for x.

{- | Sparse Forward AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Sparse V D", where
      - Primals are scalars D,
      - Tangents are gradient vectors (Sparse V D) as sparse maps.
-}
forwardAD_Sparse :: (Semiring d, Ord v) => (v -> d) -> Expr v -> d ⋉ Sparse v d
forwardAD_Sparse = abstractAD




{-- | A Kronecker Homomorphism maps from one type of Module (Vector) to another.
         hom :: (Kronecker V D E1, Kronecker V D E2) => E1 -> E2
      A Kronecker Isomorphism (hom, hom_inv) is a pair of these Homomorphisms that invert each other.

      We use "Dense V D" as a baseline for E. So when defining a new type of tangent E', we also want to
      define a Kronecker isomorphism, rep(resentation) :: Dense V D -> E' and abs(traction) :: E' -> Dense V D.
--}