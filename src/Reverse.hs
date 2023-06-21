{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Reverse where

import AbstractAD
import Prelude hiding ((<>), Monoid)
import Data.Map
import Forward

{- Rather than working directly with Module D E:
     "Module E over Semiring D"
   we now work with the isomorphic type Hom D E:
     "Homomorphisms from Semiring D (scalars) to Module E (vectors)".
   Intuitively, Hom D E augments a Vector E with an _accumulator_ for a Scalar Multiplier D.
-}
newtype Hom d e = Hom (d -> e)

-- | Vector addtion is still linear
instance Monoid e   => Monoid (Hom d e) where
  mzero          = Hom (const mzero)
  Hom f <> Hom g = Hom (\d -> f d <> g d)

-- | Scalar multiplication (•) now _accumulates_ in the function parameter D in Hom (D -> E),
--   using the cheaper semiring multiplication (⊗).
instance Module d e => Module d (Hom d e) where
  d • (Hom f)    = Hom (\d' -> f (d ⊗ d'))

-- | To instantiate a basis vector for the general type "Hom D E" for any tangent E:
--    1. We must first instantiate a basis vector of type D-Module E for variable V,
--    2. And then scalar multiply (•) this by an accumulator D.
instance {-# OVERLAPPABLE #-}
         Kronecker v d e     => Kronecker v d (Hom d e) where
  delta x        = Hom (\d  -> d • delta x)
-- | To instantiate a basis vector for the specific type "Hom D (Sparse V D)" that represents the tangent as a sparse map:
--    1. We can simply return the multiplicative accumulator D as the only map entry.
instance (Ord v, Semiring d) => Kronecker v d (Hom d (Sparse v d)) where
  delta x        = Hom (\d -> Sparse (singleton x d))


{- | Sparse Reverse AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Hom D (Sparse V D)"
      whose primals are scalars D,
      and tangents Hom D (Sparse V D) are decomposed into:
           1. An accumulator D for scalar multiplication
           2. A gradient vector (Sparse V D) as a sparse map
-}
reverseAD_Sparse :: (Ord v, Semiring d) => (v -> d) -> Expr v -> d ⋉ Hom d (Sparse v d)
reverseAD_Sparse = abstractAD