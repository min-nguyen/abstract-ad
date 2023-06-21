{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Forward where

import AbstractAD
import Prelude hiding (Monoid)

{- | Dense V D represents a Gradient Vector that maps variables V to their partial derivatives (scalars) D.
-}
newtype Dense v d = Dense (v -> d)

-- | Gradient Vectors are additive Monoids
instance Semiring d => Monoid (Dense v d) where
  mzero                = Dense (\v -> zero)
  Dense f1 <> Dense f2 = Dense (\v -> f1 v ⊕ f2 v)
-- | Gradient Vectors are Modules over the type of their elements.
-- Intuitively, vectors can be scaled by values that match the same type as their vector elements.
instance Semiring d => Module d (Dense v d) where
  a • Dense f = Dense (\v -> a ⊗ f v)
-- | Gradient Vectors have a Kronecker-delta function
instance (Semiring d, Eq v) => Kronecker v d (Dense v d) where
  delta v = Dense (\v' -> if v == v' then one else zero)