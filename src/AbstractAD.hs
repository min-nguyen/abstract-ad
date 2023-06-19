{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (Monoid(..), (<>))

{-- Modules E over Semirings D
--}

{- Some background:
    A "Field" is a set F that supports two  operations:
      1. Addition ,             f1 + f2     where f1, f2 ∈ F      (Commutative)
      2. Multiplication,        f1 * f2     where f1, f2 ∈ F      (Commutative)
    A "Vector Space over a Field F (of scalars)" is a set V that supports:
      1. Vector addition,       v1 + v2     where   v1, v2 ∈ V
      2. Scalar multiplication, f • v       where f ∈ F, v ∈ V
    A "Ring" is a set R that generalises the notion of a Field, by not requiring multiplication to be commutative. It is:
      1. Addition,              r1 + r2     where r1, r2 ∈ R
      2. Multiplication,        r1 * r2     where r1, r2 ∈ R    (R is a monoid under (*))
    A "Module over a Ring R" is a set M that generalises the notion of a Vector Space, by simply replacing the Field F with the Ring R:
      1. Vector addition,       v + w     where v, w ∈ V
      2. Scalar multiplication, f • v     where f ∈ F, v ∈ V
-}

{- | A Monoid E.
   For the purposes of dual numbers, (<>) is interpreted additively (⊕) rather than multiplicatively
-}
class Monoid e where
  mzero :: e
  (<>)  :: e -> e -> e

{- | A Semiring is a set D that can be viewed as two monoids of the same underlying type.
    1. The first monoid is additive (⊕) (and also commutative).
    2. The second monoid is multiplicative (⊗).
-}
class Semiring d where
  zero :: d
  one  :: d
  (⊕)  :: d -> d -> d
  (⊗)  :: d -> d -> d

{- | A "Module E over a Semiring D" is :
     1. A commutative additive Monoid E.
          mzero :: e
          (<>)  :: e -> e -> e
     2. Together with *scalar* multiplication that distributes over (<>).
          (•) :: d -> e -> e
  This abstraction generalises the notion of a gradient.
-}
class (Semiring d, Monoid e) => Module d e | e -> d where
  (•) :: d -> e -> e

-- | Every semiring d is trivially a d-module, by setting scalar multiplication to be multiplication
instance (Semiring d, Monoid d) => Module d d where
  (•) :: d -> d -> d
  (•) = (⊗)

{- Nagata numbers d ⋉ e,
   as a generalisation over dual numbers Dual d where the primal and tangent have different types
-}
data d ⋉ e = Nagata d e deriving Functor

-- | Every d-module e admits a semiring structure of Nagata numbers d ⋉ e
instance (Module d e) => Semiring (d ⋉ e) where
  zero :: Module d e => d ⋉ e
  zero = Nagata zero mzero
  one :: Module d e => d ⋉ e
  one = Nagata one mzero
  (⊕) :: Module d e => d ⋉ e -> d ⋉ e -> d ⋉ e
  (⊕) (Nagata f df) (Nagata g dg) = Nagata (f ⊕ g) (df <> dg)
  (⊗) :: Module d e => d ⋉ e -> d ⋉ e -> d ⋉ e
  (⊗) (Nagata f df) (Nagata g dg) = Nagata (f ⊗ g) ((f • dg) <> (g • df))

{- Kronecker Delta

-}
class Module d e => Kronecker v d e where
  delta :: v -> e

