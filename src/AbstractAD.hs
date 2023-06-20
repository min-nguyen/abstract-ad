{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module AbstractAD where

import Prelude hiding (Monoid(..), (<>))

{-- | ABSTRACTION 1: Modules E over Semirings D
--}

{- | A Monoid (E, <>).
   For the purposes of dual numbers, (<>) is interpreted additively (⊕) rather than multiplicatively
-}
class Monoid e where
  mzero :: e
  (<>)  :: e -> e -> e

{- | A Semiring (D, ⊕, ⊗) is two monoids of the same underlying type.
    1. An additive monoid      (D, ⊕) which is also commutative.
    2. A multiplicative monoid (D, ⊕).
-}
class Semiring d where
  zero :: d
  one  :: d
  (⊕)  :: d -> d -> d
  (⊗)  :: d -> d -> d

{- | A "Module (E, <>) over a Semiring of scalars (D, ⊕, ⊗)" is :
     1. An additive Monoid (E, <>) which is also commutative.
     2. An operation for *scalar* multiplication that distributes over (<>).
          (•) :: d -> e -> e
     This generalises the notion of a Vector Space V over a Field of scalars F
-}
class (Semiring d, Monoid e) => Module d e | e -> d where
  (•) :: d -> e -> e

-- | Every Semiring D is trivially a D-Module, by setting scalar multiplication to be multiplication
instance (Semiring d, Monoid d) => Module d d where
  (•) :: d -> d -> d
  (•) = (⊗)

{-- | ABSTRACTION 2: Nagata Numbers D ⋉ E
   This generalises over dual numbers Dual D by letting the primal and tangent have different types.
   Intuitively:
    D is a scalar representing the result of evaluation
    E is a vector representing the gradient
--}
data d ⋉ e = Nagata d e deriving Functor

-- | Every D-Module E admits a Semiring of Nagata numbers D ⋉ E
instance (Module d e) => Semiring (d ⋉ e) where
  zero :: Module d e => d ⋉ e
  zero = Nagata zero mzero
  one :: Module d e => d ⋉ e
  one = Nagata one mzero
  (⊕) :: Module d e => d ⋉ e -> d ⋉ e -> d ⋉ e
  (⊕) (Nagata f df) (Nagata g dg) = Nagata (f ⊕ g) (df <> dg)
  (⊗) :: Module d e => d ⋉ e -> d ⋉ e -> d ⋉ e
  (⊗) (Nagata f df) (Nagata g dg) = Nagata (f ⊗ g) ((f • dg) <> (g • df))

{- ABSTRACTION 3: Kronecker Delta

-}
class Module d e => Kronecker v d e where
  delta :: v -> e

