{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reverse where

import AbstractAD
import Prelude hiding (abs, (<>), Monoid)
import Data.Map
import Forward

{- ACCUMULATING MULTIPLICATION

   Rather than working directly with Module D E:
     "Module E over Semiring D"
   we now work with the isomorphic type Hom D E:
     "Homomorphisms from Semiring D (scalars) to Module E (vectors)".
   Intuitively, Hom D E augments a Vector E with an _accumulator_ for a Scalar Multiplier D.
-}
newtype Hom d e = Hom (d -> e)

-- | Vector addition is still linear
instance Monoid e   => Monoid (Hom d e) where
  mzero          = Hom (const mzero)
  Hom f <> Hom g = Hom (\d -> f d <> g d)

-- | Scalar multiplication (•) now _accumulates_ in the function parameter D in Hom (D -> E),
--   using the cheaper semiring multiplication (⊗), in O(1).
instance {-# OVERLAPPING #-} Module d e => Module d (Hom d e) where
  d • (Hom f)    = Hom (\d' -> f (d ⊗ d'))

-- | To instantiate a basis vector for the general type "Hom D E" for any tangent E:
--    1. We must first instantiate a basis vector of type D-Module E for variable V, generally in O(n).
--    2. And then scalar multiply (•) this by an accumulator D, generally in O(n).
instance {-# OVERLAPPABLE #-}
         Kronecker v d e     => Kronecker v d (Hom d e) where
  delta x        = Hom (\d  -> d • delta x)
-- | To instantiate a basis vector for the specific type "Hom D (Sparse V D)" that represents the tangent as a sparse map:
--    1. We can simply return the multiplicative accumulator D as the only map entry, in O(1).
instance (Ord v, Semiring d) => Kronecker v d (Hom d (Sparse v d)) where
  delta x        = Hom (\d -> Sparse (singleton x d))

{- | Sparse Reverse AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Hom D (Sparse V D)"
      - Primals are scalars D,
      - Tangents Hom D (Sparse V D) are decomposed into:
           1. An accumulator D for scalar multiplication
           2. A gradient vector (Sparse V D) as a sparse map
-}
reverseAD_Sparse :: (Ord v, Semiring d) => (v -> d) -> Expr v -> d ⋉ Hom d (Sparse v d)
reverseAD_Sparse = abstractAD

{- ACCUMULATING ADDITION

  Rather than working directly in E as vectors that add:
     Monoid (E, <>)
  We now work with the Cayley representation of E as functions that compose:
     Monoid (Cayley E, .)
-}
newtype Cayley e = Cayley (e -> e)

rep :: Monoid e => e -> Cayley e
rep e = Cayley (<> e)
abs :: Monoid e => Cayley e -> e
abs (Cayley f) = f mzero

-- | Vector addition is now represented by function composition which is O(1)
instance Monoid e   => Monoid (Cayley e) where
  mzero = Cayley id
  Cayley f <> Cayley g = Cayley (f . g)

-- | To scalar multiply the general type "Cayley E":
--    1. We must convert the function representation (E -> E) to a vector E
--    2. We must scalar multiply this by D, in O(n)
--    3. We must convert back the vector E to a function representation (E -> E)
instance Module d e => Module d (Cayley e) where
  d • (Cayley f) -- = rep (d • abs (Cayley f))
                    = Cayley (<> (d • f mzero))
  -- | Why can't we just write:
  --  d • (Cayley f)            = Cayley ((d •) . f)
  --  abs (Cayley ((d •) . f))  = d • (f mzero)
                            --  = abs ((d •) .)

-- | To instantiate a basis vector for the general type "Cayley E":
--    1. We must instantiate a basis vector of type E for variable V, generally in O(n)
--    2. And then convert this to a function representation (E -> E)
instance {-# OVERLAPPABLE #-} Kronecker v d e
  => Kronecker v d (Cayley e) where
  delta v = Cayley (<> delta v)
-- | To instantiate a basis vector for the specific type "Cayley (Sparse v d)":
--    1. We can simply insert (add) one to the entry for variable V in the prepended map, in O(log n)
instance  (Ord v, Semiring d)
  => Kronecker v d (Cayley (Sparse v d)) where
  delta v  {- Convert to Cayley representation on Sparse V D, where (<>) = unionWith (⊕) -}
           -- = Cayley (<> delta v)
           {- Definition of delta on Sparse maps -}
           -- = Cayley (<> Sparse (singleton v one))
           {- The operation (<>) carries out "unionWith (⊕) m (singleton v one)", which is equivalent to -}
           = Cayley (\(Sparse m) -> Sparse (insertWith (⊕) v one m))
-- | To instantiate a basis vector for the specific type "Hom d (Cayley (Sparse v d))":
--    1. We can simply insert (add) the accumulated scalar multiplier to the entry for variable V in the prepended map, in O(log n)
instance  (Ord v, Semiring d)
  => Kronecker v d (Hom d (Cayley (Sparse v d))) where
  delta v  {- Convert to Cayley representation on Sparse V D, where (<>) = unionWith (⊕) -}
           -- = Hom (\d -> d • Cayley (<> delta v))
           {- Definition of delta on Sparse maps -}
           -- = Hom (\d -> d • Cayley (<> Sparse (singleton v one))
           {- The operation (<>) carries out "unionWith (⊕) m (singleton v one)", which is equivalent to -}
           -- = Hom (\d -> d • Cayley (\(Sparse m) -> Sparse (insertWith (⊕) v one m)))
           {- d • one = d -}
           = Hom (\d -> Cayley (\(Sparse m) -> Sparse (insertWith (⊕) v d m)))

{- | By using the type composition "Hom d (Cayley (Sparse v d))"
  1.
-}