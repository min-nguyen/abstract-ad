{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Reverse where

import AbstractAD
import Prelude hiding (abs, (<>), Monoid)
import Data.Map
import Forward

{-- | ACCUMULATING MULTIPLICATION
   Rather than working directly in E as vectors with scalar multiplication:
     Monoid   (E, <>)
     Semiring (D, ⊕, ⊗)
     Module   (E, •, D)
   We now work with the type Hom D E as homomorphisms from scalars D to vectors E
     Monoid   (D -> E, <>)
     Semiring (D, ⊕, ⊗)
     Module   (D -> E, •, D)
    such that Hom D E is specifically functions (D -> E) with the *multiplicative homogeneity* property:
      f (d1 ⊗ d2) = d1 • f d2         where f :: D -> E

   Intuitively, Hom D E augments a Vector E with an _accumulator_ for a Scalar Multiplier D.
--}
newtype Hom d e = Hom (d -> e)

rep_h :: Module d e => e -> Hom d e
rep_h e = Hom (\d -> d • e)
abs_h :: Module d e => Hom d e -> e
abs_h (Hom f) = f one

-- | Vector addition is still O(n)
instance Monoid e   => Monoid (Hom d e) where
  mzero          = Hom (const mzero)
  Hom f <> Hom g = Hom (\d -> f d <> g d)

-- | Scalar multiplication (•) is O(1) by using semiring multiplication (⊗) instead, accumulating the scalar multiplier in the parameter D in Hom D E
instance {-# OVERLAPPING #-} Module d e => Module d (Hom d e) where
  d • (Hom f)    = Hom (\d' -> f (d ⊗ d'))

-- | Instantiating a basis vector for the general type "Hom D E" is O(n):
      -- 1. We must first instantiate a basis vector of type D-Module E for variable V, generally in O(n).
      -- 2. And then scalar multiply (•) this by an accumulator D, generally in O(n).
instance {-# OVERLAPPABLE #-}
         Kronecker v d e     => Kronecker v d (Hom d e) where
  delta x        = Hom (\d  -> d • delta x)
-- | Instantiating a basis vector for the specific type "Hom D (Sparse V D)" is O(1):
--    1. We can simply return the multiplicative accumulator D as the only map entry, in O(1).
instance (Ord v, Semiring d) => Kronecker v d (Hom d (Sparse v d)) where
  delta x        = Hom (\d -> Sparse (singleton x d))

{-- | SPARSE REVERSE-MODE AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Hom D (Sparse V D)"

      - Primals are scalars D,
      - Tangents Hom D (Sparse V D) are decomposed into:
           (i)  An accumulator D for scalar multiplication
           (ii) A gradient vector (Sparse V D) as a sparse map
--}
reverseAD_Sparse :: (Ord v, Semiring d) => (v -> d) -> Expr v -> d ⋉ Hom d (Sparse v d)
reverseAD_Sparse = abstractAD


{-- | ACCUMULATING ADDITION
    Rather than working directly in E as vectors with element-wise addition:
       Monoid (E, (<>))
    We now work with the type Cayley E as a function representation of vectors with element-wise addition:
       Monoid (E -> E, (<>))
      such that Cayley E is specifically functions (E -> E) with the *additive homogeneity* property:
        f (e1 <> e2) = e1 <> (f e2)    where f :: E -> E
--}
newtype Cayley e = Cayley (e -> e)

rep_c :: Monoid e => e -> Cayley e
rep_c e = Cayley (<> e)
abs_c :: Monoid e => Cayley e -> e
abs_c (Cayley f) = f mzero

-- | Vector addition is O(1), now represented by function composition
instance Monoid e   => Monoid (Cayley e) where
  mzero = Cayley id
  Cayley f <> Cayley g = Cayley (f . g)

-- | Scalar multiplication (•) is O(n):
     -- 1. We must convert the function representation (E -> E) to a vector E
     -- 2. We must scalar multiply this by D, in O(n)
     -- 3. We must convert back the vector E to a function representation (E -> E)
instance Module d e => Module d (Cayley e) where
  d • (Cayley f) -- = rep_c (d • abs_c (Cayley f))
                    = Cayley (\e -> e <> (d • f mzero))
  -- | Note that this is semantically different to the following which does *not* produce a valid Cayley representation:
  --      d • (Cayley f)  = Cayley ((d •) . f)
  --                      = Cayley (\e -> d • (f e))
  --   This is because the result does not have the required additive homogenity property.


-- | Instantiating a basis vector for the general type "Cayley E" is O(n):
     -- 1. We must instantiate a basis vector of type E for variable V, generally in O(n)
     -- 2. And then convert this to a function representation (E -> E)
instance {-# OVERLAPPABLE #-} Kronecker v d e
  => Kronecker v d (Cayley e) where
  delta v = Cayley (<> delta v)
-- | Instantiating a basis vector for the specific type "Cayley (Sparse v d)" is O(log n):
      -- 1. We can simply insert (add) one to the entry for variable V in the prepended map, in O(log n)
instance  (Ord v, Semiring d)
  => Kronecker v d (Cayley (Sparse v d)) where
  delta v  {- Convert to Cayley representation on Sparse V D, where (<>) = unionWith (⊕) -}
           -- = Cayley (<> delta v)
           {- Definition of delta on Sparse maps -}
           -- = Cayley (<> Sparse (singleton v one))
           {- The operation (<>) carries out "unionWith (⊕) m (singleton v one)", which is equivalent to -}
           = Cayley (\(Sparse m) -> Sparse (insertWith (⊕) v one m))
-- | Instantiating a basis vector for the specific type "Hom d (Cayley (Sparse v d))" is O(log n):
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

{-- | CAYLEY REVERSE-MODE AD specialises the Abstract AD to work with Nagata numbers "D ⋉ Hom d (Cayley (Sparse v d))".

    By using the specific type "Hom d (Cayley (Sparse v d))" for gradient vectors:
    1. We get to use the specific instance "Module d (Hom d e)" , where:
        Scalar multiplication (•) is O(1), by accumulating in d
    2. We get to use the specific instance "Monoid (Cayley e)", where:
        Vector addition (<>) is O(1), by replacing it with function composition
    3. We get to use the specific instance "Kronecker v d (Hom d (Cayley (Sparse v d)))", where:
        Instantiating basis vectors (delta) is O(log n), by using "insertWith (⊕)" on the sparse map.
--}
reverseAD_Cayley :: (Ord v, Semiring d) => (v -> d) -> Expr v -> d ⋉ Hom d (Cayley (Sparse v d))
reverseAD_Cayley = abstractAD