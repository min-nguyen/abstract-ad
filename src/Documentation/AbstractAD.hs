{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Documentation.AbstractAD where

import Prelude hiding (Monoid(..), (<>))

{-- | FOUNDATION: Monoids, semirings, and expressions.
--}

{- | A Monoid (E, <>).
   For the purposes of dual numbers, (<>) is interpreted additively (⊕) rather than multiplicatively.
-}
class Monoid e where
  mzero :: e
  (<>)  :: e -> e -> e

{- | A Semiring (D, ⊕, ⊗) is two monoids of the same underlying type. These are used to represent scalar numbers.
    1. An additive monoid       (D, ⊕) which is also commutative.
    2. A  multiplicative monoid (D, ⊕).
-}
class Semiring d where
  zero :: d
  one  :: d
  (⊕)  :: d -> d -> d
  (⊗)  :: d -> d -> d

-- | All numbers are Semirings
instance {-# OVERLAPPABLE #-} Num a => Semiring a where
  zero  = 0
  one   = 1
  (⊕)  = (+)
  (⊗)  = (*)

{- | Expr v is a symbolic expression that captures polynomials over variables 'v'.
-}
data Expr v = Var v | Zero | One | Plus (Expr v) (Expr v) | Times (Expr v) (Expr v) deriving (Eq)
data XY = X | Y deriving (Eq, Ord, Show)

instance Show a => Show (Expr a) where
  showsPrec p (Var x)  =  showsPrec p x
  showsPrec p Zero     =  shows 0
  showsPrec p One      =  shows 1
  showsPrec p (Plus e1 e2)   =  showParen (p >= 6) $ (showsPrec 6 e1) . (" + " ++) . (showsPrec 6 e2)
  showsPrec p (Times e1 e2)  =  showParen (p >= 7) $ (showsPrec 7 e1) . (" * " ++) . (showsPrec 7 e2)

example :: Expr XY
example = Times (Var X) (Plus (Var X) One)    -- x * (x + 1)

instance Monoid (Expr v) where
  mzero = Zero
  (<>)  = Plus

instance Semiring (Expr v) where
  zero  = Zero
  one   = One
  (⊕)   = Plus
  (⊗)   = Times

{- | The function 'eval var :: Expr v -> d' is a (semiring) homomorphism for any choice of variable mapping
     'var :: v -> d', interpreting a symbolic expression to a semiring d.
-}
eval :: Semiring d => (v -> d) -> Expr v -> d
eval var (Var x)       = var x
eval var Zero          = zero
eval var One           = one
eval var (Plus e1 e2)  = eval var e1 ⊕ eval var e2
eval var (Times e1 e2) = eval var e1 ⊗ eval var e2

-- | Evaluate 'x * (x + 1)' for x = 5 to the Int semiring
eval_example :: Int
eval_example = eval (\X -> 5) example

{-- | ABSTRACTION 1: Modules E over Semirings D
--}

{- | A D-Module (E, <>) is:
     1. A Monoid   (E, <>)   of Vectors, where <> is additive and commutative.
     2. A Semiring (D, ⊕, ⊗) of Scalars.
     3. Scalar multiplication (•) that distributes over (<>).
          (•) :: d -> e -> e
-}
class (Semiring d, Monoid e) => Module d e | e -> d where -- Knowing the Vector type E determines the Scalar type D.
  (•) :: d -> e -> e

{- | Every Semiring (D, ⊕, ⊗) is trivially a D-Module, by setting scalar multiplication to be multiplication.
     Intuitively, if in Module D E the "vector" E is infact a scalar like a Double, then the scalar D must be a Double.
      instance (Semiring d, Monoid d) => Module d d where
        (•) :: d -> d -> d
        (•) = (⊗)
    The above needs to be commented out to avoid problems with functional dependencies, otherwise Haskell forces E and D
    to always be the same and E hence must always be a semiring (scalar).
    However, we can write concretes instance for each possible case where E is itself a semiring:
      instance (Monoid Int, Semiring Int) => Module Int Int where
        (•) = (⊗)
-}

{-- | ABSTRACTION 2: Nagata Numbers D ⋉ E

   AD algorithms are those that compute Dual numbers (D, D) that contain both the value and  partial derivative (tangent)
   of an expression at a point.
   The type Nagata D E generalises over dual numbers (D, D) by letting the primal and tangent have different types.
   Intuitively:
    D is a scalar representing the result of evaluation
    E is a vector representing the gradient
--}
data d ⋉ e = Nagata { primal :: d, tangent :: e } deriving (Functor, Show)

{- |  Every D-Module E admits a *Semiring* (D ⋉ E, ⊕, ⊗) of Nagata numbers
-}
instance Module d e => Semiring (d ⋉ e) where
  zero = Nagata zero mzero
  one  = Nagata one mzero
  (⊕) (Nagata f df) (Nagata g dg) = Nagata (f ⊕ g) (df <> dg)
  (⊗) (Nagata f df) (Nagata g dg) = Nagata (f ⊗ g) ((f • dg) <> (g • df))


{- ABSTRACTION 3: Kronecker Delta
    Delta computes the partial derivatives E (as a basis vector) of all variables V wrt a specific variable.
  For example, in the module R^3 of real numbers R comprised of variables x_i:
    delta (x_0) = [1, 0, 0], delta (x_1) = [0, 1, 0], delta (x_2) = [0, 0, 1]
-}
class Module d e => Kronecker v d e where
  delta :: v -> e

{-- | ABSTRACT AD:
--}
abstractAD :: Kronecker v d e => (v -> d) -> Expr v -> d ⋉ e
abstractAD var = eval gen where
  gen x = Nagata (var x) (delta x) -- | turn each variable V into a Nagata number D ⋉ E

