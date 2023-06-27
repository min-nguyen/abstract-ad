{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Documentation.Background where

-- | 'Expr v' is a symbolic expression that captures polynomials over variables 'v'
data Expr v = Var v | Zero | One | Plus (Expr v) (Expr v) | Times (Expr v) (Expr v)

-- | The type 'X' for a single variable
data X = X deriving Eq

-- | x * (x + 1)
example_1 :: Expr X
example_1 = Times (Var X) (Plus (Var X) One)

{-- Semiring
--}
class Semiring d where
  zero  :: d
  one   :: d
  (⊕)  :: d -> d -> d
  (⊗)  :: d -> d -> d

-- | All numbers are semirings
instance Num a => Semiring a where
  zero  = 0
  one   = 1
  (⊕)  = (+)
  (⊗)  = (*)

-- | The "free" semiring instance
instance {-# OVERLAPPING #-} Semiring (Expr v) where
  zero = Zero
  one  = One
  (⊕) = Plus
  (⊗) = Times

-- | The function 'eval var :: Expr v -> d' is a (semiring) homomorphism for any choice of 'var'.
--   It interprets a symbolic expression to a semiring d, given a mapping from variables v to d.
eval :: Semiring d => (v -> d) -> Expr v -> d
eval var (Var x)       = var x
eval var Zero          = zero
eval var One           = one
eval var (Plus e1 e2)  = eval var e1 ⊕ eval var e2
eval var (Times e1 e2) = eval var e1 ⊗ eval var e2

-- | Evaluate 'x * (x + 1)' for x = 5 with the Int semiring
run_example_1 :: Int
run_example_1 = eval (\X -> 5) example_1

{-- Symbolic differentation
--}

-- | The following function correctly expresses symbolic differentation,
--   but cannot be defined in terms of eval (structural recursion) because of the case of Times
symbolicDeriv :: Eq v => v -> Expr v -> Expr v
symbolicDeriv x (Var y)         =  if x == y then One else Zero
symbolicDeriv x Zero            =  Zero
symbolicDeriv x One             =  Zero
symbolicDeriv x (Plus   e1 e2)  =  symbolicDeriv x e1 ⊕ symbolicDeriv x e2
symbolicDeriv x (Times  e1 e2)  =
  -- | e2 and e1 appear *outside* of recursive calls
  (e2 ⊗ symbolicDeriv x e1) ⊕ (e1 ⊗ symbolicDeriv x e2)

-- | By defining symbolic diff to also return the original expression (thus becoming automatic differentation):
--    symbolicDeriv' x e = (e, symbolicDeriv x e)
--   We can redefine the case of Times to be structurally recursive
symbolicDeriv' :: Eq v => v -> Expr v -> (Expr v, Expr v)
symbolicDeriv' x (Var y)         =  (Var y,  if x == y then One else Zero)
symbolicDeriv' x Zero            =  (Zero,   Zero)
symbolicDeriv' x One             =  (One,    Zero)
symbolicDeriv' x (Plus   e1 e2)  =  let  (e1', de1)  =  symbolicDeriv' x e1
                                         (e2', de2)  =  symbolicDeriv' x e2
                                    in   (e1' ⊕ e2', de1 ⊕ de2)
symbolicDeriv' x (Times  e1 e2)  =
  let (e1', de1)  =  symbolicDeriv' x e1
      (e2', de2)  =  symbolicDeriv' x e2
  -- | e1' and e2' are results of recursive calls, allowing symbolicDeriv' to be defined in terms of eval
  in  (e1' ⊗ e2', (e2' ⊗ de1) ⊕ (e1' ⊗ de2))

{-- Dual numbers
--}

-- | A dual number is a tuple whose components are drawn from an arbitrary semiring,
--   Representing
data Dual d = D { primal :: d, tangent :: d } deriving Functor

-- | Any semiring d gives rise to a semiring structure Dual d, whose semiring operations
--   precisely follow the definition of symbolicDeriv'.
instance {-# OVERLAPPING #-} Semiring d => Semiring (Dual d) where
  zero        = D zero zero
  one         = D one zero
  (D f df) ⊕ (D g dg)  = D (f ⊕ g) (df ⊕ dg)
  (D f df) ⊗ (D g dg) = D (f ⊗ g) ((g ⊗ df) ⊕ (f ⊗ dg))

-- | Symbolic differentiation, also as an instance of eval like symbolicDeriv',
--   but now defined in terms of eval & into the semiring of dual numbers over symbolic expressions.
symbolicDeriv'' :: Eq v => v -> Expr v -> Dual (Expr v)
symbolicDeriv'' x = eval gen
  where
    gen y = D (Var y) (ddx y)
    ddx y = if x == y then one else zero

{-- | Forward AD
--}

-- | AD algorithms are those that compute 1) both the value and derivative of an expression, 2) at a point.
--   Forward AD is hence definable by composing:
--       1) symbolic differentation (into the semiring  Dual (Expr v))
--   and 2) evaluation of symbolic expressions (Expr v) into a "numeric" semiring d.
forwardAD :: (Semiring d, Eq v) => (v -> d) -> v -> Expr v -> Dual d
forwardAD var x = fmap (eval var)      -- | evaluation (Expr v -> d) at a point into a semiring d
                . symbolicDeriv'' x    -- | symbolic differentiation into the semiring Dual (Expr v)

-- | Alternatively written like:
forwardAD' :: (Semiring d, Eq v) => (v -> d) -> v -> Expr v -> Dual d
forwardAD' var x = eval gen
  where
    gen y = D (var y) (ddx y)
    ddx y = if x == y then one else zero

run_forwardAD :: Dual Int
run_forwardAD = forwardAD' (\X -> 5) X example_1

{-- | Symbolic AD from Forward AD
--}

-- | Symbolic evaluates into the symbolic semiring Expr v,
-- | Automatic evaluates into a numeric semiring d

-- | We can thus recover Symbolic from ForwardAD, by interpreting variables with Var :: v -> Expr v
symbolicDeriv''' :: Eq v => v -> Expr v -> Dual (Expr v)
symbolicDeriv''' = forwardAD Var