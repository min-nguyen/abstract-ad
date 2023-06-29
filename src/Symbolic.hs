{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Symbolic where

import AbstractAD
import Forward hiding (forwardAD_Dense)

{-- Automatic Differentiation computes the value and derivative of an expression *at a point*, producing numeric values.
    Symbolic differentation computes the value and derivative as symbolic mathematical expressions.
      There is an isomorphism

    Below is the basic non-abstract implementation, evaluating an expression into its symbolic derivative wrt a concrete variable x.
      symbolicDeriv :: Eq v => v -> Expr v -> Expr v ⋉ Expr v
      symbolicDeriv x (Var y)         =  Nagata (Var y) (if x == y then One else Zero)
      symbolicDeriv x Zero            =  Nagata Zero Zero
      symbolicDeriv x One             =  Nagata One Zero
      symbolicDeriv x (Plus   e1 e2)  =
        let Nagata e1' de1  =  symbolicDeriv x e1
            Nagata e2' de2  =  symbolicDeriv x e2
        in  Nagata (e1' ⊕ e2') (de1 ⊕ de2)
      symbolicDeriv x (Times  e1 e2)  =
        let Nagata e1' de1  =  symbolicDeriv x e1
            Nagata e2' de2  =  symbolicDeriv x e2
        in  Nagata (e1' ⊗ e2') ((e2' ⊗ de1) ⊕ (e1' ⊗ de2))
--}

{-- |  Dense Symbolic differentiation evaluates an Expr V into dual (Expr V, V -> Expr V) of symbolic semiring Expr V.
       The tangent (V -> Expr V) returns the symbolic derivative of an expression wrt to a supplied variable V.
--}
symbolic_Dense :: Eq v => Expr v -> Expr v ⋉ (Dense v (Expr v))
symbolic_Dense = abstractAD Var

symbolic_example :: Expr XY ⋉ (Expr XY)
symbolic_example = Nagata sym_e (sym_de X) where
  Nagata sym_e (Dense sym_de) = symbolic_Dense example

{-- |  We can recover Forward AD from Symbolic Diff, by applying a mapping "var :: V -> D" that evaluates
       the Symbolic output into the Numeric Semiring.
          1) differentation into the "symbolic" semiring (V -> Expr V)
          2) providing a concrete variable V to differentiate wrt, to yield the symbolic derivative Expr V
          3) evaluating the symbolic derivative (Expr v) into a "numeric" semiring D, by providing a mapping
             of values V -> D at the point being differentiated.
--}
forwardAsSymbolic :: (Semiring d, Eq v) => (v -> d) -> Expr v -> d ⋉ (Dense v d)
forwardAsSymbolic var e = Nagata y dy where
    Nagata sym_e (Dense sym_de) = symbolic_Dense e      -- | compute symbolic expression and symbolic derivative
    y    = eval var sym_e                               -- | evaluate symbolic expression at a point "var"
    dy   = Dense (\x -> eval var (sym_de x))            -- | evaluate symbolic derivative wrt variable "x" at a point "var"
    {-- Instantiating Dense Forward AD for a single concrete variable x.
          forwardAD :: (Semiring d, Eq v) => (v -> d) -> Expr v -> v -> d ⋉ d
          forwardAD var e x = fmap (\(Dense dy) -> dy x) (forwardAD_Dense var e)
    --}

forward_example :: Double ⋉ Double
forward_example = Nagata y (dy X) where              -- | evaluate symbolic derivative of example wrt X at the point (X = 5)
  Nagata y (Dense dy) = forwardAsSymbolic (\X -> 5) example

{-- |  We can recover Symbolic Diff from Forward AD by providing it
       the variable mapping "Var :: V -> Expr V" into the Symbolic Semiring.
--}
symbolicAsForward :: Eq v => Expr v -> Expr v ⋉ (Dense v (Expr v) )
symbolicAsForward = forwardAsSymbolic Var