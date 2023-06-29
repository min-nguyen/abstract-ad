{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Dist where
import Simple.ForwardAD
import           Numeric.MathFunctions.Constants
import           Numeric.SpecFunctions


class ContDist d a | d -> a where
  draw   :: d -> a -> a
  logpdf :: d -> a -> a

data Normal a where
  Normal :: a -> a -> Normal a

instance ContDist (Normal Double) Double where
  draw :: Normal Double -> Double -> Double
  draw (Normal μ σ) r =  (- invErfc (2 * r)) * (m_sqrt_2 * σ) + μ

  logpdf :: Normal Double -> Double -> Double
  logpdf (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

instance ContDist (Normal (Nagata v Double)) (Nagata v Double) where
  -- draw :: Normal (Nagata v Double) -> Double -> (Nagata v Double)
  -- draw (Normal (N μ dμ) (N σ dσ)) (N r dr) =
  --   N ((- invErfc (2 * r)) * (m_sqrt_2 * σ) + μ)
  --     undefined

  logpdf :: Normal (Nagata v Double) -> (Nagata v Double) -> (Nagata v Double)
  logpdf (Normal (N μ dμ) (N σ dσ)) (N x dx) =
    N (logpdf (Normal μ σ) x) undefined
    where xμ = x - μ

linRegr :: [Double] -> [Double] -> Double -> Double -> a
linRegr xs ys = \m c ->
  do  let ys = map (\x -> draw (Normal (m * x + c) 1.0)) xs
      undefined