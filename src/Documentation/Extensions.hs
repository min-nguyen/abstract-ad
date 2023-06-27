module Documentation.Extensions where

import Documentation.AbstractAD
import Documentation.Forward

derive2nd :: (Eq v, Semiring d) => (v -> d) -> Expr v -> Dense v (Dense v d)
derive2nd var e = 
  let f = forwardAD_Dense var
  
  in undefined -- fmap (fmap (eval var) . tanN . forwardADDense Var) . tanN . 
