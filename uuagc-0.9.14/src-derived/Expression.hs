

-- UUAGC 0.9.14 (Expression.ag)
module Expression where

import UU.Scanner.Position(Pos)
import HsToken
-- Expression --------------------------------------------------
{-
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
data Expression  = Expression (Pos) ([HsToken]) 