--------------------------------------------------------------------------------
-- | A data type for arithmetic expressions ------------------------------------
--------------------------------------------------------------------------------
data Expr
  = VarX
  | VarY
  | Sine    Expr
  | Cosine  Expr
  | Average Expr Expr
  | Times   Expr Expr
  | Thresh  Expr Expr Expr Expr
  deriving (Show)

depth :: Expr -> Int
depth VarX = 0
depth VarY = 0
depth (Sine e) = 1 + depth e
depth (Cosine e) = 1 + depth e
depth (Average e1 e2) = 1 + max (depth e1) (depth e2)
depth (Times e1 e2) = 1 + max (depth e1) (depth e2)
depth (Thresh a b c d) = 1 + max (max (depth a) (depth b)) (max (depth c) (depth d))


myExpr :: Expr
-- x y 2 AVG y TIMES SINE COSINE TIMES
myExpr = Times (Average VarX VarY) (Cosine (Times VarX VarY))

