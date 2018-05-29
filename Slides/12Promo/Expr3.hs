data Expr a = I Int
            | B Bool
            | Add (Expr Int) (Expr Int)
            | Eq  (Expr Int) (Expr Int)

  
eval :: Expr a -> a
-- eval (I n)       = n -- error
eval = undefined
