# GADTs - Generalised Abstract Data Types

```
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool
  
eval :: Expr a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

deriving instance Show (Expr a)
```

**Exercise:** allow comparing booleans, e.g `Eq (B True) (B True)`

# HList

```
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

data Tuple :: (*,*) -> * where
  Tuple :: a -> b -> Tuple '(a,b)

foo0 :: HList '[]
foo0 = HNil

foo1 :: HList '[Int]
foo1 = HCons (3::Int) HNil

foo2 :: HList [Int, Bool]
foo2 = undefined  -- (easy) exercise
```

# Vec

``` {.haskell}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat

data Vec :: Nat -> * -> * where
  Vnil :: Vec 'Z a
  Vcons :: a -> Vec n a -> Vec ('S n) a

vhead :: Vec (S n) a -> a
-- Jakiego typu ma być vtail?
```

# Rodziny typów

``` {.haskell}
type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance (S n) :+ m = S (n :+ m)

vapp :: Vec m a -> Vec n a -> Vec (m :+ n) a
vapp Vnil ys = ys
vapp (Vcons x xs) ys = Vcons x (vapp xs ys)
```

**Ćwiczenie:** zdefiniować mnożenie
``` {.haskell}
type family (n :: Nat) :* (m :: Nat) :: Nat
```

