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

# Indeksowanie

``` haskell
-- |
-- Indexing
-- >>> (1:>V0) `atIndex` FinZ
-- 1
--
-- atIndex :: Vec n a -> (m < n) -> a

data Fin n where
    FinZ :: Fin ('S n) -- zero is less than any successor
    FinS :: Fin n -> Fin ('S n) -- n is less than (n+1)

atIndex :: Vec n a -> Fin n -> a
atIndex (x:>_) FinZ = x
atIndex (_:>xs) (FinS k) = atIndex xs k

-- Question - why not:
-- atIndex :: Vec (S n) a -> ... ?
```

# Replicate

Spróbujmy stworzyć analog funkcji `replicate :: Int -> a -> [a]`

``` {.haskell}
vreplicate :: Nat -> a -> Vec n a
vreplicate Z _ = Vnil -- fail on oh, so many levels
```

dokładniej

``` {.haskell}
vreplicate2 :: (n::Nat) -> a -> Vec n a
```

...ale nie ma wartości typu rodzaju `Nat`

*Ćwiczenie:* wypróbować różne varianty `vreplicate`

# Singleton

``` {.haskell}
data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
deriving instance Show(SNat n)

add :: (SNat m) -> (SNat n) -> SNat(m :+ n)
add SZ n = n
add (SS m) n = SS (add m n)
```
**Ćwiczenie:** zdefiniować mnożenie
``` {.haskell}
mul :: (SNat m) -> (SNat n) -> SNat(m :* n)
```

**Ćwiczenie:** zdefiniować

``` {.haskell}
vcycle :: SNat n -> Vec m a -> Vec (n:*m) a
```
