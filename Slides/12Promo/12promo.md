% Zaawansowane programowanie funkcyjne
% Marcin Benke
% 4 czerwca 2018
<meta name="duration" content="80" />

# Plan
1. Rodzaje
2. GADT - [https://en.wikibooks.org/wiki/Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
3. Promocja typów - [https://github.com/slindley/dependent-haskell](https://github.com/slindley/dependent-haskel)
<!--
``` {.haskell}
    data Nat = Z | S Nat
    data Vec :: Nat -> * -> * where
    vhead :: Vec (S n) a -> a
```
-->

4. Rodziny typów
``` {.haskell}
   type family (m::Nat) :+ (n::Nat) :: Nat
   vappend :: Vec m a -> Vec n a -> Vec (m :+ n) a
   ? :: Vec(m :+ n) a -> (Vec m a, Vec n a)
```

5. Zależności dynamiczne, singletony

``` {.haskell}
   data Natty :: Nat -> *
   vchop :: Natty m -> Vec (m :+ n) a -> (Vec m a, Vec n a)
   ? :: Natty m -> Vec (m :+ n) a -> Vec m a
```

6. Zależności statyczne, Proxy
```
   data NP :: Nat -> * where NP :: NP n
   vtake1 :: Natty m -> NP n -> Vec (m :+ n) -> Vec m a
```

7. Kind polymorphism
```
   data Proxy :: k -> * where Proxy :: Proxy i
   vtake2 :: Natty m -> Proxy n -> Vec (m :+ n) -> Vec m a 
```

8. TypeApplication

9. Aplikacja wektorowa
```
   type family Arity (n::Nat) (a::*) :: *
   vap :: Arity n a -> Vec n a -> a
```
   
10. Zależności implicite
```
   class NATTY (n::Nat)
   vtrunc :: NATTY m => Proxy n -> Vec (m :+ n) a -> Vec m a
```

11. Biblioteka `singletons`

# Rodzaje (kinds)

* Operacje na wartościach są opisywane przez ich typy

* Operacje na typach są opisywane przez ich rodzaje (kinds)

* Typy (np. `Int`) są rodzaju `*`

* Jednoargumentowe konstruktory (np. `Tree`) są rodzaju `* -> *`

    ~~~~ {.haskell}
    {-#LANGUAGE KindSignatures, ExplicitForAll #-}

    class Functor f => Pointed (f :: * -> *) where
        pure :: forall (a :: *).a -> f a
    ~~~~

* Występują też bardziej złożone rodzaje, np. dla transformatorów monad:

    ~~~~ {.haskell}
    class MonadTrans (t :: (* -> *) -> * -> *) where
        lift :: Monad (m :: * -> *) => forall (a :: *).m a -> t m a
    ~~~~

NB spacje są niezbędne - `::*->*` jest jednym leksemem.

W GHC jest jeszcze rodzaj `#` dla nieopakowanych typów (jak `Int#`)

Jak się przekonamy, użytkownik może wprowadzić więcej rodzajów.

# Expr1

Rozważmy prosty interpreter dla wyrażeń:

``` {.haskell}
data Expr = I Int
          | Add Expr Expr

eval :: Expr -> Int
eval (I n)       = n
eval (Add e1 e2) = eval e1 + eval e2
```

Co jeśli spróbujemy dodać `Bool`?

``` {.haskell}
data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Eq  Expr Expr
```

Jakiego typu ma być `eval`?

# Expr2

``` {.haskell}
data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Eq  Expr Expr

-- eval :: Either Int Bool ?
-- eval (Add (B True) (I 1)) = ?

eval :: Expr -> Maybe (Either Int Bool)
eval (I n)       = Just (Left n)
eval (B n)       = Just (Right n)
eval _ = undefined       -- Exercise
```

Jak sprawić żeby typechecker odrzucił `eval (Add (B True) (I 1))` ?

# Expr3 - Phantom types

```  {.haskell}
data Expr a = I Int
            | B Bool
            | Add (Expr Int) (Expr Int)
            | Eq  (Expr Int) (Expr Int)

  
eval :: Expr a -> a
eval (I n)       = n -- error
```

mamy

```
I :: Int -> Expr a
B :: Bool -> Expr a
Add :: Expr Int -> Expr Int -> Expr a
```
chcemy

```
I :: Int -> Expr Int
B :: Bool -> Expr Bool
Add :: Expr Int -> Expr Int -> Expr Int
```

# GADTs - Generalised Abstract Data Types

```
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool
  -- exercise: allow comparing booleans, e.g `Eq (B True) (B True)`
  
eval :: Expr a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

deriving instance Show (Expr a)
```

# Vec

Przypomnijmy przykład z pierwszego wykładu:

``` {.haskell}
data Zero
data Succ n

data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> xs) = x
```

Jeśli mamy już typ danych `Nat`, możemy automagicznie wygenerować odpowiednie typy, np.

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

``` {.haskell}
-- This defines
-- Type Nat
-- Value constructors: Z, S

-- Promotion (lifting) to type level yields
-- kind Nat
-- type constructors: 'Z :: Nat; 'S :: Nat -> Nat
-- 's can be omitted in most cases, but...

-- data P          -- 1
-- data Prom = P   -- 2
-- type T = P      -- 1 or promoted 2?
-- quote disambiguates:
-- type T1 = P     -- 1
-- type T2 = 'P    -- promoted 2
```

# Inne przykłady promocji

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

# Konkatenacja

``` {.haskell}
class (Nat a, Nat b) => Add a b c | a b -> c where
  add :: a -> b -> c
  add = undefined
instance Nat b =>     Add  Zero    b  b
instance Add a b c => Add (Succ a) b (Succ c)

vappend :: (Nat m, Nat n, Add m n s) => Vec m a -> Vec n a -> Vec s a
vappend Vnil ys = ys
```

niestety...

```
error: …
    • Could not deduce: n ~ s
      from the context: m ~ Zero
```

Kompilator ''nie wie'', że jeśli `m = 0` to `s = n`

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

``` {.haskell}
-- atIndex :: Vec n a -> (m < n) -> a

data Fin n where
    FinZ :: Fin (S n) -- zero is less than any successor
    FinS :: Fin n -> Fin (S n) -- n is less than (n+1)

atIndex :: Vec n a -> Fin n -> a
atIndex (Vcons x xs) FinZ = x
atIndex (Vcons x xs) (FinS k) = atIndex xs k
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

# vchop

Chcemy funkcję odwrotną do `vappend`:

``` {.haskell}
--| chop a vector in two
vchop1 :: Vec (m :+ n) a -> (Vec m a, Vec n a)
vchop1 _ _ = undefined
```

Czy umiemy chociaż napisać doctest?

Potrzebujemy wartości `m`

``` {.haskell}
-- | Chop a vector in two, using first argument as a measure
-- >>> vchop2 (Vcons undefined Vnil) (Vcons 1 (Vcons 2 Vnil))
-- (Vcons 1 Vnil,Vcons 2 Vnil)
vchop2 :: Vec m x -> Vec (m :+ n) a -> (Vec m a, Vec n a)
vchop2 Vnil xs = (Vnil, xs)
vchop2 (Vcons _ m) (Vcons x xs) = (Vcons x ys, zs) where
  (ys, zs) = vchop2 m xs
```

# Singleton
Nie potrzebujemy całego wektora, a tylko jego długości.

Ale typ Nat jest za mało precyzyjny, mamy `Z :: Nat`, chielibyśmy `Zero :: SNat Z` 

Pomysł: stwórzmy po jednym reprezentancie każdego z typów rodzaju Nat

``` {.haskell}
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
# Porównania i inny sposób indeksowania wektora

```
-- # Comparison

type family (m::Nat) :< (n::Nat) :: Bool
type instance m :< 'Z = 'False
type instance 'Z :< ('S n) = 'True
type instance ('S m) :< ('S n) = m :< n

-- nth
nth :: (m:<n) ~ 'True => SNat m -> Vec n a -> a
nth SZ (a:>_)  = a
nth (SS m') (_:>xs) = nth m' xs
```

# Bilioteka singletons

# vchop3

``` {.haskell}
-- | chop a vector in two parts
-- >>> vchop (SS SZ) (Vcons 1 (Vcons 2 Vnil))
-- (Vcons 1 Vnil,Vcons 2 Vnil)
vchop = vchop3
vchop3 :: SNat m -> Vec(m:+n) a -> (Vec m a, Vec n a)
vchop3 SZ xs = (Vnil, xs)
vchop3 (SS m) (Vcons x xs) = (Vcons x ys, zs) where
  (ys,zs) = vchop3 m xs
```

# vreplicate

``` {.haskell}
-- | `vreplicate n a` is a vector of n copies of a
-- >>> vreplicate (SS SZ) 1
-- Vcons 1 Vnil
-- >>> vreplicate (SS (SS SZ)) 1
-- Vcons 1 (Vcons 1 Vnil)
vreplicate :: SNat n -> a -> Vec n a
vreplicate SZ _ = Vnil
vreplicate (SS n) x = Vcons x (vreplicate n x)
```

**Ćwiczenie:** zdefiniować

``` {.haskell}
vcycle :: SNat n -> Vec m a -> Vec (n:*m) a
```

# vtake

Chcemy zdefiniować analog funkcji `take`
``` {.haskell}
{-# LANGUAGE AllowAmbiguousTypes #-}

vtake1 :: SNat m -> Vec (m :+ n) -> Vec m x
vtake1  SZ     xs     = V0
vtake1 (SS m) (x:>xs) = x :> vtake1 m xs
```

``` {.error}
error: …
    • Could not deduce: (n1 :+ n0) ~ n2
      from the context: m ~ 'S n1
        bound by a pattern with constructor:
                   SS :: forall (n :: Nat). SNat n -> SNat ('S n),
                 in an equation for ‘vtake1’
```

Kompilator nie potrafi otypować przypadku rekurencyjnego.
Problem w tym  czy `(m :+)` jest różnowartościowe. NB żeby w ogóle spróbował musieliśmy użyć `AllowAmbiguousTypes`

# Różnowartościowość (injectivity)

`Maybe a ~ Maybe b => a ~ b`

ale trudniej ustalić, czy

`m :+ n0 ~ m :+ n1 => n0 ~ n1`

Konkretnie w typie

`vtake1 :: SNat m -> Vec (m :+ n) -> Vec m x`

brakuje nam "uchwytu" do `n`;  w "prawdziwych" typach zależnych napisalibyśmy

```
(m : Nat) -> (n : Nat) -> Vec (m + n) x -> Vec m x
```

# Proxy czyli uchwyt

Spróbujmy sobie skonstruować odpowiedni uchwyt:

``` haskell
-- | Nat Proxy
data NP :: Nat -> * where NP :: NP n

-- >>> let v = 1 :> (1 :> (1 :> V0)); two = SS(SS SZ) in vtake2 two NP v
-- 1 :> (1 :> V0)
vtake2 :: SNat m -> NP n -> Vec (m :+ n) a -> Vec m a
vtake2 SZ     _ _ = V0
vtake2 (SS m) n (x:>xs) = x :> vtake2 m n xs
```

# Uniwersalny uchwyt

``` haskell
-- | Nat Proxy
data NP :: Nat -> * where NP :: NP n
```

 nie widać powodu dlaczego uchwyt miałby zależeć od `Nat`, może być polimorficzny
 
``` haskell
{-# LANGUAGE PolyKinds #-}
-- | Generic Proxy
data Proxy :: k -> * where
  Proxy :: Proxy i

-- >>> let v = 1 :> (1 :> (1 :> V0)); two = SS(SS SZ) in vtake3 two Proxy v
-- 1 :> (1 :> V0)
vtake3 :: SNat m -> Proxy n -> Vec (m :+ n) a -> Vec m a
vtake3 SZ     _ _ = V0
vtake3 (SS m) n (x:>xs) = x :> vtake3 m n xs
```

**Uwaga:** `k` jest zmienną rodzajową, stąd potrzeba rozszerzenia `PolyKinds`

# Back to the future or another take on vtake1

GHC od wersji 8.0 umożliwa jawną apliację typową, np.

```
Prelude> :set -XTypeApplications
Prelude> :t read
read :: Read a => String -> a
Prelude> read @Int "42"
42
Prelude> read @Double "42"
42.0
```

``` haskell
-- vtake4 requires:
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-} -- GHC>=8.0

-- >>> let v = 1 :> (1 :> (1 :> V0)); two = SS(SS SZ) in vtake4 two v
-- 1 :> (1 :> V0)
vtake4 :: forall n m a. SNat m -> Vec (m :+ n) a -> Vec m a
vtake4 SZ _ = V0
vtake4 (SS m) (x:>xs) = x :> vtake4 @n m xs
```
