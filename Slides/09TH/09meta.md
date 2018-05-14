---
title: Zaawansowane programowanie funkcyjne
subtitle: Template Haskell
author:  Marcin Benke
date: 15 maja 2018
---

# Metaprogramowanie - Template Haskell

Interaktywne tutoriale na  [School of Haskell](https://www.schoolofhaskell.com/) (SoH jest w trakcie migracji chwilowo nie działa dobrze)

* [Template Haskell](https://www.schoolofhaskell.com/user/marcin/template-haskell-101)

* Code/TH/Here

* Code/TH/Projections

* [Quasiquotation](https://www.fpcomplete.com/user/marcin/quasiquotation-101)

* Code/TH/QQ

# Problem: wieloliniowe napisy


``` {.haskell}
showClass :: [Method] -> String
showClass ms = "\
\.class  public Instant\n\
\.super  java/lang/Object\n\
\\n\
\;\n\
\; standard initializer\n\
\.method public <init>()V\n\
\   aload_0\n\
\   invokespecial java/lang/Object/<init>()V\n\
\   return\n\
\.end method\n" ++ unlines (map showMethod ms)
```

# Template Haskell

Wieloliniowe napisy w Haskellu wg  Haskell Wiki:

```
{-# LANGUAGE QuasiQuotes #-}
module Main where
import Str
 
longString = [str|This is a multiline string.
It contains embedded newlines. And Unicode:
 
Ἐν ἀρχῇ ἦν ὁ Λόγος
 
It ends here: |]
 
main = putStrLn longString
   
```

``` {.haskell}
module Str(str) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
str = QuasiQuoter { quoteExp = stringE }
```

Spróbujmy zrozumieć o co chodzi...

# Parsowanie kodu w trakcie wykonania

Ten [tutorial](http://www.hyperedsoftware.com/blog/entries/first-stab-th.html) poleca eksperymenty w GHCi:

```
$ ghci -XTemplateHaskell

> :m +Language.Haskell.TH


> runQ [| \x -> 1 |]

LamE [VarP x_0] (LitE (IntegerL 1))

> :t it
it :: Exp

> runQ [| \x -> x + 1 |]  >>= putStrLn . pprint
\x_0 -> x_0 GHC.Num.+ 1
```

# Q, runQ

```
> :t [| \x -> 1 |]
[| \x -> 1 |] :: ExpQ
> :i ExpQ
type ExpQ = Q Exp 	-- Defined in ‘Language.Haskell.TH.Lib’

> :i Q
newtype Q a = ... -- Defined in ‘Language.Haskell.TH.Syntax’
instance Monad Q 
instance Functor Q

> :t runQ
runQ :: Language.Haskell.TH.Syntax.Quasi m => Q a -> m a

>: i Quasi
class MonadFail m => Quasi (m :: * -> *) where ...
instance Quasi Q 
instance Quasi IO
```

# Wklejanie drzew struktury do programu

```
> runQ [| succ 1 |]
AppE (VarE GHC.Enum.succ) (LitE (IntegerL 1))
Prelude Language.Haskell.TH> $(return it)
2

> $(return (LitE (IntegerL 42)))
42

```

ale:
```
> $(return (AppE (VarE GHC.Enum.succ) (LitE (IntegerL 1))))

<interactive>:16:22:
    Couldn't match expected type ‘Name’ with actual type ‘a0 -> a0’
    Probable cause: ‘succ’ is applied to too few arguments
    In the first argument of ‘VarE’, namely ‘succ’
    In the first argument of ‘AppE’, namely ‘(VarE succ)’
Prelude Language.Haskell.TH> $(return (AppE (VarE "GHC.Enum.succ") (LitE (IntegerL 1))))

> :t VarE
VarE :: Name -> Exp
```


# Nazwy, wzorce, deklaracje

```
> :t mkName
mkName :: String -> Name

> $( return (AppE (VarE (mkName "succ")) (LitE (IntegerL 1))))
2

```

Dotąd budowaliśmy wyrażenia, ale podobnie można budować wzorce, deklaracje, etc.:

```
> runQ [d| p1 (a,b) = a |]
[FunD p1_0 [Clause [TupP [VarP a_1,VarP b_2]] (NormalB (VarE a_1)) []]]
```

`FunD` etc - patrz  [dokumentacja](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH.html#g:15).

Spróbujmy teraz sami zbudować podobną definicję.  Definicje uruchamiane w czasie kompilacji muszą być zaimportowane z innego modułu, dlatego musimy użyć dwóch modułów.

# Build1


``` {.haskell}
{-# START_FILE Build1.hs #-}
{-# LANGUAGE TemplateHaskell #-}
module Build1 where
import Language.Haskell.TH

-- p1 (a,b) = a
build_p1 :: Q [Dec]
build_p1 = return
    [ FunD p1 
             [ Clause [TupP [VarP a,VarP b]] (NormalB (VarE a)) []
             ]
    ] where
       p1 = mkName "p1"
       a = mkName "a"
       b = mkName "b"
       
{-# START_FILE Declare1.hs #-}       
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Build1

$(build_p1)

main = print $ p1 (1,2)
```


# Drukowanie zbudowanych deklaracji

``` {.haskell}
import Build1

$(build_p1)

pprLn :: Ppr a => a -> IO ()
pprLn = putStrLn . pprint
-- pprint :: Ppr a => a -> String

main = do
  decs <- runQ build_p1
  pprLn decs
  print $ p1(1,2)
```

``` {.haskell }
class Monad m => Quasi m where ...
instance Quasi Q where ...
instance Quasi IO where ...
runQ :: Quasi m => Q a -> m a
```

# Nowe nazwy

Budowanie i transformacje drzew struktury dla języka z wiązaniami jest skomplikowane z uwagi na potencjalne konflikty nazw.

Na szczęście TH dostarcza funkcję
[newName](http://hackage.haskell.org/packages/archive/template-haskell/2.9.0.0/doc/html/Language-Haskell-TH.html#v:newName):

```
newName :: String -> Q Name
```

(co przy okazji wyjasnia jeden z powodów, dla których [Q](http://hackage.haskell.org/packages/archive/template-haskell/2.9.0.0/doc/html/Language-Haskell-TH.html#t:Q) jest monadą.)

Przy użyciu `newName` możemy uodpornić nasz przyklad na konflikty nazw. Zauważmy jednak, że `p1` jest globalne i musi nadal używać `mkName`, natomiast `a` i `b` mogą być dowolnymi nazwami, więc wygenerujemy je przy użyciu `newName`.

# Build2

``` 
{-# START_FILE Build2.hs #-}
{-# LANGUAGE TemplateHaskell #-}
module Build2 where
import Language.Haskell.TH

build_p1 :: Q [Dec]
build_p1 = do
  let p1 = mkName "p1"  
  a <- newName "a"
  b <- newName "b"
  return
    [ FunD p1 
             [ Clause [TupP [VarP a,VarP b]] (NormalB (VarE a)) []
             ]
    ]

{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Build2

$(build_p1)

main = print $ p1 (1,2)
```

# Typowe użycie TH

Zdefiniujmy wszystkie rzutowania dla dużych (powiedzmy 16-) krotek. 
Zrobienie tego ręcznie byłoby koszmarem, ale TH może pomóc.

Tutaj zaczniemy od par, rozszerzenie tego na 16-krotki jest już prostym ćwiczeniem.

Przyda się pomocnicza definicja budująca deklarację prostej funkcji, np.simple function, e.g.

```
simpleFun name pats rhs = FunD name [Clause pats (NormalB rhs) []]
```

Mając funkcję taką, że `build_p n` buduje n-tą definicję, możemy zbudować wszystkie, używając `mapM`

```
build_ps = mapM build_p [1,2]
```

a potem

```
$(build_ps)

main = mapM_ print 
  [ p2_1 (1,2)
  , p2_2 (1,2)
  ]
```

# Build3

``` {.haskell}
{-# START_FILE Build3.hs #-}
{-# LANGUAGE TemplateHaskell #-}
module Build3 where
import Language.Haskell.TH

simpleFun :: Name -> [Pat] -> Exp -> Dec
simpleFun name pats rhs = FunD name [Clause pats (NormalB rhs) []]

build_ps = mapM build_p [1,2] where
    fname n = mkName $ "p2_" ++ show n
    argString k = "a" ++ show k
    argStrings = map argString [1,2]
    build_p n = do    
        argNames <- mapM newName argStrings
        let args = map VarP argNames
        return $ simpleFun (fname n) [TupP args] (VarE (argNames !! (n-1)))

{-# START_FILE Declare3.hs #-} 
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import Build3
build_ps -- dla deklaracji $(...) jest zbędne

main = mapM_ print
    [ p2_1 (1,2)
    , p2_2 (1,2)
    ]
```

# Quasiquoting

Widzieliśmy już standardowe quasiqotery e, t, d,p (np. `[e| \x -> x +1|]` ).
Ale możemy też definiować własne:

```
longString = [str|This is a multiline string.
It contains embedded newlines. And Unicode:
 
Ἐν ἀρχῇ ἦν ὁ Λόγος
 
It ends here: |]
 
main = putStrLn longString
```

``` {.haskell}
module Str(str) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
str = QuasiQuoter { quoteExp = stringE }
```

# Parsing Expressions

Let's start with a simple data type and parser for arithmetic expressions


``` { .haskell }
data Exp = EInt Int
         | EAdd Exp Exp
	     | ESub Exp Exp
		 | EMul Exp Exp
		 | EDiv Exp Exp
		 deriving(Show,Typeable,Data)

pExp :: Parser Exp
-- ...

test1 = parse pExp "test1" "1 - 2 - 3 * 4 "
main = print test1
```

# Testowanie

Now let's say we need some expresion trees in our program. For this kind of expressions we could (almost) get by  with `class Num` hack:

``` { .haskell }
instance Num Exp where
  fromInteger = EInt . fromInteger
  (+) = EAdd
  (*) = EMul
  (-) = ESub

testExp :: Exp
testExp = (2 + 2) * 3
```

...but it is neither extensible nor, in fact, nice.

Of course as soon as we have a parser ready we could use it to build expressions

``` { .haskell }
testExp = parse pExp "testExp" "1+2*3"
```
...but then potential errors in the expression texts remain undetected until runtime, and also this is not flexible enough: what if we wanted a simplifier for expressions, along the lines of

``` { .haskell }
simpl :: Exp -> Exp
simpl (EAdd (EInt 0) x) = x
```

# Why it's good to be Quasiquoted

what if we could instead write

``` { .haskell }
simpl :: Exp -> Exp
simpl (0 + x) = x
```

turns out with quasiquotation we can do just that (albeit with a slightly different syntax), so to whet your appetite:

``` { .haskell }
simpl :: Exp -> Exp
simpl [expr|0 + $x|] = x

main = print $ simpl [expr|0+2|]
-- ...
expr  :: QuasiQuoter
expr  =  QuasiQuoter
  { quoteExp = quoteExprExp
  , quotePat = quoteExprPat
  , quoteDec = undefined
  , quoteType = undefined
  }
```

as we can see, a QuasiQuoter consists of quasiquoters for expressions, patterns, declarations and types (the last two remain undefined in our example). Let us start with the (perhaps simplest) quasiquoter for expressions:


``` { .haskell }
quoteExprExp s = do
  pos <- getPosition
  exp <- parseExp pos s
  dataToExpQ (const Nothing) exp
```

# Quasiquoting Expressions

There are three steps:

* record the current position in Haskell file (for parse error reporting);
* parse the expression into our abstract syntax;
* convert our abstract syntax to its Template Haskell representation.

The first step is accomplished using [Language.Haskell.TH.location](http://hackage.haskell.org/packages/archive/template-haskell/2.8.0.0/doc/html/Language-Haskell-TH.html#v:location) and converting it to something usable by Parsec:

``` haskell
getPosition = fmap transPos TH.location where
  transPos loc = (TH.loc_filename loc,
                           fst (TH.loc_start loc),
	                       snd (TH.loc_start loc))
```

Parsing is done using our expression parser introduced at the beginning - nothing exciting here, but then comes the last part: generating Template Haskell, which seems like quite a task. Luckily we can save us some work using facilities for generic programming provided by [Data.Data](http://hackage.haskell.org/packages/archive/base/4.6.0.1/doc/html/Data-Data.html) combined with an almost magical Template Haskell function [dataToExpQ](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Quote.html#v:dataToExpQ).


# Quasiquoting patterns

So far, we are halfway through to our goal: we can use the quasiquoter on the right hand side of function definitions:

``` { .haskell }
testExp :: Exp
testExp = [expr|1+2*3|]
```

To be able to write things like

``` { .haskell }
simpl [expr|0 + $x|] = x
```
we need to write a quasiquoter for patterns. However, let us start with something less ambitious -  a quasiquoter for constant patterns, allowing us to write

``` { .haskell }
testExp :: Exp
testExp = [expr|1+2*3|]

f1 :: Exp -> String
f1 [expr| 1 + 2*3 |] = "Bingo!"
f1 _ = "Sorry, no bonus"

main = putStrLn $ f1 testExp
```

This can be done similarly to the quasiquoter for expressions:

* record the current position in Haskell file (for parse error reporting);
* parse the expression into our abstract syntax;
* convert our abstract syntax to its Template Haskell representation.

Only the last part needs to be slightly different - this time we need to construct Template Haskell pattern representation:

``` haskell
quoteExprPat :: String -> TH.Q TH.Pat
quoteExprPat s = do
  pos <- getPosition
  exp <- parseExp pos s
  dataToPatQ (const Nothing) exp

```

The functions `quoteExprExp` and `quoteExprPat` differ in two respects:

* use `dataToPatQ` instead of `dataToExpQ`
* the result type is different (obviously)

# Antiquotation

The quasiquotation mechanism we have seen so far allows us to translate domain-specific code into Haskell and `inject` it into our program. Antiquotation, as the name suggests goes in the opposite direction: embeds Haskell entities (e.g. variables) in our DSL.

This sounds complicated, but isn't really. Think HTML templates:

``` { .html}
<html>
<head>
<title>#{pageTitle}
```

The meaning is hopefully obvious - the value of program variable `pageTitle` should be embedded in the indicated place. In our expression language we might want to write

```
twice :: Exp -> Exp
twice e = [expr| $e + $e |]

testTwice = twice [expr| 3 * 3|]
```

This is nothing revolutionary. Haskell however, uses variables not only in expressions, but also in patterns, and here the story becomes a little interesting.

# Metavariables

Recall the pattern quasiquoter:

``` { .haskell }
quoteExprPat :: String -> TH.Q TH.Pat
quoteExprPat s = do
  pos <- getPosition
    exp <- parseExp pos s
      dataToPatQ (const Nothing) exp

```

You might have wondered about the `const Nothing` previously, and that is exactly the place we may add own extensions to the standard `Data` to `Pat` translation.

Let us extend our expression syntax and parser with metavariables (variables from the metalanguage):

```haskell
data Exp =  ... | EMetaVar String
           deriving(Show,Typeable,Data)

pExp :: Parser Exp
pExp = pTerm `chainl1` spaced addop

pTerm = spaced pFactor `chainl1` spaced mulop
pFactor = pNum <|> pMetaVar

pMetaVar = char '$' >> EMetaVar <$> ident

test1 = parse pExp "test1" "1 - 2 - 3 * 4 "
test2 = parse pExp "test2" "$x - $y*$z"
```

The antiquoter is defined as an extension for the `dataToPatQ`:

``` haskell
antiExprPat :: Exp -> Maybe (TH.Q TH.Pat)
antiExprPat (EMetaVar v) = Just $ TH.varP (TH.mkName v)
antiExprPat _ = Nothing
```

* metavariables are translated to `Just` TH variables
* for all the other cases we say `Nothing` - allowing `dataToPatQ` use its default rules

And that's it! Now we can write

``` haskell
eval [expr| $a + $b|] = eval a + eval b
eval [expr| $a * $b|] = eval a * eval b
eval (EInt n) = n
```

## Exercises

* Extend the expression simplifier with more rules.

* Add antiquotation to `quoteExprExp`

* Extend the expression quasiquoter to handle metavariables for
  numeric constants, allowing to implement simplification rules like

```
simpl [expr|$int:n$ + $int:m$|] = [expr| $int:m+n$ |]
```

(you are welcome to invent your own syntax in place of `$int: ... $`)

# Projekty zaliczeniowe

Haskell: **prosty** projekt 1-3 osobowy

Prezentacje projektów 12 czerwca.

Przykładowe tematy:

* wykorzystanie technik prezentowanych na wykładzie, np. Template Haskell, równoległość, typy zależne
* wizualizacja ewaluacji w Haskellu - coś na kształt <https://github.com/bmillwood/stepeval>
* synteza funkcji podanego typu (rachunek lambda lub Haskell), ew. spełniającej podane testy (to może być trudne, wystarczy zrobić w ograniczonym zakresie); warto wykorzystać równoległość

Alternatywnie: prezentacja na wykładzie (z elementami do przećwiczenia na laboratorium)

