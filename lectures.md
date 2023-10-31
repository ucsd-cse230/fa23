---
title: Lecture Notes
headerImg: books.jpg
---


## Videos

The lectures will be recorded and available on [CANVAS](https://canvas.ucsd.edu/courses/29526)

## Topics, Notes and Code

| **Date**   | **Topic**                       | **Notes**                 | **Markup**           | **Code**           |
|:----------:|:--------------------------------|:--------------------------|:---------------------|:-------------------|
| *9/28*     | Intro                           | [pdf][00-intro]           |                      |                    |
| *10/3*     | Lambda Calculus                 | [html][01-lambda]         | [pdf][01-pdf]        |                    |
| *10/10*    | Haskell Basics                  | [html][02-basic]          | [pdf][02-pdf]        | [code][code-10-10]
| *10/12*    | Algebraic Data Types            | [html][03-adt]            | [pdf][03-pdf]        | [code][code-10-12] |
| *10/17*    | Higher-order Fun                | [html][04-hof]            | [pdf][04-pdf]        | [code][code-10-17] |
| *10/19*    | IO                              | [html][05-io]             | [pdf][05-pdf]        | [code][code-10-19] |
| *10/24*    | Type Classes 		           | [html][06-typeclasses]    | [pdf][06-pdf]        | [code][code-10-24] |
| *10/26*    | Functors                        | [html][07-functors]       | [pdf][07-pdf]        | [code][code-10-26] |
| *10/31*    | Monads                          | [html][08-monads]         | [pdf][08-pdf]        | [code][code-10-31] |

<!--
|            | Iteration and State             | [lists][10-list] [state][11-state]   |  [pdf][11-state-A] | [code][code-10-28] |
|            | Parser Combinators              | [html][12-parsers]        |                      | [code][code-11-2]  |
|            | ""                              |                           |                      | [code][code-11-4]  |
|            | Monad Transformers              | [html][13-transformers]   |                      | [code][code-11-9]  |
|            | Property-based Testing          | [html][14-testing]        |                      | [code][code-11-16] |
|            | Concurrency                     | [html][15-stm]            | [pdf][pfd13]         | [code][code-11-23] |
|            | Refinement Types                | [1][lh1] [2][lh2] [3][lh3] [4][lh4]   |          |                    |

| *12/3*     | Exceptions                      | [html][13-transformers]   | [pdf][13-exceptions] | [code][code]      |
| *12/8*     | Monad Transformers              | [html][13-transformers]   | [TBD][13-trans]      | [code][code]      |
|            | Property-based Testing          | [html][14-testing]        | [TBD][TBD]           | [code][code]      |
|            | List Monad                      | [html][10-list]           |                      |                  e
-- >>> 1 + 2
-- 3

inc :: Int -> Int
inc x = x + 1


-- >>> plus True False
-- No instance for (Num Bool) arising from a use of `plus'
-- In the expression: plus True False
-- In an equation for `it_aKhA': it_aKhA = plus True False

plus :: Num a => a -> a -> a
plus x y = x + y

eq :: Eq a => a -> a -> Bool
eq x y = x == y

lt :: Ord a => a -> a -> Bool
lt x y = x < y

-- >>> show [1,2,3]
-- "[1,2,3]"

-- >>> Taco < Burrito
-- True

-- >>> Taco
-- <Taco>

data Filling = Rice | Beans deriving (Eq, Ord)

data Jhala = Taco | Burrito  | Salsa deriving (Eq, Ord )

-- jInt :: Jhala -> Int
-- jInt Taco    = 0
-- jInt Burrito = 1
-- jInt Salsa   = 2

-- instance Show Jhala where
--     show Taco = "<Taco>"
--     show Burrito = "<Burrito>"
--     show Salsa = "<Salsa>"

-- instance Eq Jhala where
--   (==) :: Jhala -> Jhala -> Bool
--   (==) x y = jInt x == jInt y

-- instance Ord Jhala where
--     compare x y = compare (jInt x) (jInt y)

-- >>> get menu0 "taco"
-- 5

-- >>> get menu1 Salsa
-- 0

data Table k v
  = Emp
  | Bind k v (Table k v)
  deriving (Show)
instance Mappable Table where
  gmap _ Emp = Emp
  gmap f (Bind k v rest) = Bind k (f v) (gmap f rest)

menu0 :: Table String Int
menu0 = Bind "burrito" 10 (Bind "salsa" 0 (Bind "taco" 5 Emp))

menu1 :: Table Jhala Int
menu1 = Bind Burrito 10 (Bind Taco 5 (Bind Salsa 0 Emp))

-- >>> getU menu0 "churro"
-- Fail

data Option v   = Some v          | None
data Maybe v    = Just v          | Nothing
data Nullable v = Value v         | Null deriving (Show)
data List v     = Cons v (List v) | Empty

-- >>> waiter menu0 "taco"
-- "the price is $5"

-- >>> read "67.45" :: Float
-- 67.45

waiter :: Table String Int -> String -> String
waiter menu item = case getU menu item of
                      Null -> "sorry, we do not sell " ++ item
                      Value n -> "the price is $" ++ show n

getU :: Ord k => Table k v -> k -> Nullable v
getU (Bind key value rest) k
  | k == key      = Value value
  | k <  key      = Null
  | otherwise     = getU rest k
getU Emp   _      = Null

get :: Eq k => Table k v -> k -> v
get (Bind key value rest) k
  | key == k      = value
  | otherwise     = get rest k
get Emp   _       = undefined -- spl value or exception

getWithDefault :: Eq k => v -> Table k v -> k -> v
getWithDefault def (Bind key value rest) k
  | key == k              = value
  | otherwise             = getWithDefault def rest k
getWithDefault def Emp  _ = def -- spl value or exception




data Blah = Aaa | Baa | Haa

foo :: Blah -> Int
foo Aaa = 0
foo Baa = 1
foo Haa = 2


data JVal
  = JNum  Double
  | JBool Bool
  | JStr  String
  | JArr  [JVal]
  | JObj  [ (String, JVal) ]
  deriving (Show)

class ToJVal a where
  jval :: a -> JVal

instance ToJVal Integer where
 jval i = JNum (fromIntegral i)

instance ToJVal Int where
 jval i = JNum (fromIntegral i)


instance ToJVal Double where
  jval d = JNum d

data MyString = S String

instance ToJVal MyString where
  jval (S s) = JStr s

instance ToJVal a => ToJVal [a] where
  jval xs = JArr (map jval xs)

instance (ToJVal a, ToJVal b) => ToJVal (a, b) where
--  jval (x, y) = JArr [ jval x, jval y]
  jval (x, y) = JObj [ ("0", jval x), ("1", jval y)]

-- >>> jval (S "one", (2, (S "three", (S "four", menu0))))
-- JObj [("0",JStr "one"),("1",JObj [("0",JNum 2.0),("1",JObj [("0",JStr "three"),("1",JObj [("0",JStr "four"),("1",JObj [("burrito",JNum 10.0),("salsa",JNum 0.0),("taco",JNum 5.0)])])])])]


instance ToJVal a => ToJVal (Table String a) where
  jval t = JObj [ (k, jval v) | (k, v) <- kvs ]
             -- JObj (map (\(k, v) -> (k, intJVal v)) kvs)
    where
      kvs = keyVals t

-- >>> jval [[1,2,3], [4,5,6]]
-- JArr [JArr [JNum 1.0,JNum 2.0,JNum 3.0],JArr [JNum 4.0,JNum 5.0,JNum 6.0]]







keyVals :: Table k v -> [(k, v)]
keyVals (Bind k v rest) = (k, v) : keyVals rest
keyVals Emp             = []

showw :: (Mappable c) => c Int -> c String
showw = gmap show

sq :: (Mappable c) => c Int -> c Int
sq = gmap (\x -> x * x)

-- >>> sq [1,2,3,4,5]
-- [1,4,9,16,25]

-- >>> sq (Node 30 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf))
-- Node 900 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)

-- >>> sq menu0

-- Bind "burrito" 100 (Bind "salsa" 0 (Bind "taco" 25 Emp))
data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Show)

showTree :: Tree Int -> Tree String
showTree = mapTree show
-- showTree Leaf         = Leaf
-- showTree (Node v l r) = Node (show v) (showTree l) (showTree r)

sqTree :: Tree Int -> Tree Int
sqTree = mapTree (\x -> x * x)
-- sqTree Leaf         = Leaf
-- sqTree (Node v l r) = Node (v * v) (sqTree l) (sqTree r)

mapTree :: (t -> a) -> Tree t -> Tree a
mapTree _ Leaf         = Leaf
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)

type MyList a = [a]

-- mapList :: (t -> a) -> MyList t -> MyList a
-- mapTree :: (t -> a) -> Tree t -> Tree a

class Mappable c where
  gmap :: (a -> b) -> c a -> c b

-- >>> :kind Table
-- Table :: * -> * -> *

instance Mappable [] where
  gmap _ [] = []
  gmap f (x:xs) = f x : gmap f xs

-- Functor

instance Mappable Tree where
  gmap _ Leaf         = Leaf
  gmap f (Node v l r) = Node (f v) (gmap f l) (gmap f r)

|            | Proofs as Programs              |                           |                      |                  |
-->

[lh1]: http://ucsd-progsys.github.io/lh-workshop/01-index.html
[lh2]: http://ucsd-progsys.github.io/lh-workshop/02-refinements.html
[lh3]: http://ucsd-progsys.github.io/lh-workshop/03-datatypes.html
[lh4]: http://ucsd-progsys.github.io/lh-workshop/04-case-study-insertsort.html


[TBD]: TBD
[code]: https://github.com/ucsd-cse230/fa23/tree/master/static/code/src
[00-intro]: static/raw/lec-intro.pdf
[01-lambda]: lectures/01-lambda.html
[02-basic]: lectures/02-basic.html
[03-adt]: lectures/03-adt.html
[04-hof]: lectures/04-hof.html
[05-io]: lectures/05-io.html
[06-typeclasses]: lectures/06-typeclasses.html
[07-functors]: lectures/07-functors.html
[08-monads]: lectures/08-monad.html

[01-pdf]: static/raw/01-lambda.pdf
[02-pdf]: static/raw/02-basic.pdf
[03-pdf]: static/raw/03-adt.pdf
[04-pdf]: static/raw/04-hof.pdf
[05-pdf]: static/raw/05-io.pdf
[06-pdf]: static/raw/06-classes.pdf
[07-pdf]: static/raw/07-functors.pdf
[08-pdf]: static/raw/08-monads.pdf

[code-10-10]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_10_23.hs
[code-10-12]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_12_23.hs
[code-10-17]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_17_23.hs
[code-10-19]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/hello.hs
[code-10-24]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_26_23.hs
[code-10-26]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_24_23.hs
[code-10-31]: https://github.com/ucsd-cse230/fa23/tree/main/static/code/src/lec_10_31_23.hs
