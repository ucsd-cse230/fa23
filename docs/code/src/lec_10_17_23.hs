module Lec_10_17_23 where

-- >>> 2 + 1
-- 3

thing2 :: Double
thing2 = 5.1 + 6.9


data CircleT = MkCircle {
  xLoc :: Double,
  yLoc :: Double,
  radius :: Double
}

circle0 :: CircleT
circle0 = MkCircle 0 0 5

area :: CircleT -> Double
area c = pi * radius c * radius c

-- DOUBLE AND DOUBLE AND DOUBLE
data CuboidT = MkCuboid
  { cLength  :: Double,
    cBreadth :: Double,
    cHeight  :: Double
  }

cub0 :: CuboidT
cub0 = MkCuboid 10 20 30

volume :: CuboidT -> Double
volume c = cLength c * cBreadth c * cHeight c

data Shape
  = ShCircle CircleT
  | ShCuboid CuboidT

-- data Shape
--   = MkCir Double Double Double
--   | MkCub Double Double Double


shapes :: [Shape]
shapes = [ShCircle circle0, ShCuboid cub0]

data Shape2D
  = MkRect Double Double -- ^ 'MkRect w h' is a rectangle with width 'w', height 'h'
  | MkCirc Double        -- ^ 'MkCirc r' is a circle with radius 'r'
  | MkPoly [Vertex]      -- ^ 'MkPoly [v1,...,vn]' is a polygon with vertices at 'v1...vn'

type Vertex = (Double, Double)

area2 :: Shape2D -> Double
area2 (MkRect w h) = w * h
area2 (MkCirc r  ) = pi * r * r
area2 (MkPoly vs ) = areaPolygon vs

areaPolygon :: [Vertex] -> Double
areaPolygon (v1:v2:v3:vs) = areaTriangle v1 v2 v3 + areaPolygon (v1:v3:vs)
areaPolygon _ = 0

{-
areaPoly p = case p of
  (v1:v2:v3:vs) -> areaTriangle v1 v2 v3 + areaPolygon (v1:v3:vs)
  _             -> 0

-}

-- areaPolygon [v1,v2,v3]       = areaTriangle v1 v2 v3
-- areaPolygon [v1,v2,v3,v4]    = areaTriangle v1 v2 v3 + areaPolygon [v1,v3,v4]
-- areaPolygon [v1,v2,v3,v4,v5] = areaTriangle v1 v2 v3 + areaPolygon [v1,v3,v4,v5]


areaTriangle :: Vertex -> Vertex -> Vertex -> Double
areaTriangle v1 v2 v3 = sqrt (s * (s - s1) * (s - s2) * (s - s3))
  where
      s  = (s1 + s2 + s3) / 2
      s1 = distance v1 v2
      s2 = distance v2 v3
      s3 = distance v3 v1

distance :: Vertex -> Vertex -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)



--

data IList
  = INil
  | ICons Int IList
  deriving (Show)

data SList
  = SNil
  | SCons String SList
  deriving (Show)

data List t
  = Nil
  | Cons t (List t)
  deriving (Show)

-- >>> size sleupr
-- (1) (2) (3) (4) (5)

blerp :: List String
blerp = Cons "1" (Cons "2" (Cons "3" Nil))

glerp :: List Integer
glerp = Cons 1 (Cons 2 (Cons 3 Nil))

sleupr :: List (List Int)
sleupr = Cons (Cons 1 (Cons 2 Nil)) (Cons (Cons 3 Nil) Nil)

-- append :: List a -> List a -> List a
(+++) :: List t -> List t -> List t
(+++) Nil            ys = ys
(+++) (Cons x1 xs)   ys = Cons x1 (xs +++ ys)

size :: List a -> Int
size Nil = 0
size (Cons h t) = 1 + size t

total :: List Int -> Int
total Nil = 0
total (Cons h t) = h + total t

maxList :: List Int -> Int
maxList Nil        = 0
maxList (Cons h t) = max h (maxList t)

concatList Nil = ""
concatList (Cons h t) = h ++ concatList t
{-

foldr op b Nil         = b
foldr op b (Cons h t)  = h `op` (foldr op b t)

-- total   = megabob (+)  0
-- maxlist = megabob max  0
-- concat  = megabob (++) ""

bob Nil        = 0
bob (Cons h t) = h   +   (bob t)

bob Nil        = 0
bob (Cons h t) = h `max` (bob t)

bob Nil        = ""
bob (Cons h t) = h ++ bob t

plus x y = x + y
-}





data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

-- >>> (Cons 1 (Cons 2 (Cons 3 Nil))) +++ (Cons 4 (Cons 5 (Cons 6 Nil)))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))

-- >>> "cat" == ['c', 'a', 't']
-- True

foo :: Int -> Int -> Int
foo x y = x + y

-- >>> 10 `foo` 100
-- 110

-- >>> concatTree myOtherTree
-- "onetwothreefourfive"

myTree :: Tree Int
myTree =
  Node
    (Node
      (Node (Leaf 1) (Leaf 2))
      (Node (Leaf 3) (Leaf 4)))
    (Leaf 5)

myOtherTree :: Tree String
myOtherTree =
  Node
    (Node
      (Node (Leaf "one") (Leaf "two"))
      (Node (Leaf "three") (Leaf "four")))
    (Leaf "five")

concatTree :: Tree String -> String
concatTree (Leaf str) = str
concatTree (Node l r) = concatTree l ++ concatTree r

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)



-- >>> height myTree
-- 3



-- append (Cons x1 Nil)           ys           = Cons x1          ys
-- append (Cons x1 (Cons x2 Nil)) ys           = Cons x1 (Cons x2 ys)
-- append (Cons x1 (Cons x2 (Cons x3 Nil))) ys = Cons x1 (Cons x2 (Cons x3 ys))
-- append (Cons x1 (Cons x2 (Cons x3 Nil))) ys = Cons x1 (append (Cons x2 (Cons x3 Nil)) ys)

{-

size (Cons (Cons 1 (Cons 2 Nil)) (Cons (Cons 3 Nil) Nil))
-->
1 + size (Cons (Cons 3 Nil) Nil)
-->
1 + (1 + size Nil)
-->
1 + (1 + 0)
-->
2
-}

-- >>> bob1
-- ICons 1 (ICons 2 (ICons 3 INil))

bob :: IList
bob = INil

bob2 :: IList
bob3 = ICons 3 bob

bob3 :: IList
bob2 = ICons 2 bob3

bob1 :: IList
bob1 = ICons 1 bob2

-- >>> size
