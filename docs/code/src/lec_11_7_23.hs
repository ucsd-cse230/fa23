{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lec_11_7_23 where

data Expr
  = Nbr Int            -- ^ 0,1,2,3,4
  | Add Expr Expr      -- ^ e1 + e2
  | Sub Expr Expr      -- ^ e1 - e2
  | Div Expr Expr      -- ^ e1 / e2
  | Neg Expr           -- ^ - e
  | Ite Expr Expr Expr -- ^ if e1 != 0 then e2 else e3
  deriving (Show)

-- >>> eval (Nbr 100)
-- Ok 100

-- >>>  eval (Div (Nbr 100) (Sub (Add (Nbr 2) (Nbr 3)) (Nbr 5)))
-- Err "Sub (Add (Nbr 2) (Nbr 3)) (Nbr 5)"

eval0 :: Expr -> Int
eval0 = go
  where
   go (Nbr n)     = n
   go (Neg e)     = - (go e)
   go (Add e1 e2) = go e1 + go e2
   go (Sub e1 e2) = go e1 - go e2
   go (Div e1 e2) = go e1 `div` go e2
   go (Ite e1 e2 e3) = if go e1 /= 0 then go e2 else go e3

data Option v   = None | Some v deriving (Show {- , Functor -})

data Result e v = Err e | Ok v deriving (Show )

instance Functor (Result e) where
    fmap _ (Err e) = Err e
    fmap f (Ok x)  = Ok (f x)

instance Applicative (Result e) where

instance Monad (Result e) where
  (>>=) = bindResult

  return :: a -> Result e a
  return x = Ok x


bindResult :: Result e a -> (a -> Result e b) -> Result e b
bindResult arg1 doStuff =
    case arg1 of
        Err e ->  Err e
        Ok v1 -> doStuff v1


bindOption :: Option a -> (a -> Option b) -> Option b
bindOption arg1 doStuff =
    case arg1 of
        None -> None
        Some v1 -> doStuff v1


instance Functor Option where
    fmap _ None     = None
    fmap f (Some x) = Some (f x)
{-
class Functor t where
    fmap :: (a -> b) -> t a -> t b
-}

map2 :: (a1 -> a2 -> b) -> Option a1 -> Option a2 -> Option b
map2 f (Some v1) (Some v2) = Some (f v1 v2)
map2 _ _         _         = None

instance Applicative Option where

instance Monad Option where
    (>>=) = bindOption

    return :: a -> Option a
    return = Some
{-

    e1 >>= \x ->
        e2


IS THE SAME AS ...

    do x <- e1
       e2



class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

-}



eval1 :: Expr -> Option Int
eval1 = go
  where
   go (Nbr n)     = return n
   go (Neg e)     = do { v <- go e; return (negate v) }
   go (Add e1 e2) = do { v1 <- go e1; v2 <- go e2 ; return(v1 + v2) }
   go (Sub e1 e2) = do { v1 <- go e1; v2 <- go e2 ; return (v1 - v2) }
   go (Div e1 e2) = do { v1 <- go e1; v2 <- go e2 ;  if v2 == 0 then None else return (v1 `div` v2) }
   go (Ite e1 e2 e3) = do { v1 <- go e1; if v1 /= 0 then go e2 else go e3 }

-- eval :: Expr -> Result String Int

eval :: Expr -> Result String Int
eval = go
  where
   go (Nbr n)     = return n
   go (Neg e)     = do { v <- go e; return (negate v) }
   go (Add e1 e2) = do { v1 <- go e1; v2 <- go e2 ; return(v1 + v2) }
   go (Sub e1 e2) = do { v1 <- go e1; v2 <- go e2 ; return (v1 - v2) }
   go (Div e1 e2) = do { v1 <- go e1; v2 <- go e2 ;  if v2 == 0 then throw (show e2) else return (v1 `div` v2) }
   go (Ite e1 e2 e3) = do { v1 <- go e1; if v1 /= 0 then go e2 else go e3 }

throw :: e -> Result e v
throw e = Err e



ex0 :: Expr
ex0 = Nbr 0
