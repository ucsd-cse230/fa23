{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}

module Lec_11_28_23 where

import           Control.Monad.State
import qualified Text.Printf as Printf
import           Control.Monad.Except

data Expr
  = Number Int
  | Plus   Expr Expr
  | Div    Expr Expr
  deriving (Show)

exp0 :: Expr
exp0 = Div (Number 200) (Plus (Number 7) (Number (-5)))

exp1 :: Expr
exp1 = Div (Number 200) (Plus (Number 5) (Number (-5)))

-- >>> eval exp0

-- >>> eval exp1

eval :: Expr -> Int
eval (Number n)    = n
eval (Plus  e1 e2) = eval e1   +   eval e2
eval (Div   e1 e2) = eval e1 `div` eval e2

----------------------------------------------------------------------
-- | The "Exception" monad
----------------------------------------------------------------------

data Result a = Err String | Ok a deriving (Show, Functor)

instance Applicative Result where

instance Monad Result where
  return :: a -> Result a
  return x = Ok x

  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Ok x) >>= f = f x
  (Err s) >>= _ = Err s

-- >>> evalR exp0
-- Ok 100

-- >>> evalR exp1
-- Err "dbz: Plus (Number 5) (Number (-5))"

evalR :: Expr -> Result Int
evalR = go
  where
    go (Number n) =
      return n
    go (Plus e1 e2) = do
      n1 <- go e1
      n2 <- go e2
      return (n1 + n2)
    go (Div e1 e2) = do
      n1 <- go e1
      n2 <- go e2
      if n2 /= 0 then Ok(div n1 n2) else Err ("dbz: " ++ show e2)

{- "Result" is known as "Either" -}

evalE :: Expr -> Either Expr Int
evalE = go
  where
    go (Number n) =
      return n
    go (Plus e1 e2) = do
      n1 <- go e1
      n2 <- go e2
      return (n1+n2)
    go (Div e1 e2)  = do
      n1 <- go e1
      n2 <- go e2
      if n2 == 0 then throw e2 else return (div n1 n2)

throw :: a -> Either a b
throw = Left

----------------------------------------------------------------------
-- | A "Profiling" monad
----------------------------------------------------------------------


type Profile a = State Int a

evalPr :: Expr -> Profile Int
evalPr = go
  where
    go (Number n)   = return n
    go (Plus e1 e2) = do
      n1 <- go e1
      n2 <- go e2
      thump
      return (n1 + n2)
    go (Div e1 e2)  = do
      n1 <- go e1
      n2 <- go e2
      thump
      return (div n1 n2)

runProfile :: (Show a) => Profile a -> String
runProfile st = showValCount (runState st 0)

showValCount :: (Show v) => (v, Int) -> String
showValCount (val, n) = Printf.printf "value: %s, ops: %d" (show val) n


-- >>> runProfile (evalPr exp0)
-- divide by zero


-- count :: Profile ()
count :: (MonadState Int m) => m ()
count = do
  n <- get
  put (n + 1)


-- >>> showProf (evalP exp0)
-- "result = Left (Plus (Number 5) (Number (-5))), count = 2"



-- >>> (evalE exp0)
-- Right 100




thump :: (MonadState Int m) => m ()
thump = do
  n <- get
  put (n + 1)


-- >>> exp0
-- Div (Number 200) (Plus (Number 5) (Number (-5)))


evalPE :: (MonadState Int m, MonadError Expr m) => Expr -> m Int
evalPE (Number n)   = return n
evalPE (Plus e1 e2) = do
    n1 <- evalPE e1
    n2 <- evalPE e2
    thump
    return (n1 + n2)
evalPE (Div e1 e2)  = do
    n1 <- evalPE e1
    n2 <- evalPE e2
    thump
    if n2 == 0 then throwError e2 else return (div n1 n2)


-- >>> runExProf (evalPE exp0)

-- >>> runExProf (evalPE exp1)


runProfEx :: ProfEx a -> Either Expr (a, Int)
runProfEx profex = res
  where
    Ident res = runExceptT (runStateT profex 0)

runExProf :: ExProf a -> (Either Expr a, Int)
runExProf exprof = foo
    where
        Ident foo = runStateT (runExceptT exprof) 0

-- foo = (runExceptT, runStateT)

expr1 :: Expr
expr1 = Div (Number 200) (Plus (Number 5) (Number 5))

expr2 :: Expr
expr2 = Div (Number 10) (Plus (Number 5) (Number (-5)))


type Prof = StateT Int Ident

type Exn  = ExceptT Expr Ident

type ExProf = ExceptT Expr Prof

type ProfEx = StateT  Int Exn




newtype Ident a = Ident a

instance Functor Ident where
    fmap f (Ident a) = Ident (f a)

instance Applicative Ident where
  pure x = Ident x
  (Ident f) <*> (Ident x) = Ident (f x)

instance Monad Ident where
  return        = pure
  Ident a >>= f = f a



newtype Identity a = Id { unId :: a }  deriving (Functor)

instance Monad Identity where
  (Id a) >>= f = f a


-- type ProfExn = StateT Int Exn
-- type ExnProf = ExceptT Expr Prof

-- runExnProf :: (Show a) => ExnProf a -> Either Expr (a, Int)
-- runExnProf _ep = undefined

-- runProfExn :: (Show a) => ProfExn a -> Either Expr (a, Int)
-- runProfExn _ep = undefined

-- runStateT :: StateT s m a -> s -> m (a, s)

-- runExceptT :: ExceptT e m a -> m (Either e a)

















instance Applicative Identity where
  pure x        = Id x
  Id f <*> Id x = Id (f x)


-- runProfExn ep = unId (runExceptT (runStateT ep 0))
