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
  | Try    Expr Int
  deriving (Show)

exp0 :: Expr
exp0 = Div (Number 200) (Plus (Number 7) (Number (-5)))

exp1 :: Expr
exp1 = Div (Number 200) (Plus (Number 5) (Number (-5)))

-- >>> eval exp0
-- 100

-- >>> exp1
-- Div (Number 200) (Plus (Number 5) (Number (-5)))

eval :: Expr -> Int
eval = go
  where
    go (Number n)    = n
    go (Plus  e1 e2) = go e1   +   go e2
    go (Div   e1 e2) = go e1 `div` go e2

----------------------------------------------------------------------
-- | The "Exception" monad
----------------------------------------------------------------------
data Option a = Null       | Some a
data    Result   a = Err String | Ok   a deriving (Show, Functor)
-- data Either e a = Left e     | Right a

instance Applicative Result where

instance Monad Result where
  return :: a -> Result a
  return x = Ok x
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Ok x)  >>= f = f x
  (Err s) >>= _ = Err s

{-
instance Monad (Either e) where

  return :: a -> Either e a
  return x = Right x

  (>>=)  :: Either e a -> (a -> Either e b) -> Either e b
  (Right x) >>= f  = f x
  (Left err) >>= _ = Left err

-}

-- do x <- e1; e2             e1 >>= \x -> e2

-- >>> exp0
-- Div (Number 200) (Plus (Number 7) (Number (-5)))

-- (A) 100
-- (B) Ok 100

-- >>> evalR exp0
-- Ok 100

-- >>> evalR (Try exp1 666)
-- Ok 666

-- >>> evalR exp1

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
      if n2 /= 0 then return (div n1 n2) else throw ("dbz: " ++ show e2)
    go (Try e n) = case go e of
      Ok m -> return m
      Err _ -> return n



throw :: String -> Result Int
throw s = Err s

{- "Result" is known as "Either" -}

-- >>> exp0
-- Div (Number 200) (Plus (Number 7) (Number (-5)))

-- >>> evalE exp0
-- Right 100

-- >>> exp1
-- Div (Number 200) (Plus (Number 5) (Number (-5)))

exp3 :: Expr
exp3 = Plus (Number 5) (Div (Number 200) (Plus (Number 5) (Number (-5))))

-- >>> exp3
-- Plus (Number 5) (Div (Number 200) (Plus (Number 5) (Number (-5))))

-- >>> evalE (Try exp3 99)
-- Right 99

evalE :: (MonadError Expr m) => Expr -> m Int
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
      if n2 /= 0 then return (div n1 n2) else throwError e2
    go (Try e n) = catchError (go e) (\_ -> return n)

catchExn :: Either e v -> (e -> Either e v) -> Either e v
catchExn (Left exn)  handler = handler exn
catchExn (Right val) _       = Right val

throwExn :: a -> Either a b
throwExn e = Left e




      -- case go e of
      -- Left _   -> return n
      -- Right val -> return val

{-
   go (Plus (Number 5) (Div (Number 10) (Number 0)))
=>
   go (Number 5) >>= \n1 ->
     go (Div (Number 10) (Number 0)) >>= \n2 ->
      return (n1+n2)
=> Right 5 >>= \n1 ->
     go (Div (Number 10) (Number 0)) >>= \n2 ->
      return (n1+n2)

=>  (\n1 ->
     go (Div (Number 10) (Number 0)) >>= \n2 ->
      return (n1+n2)) 5

=>
     go (Div (Number 10) (Number 0)) >>= \n2 ->
      return (5 + n2)

=>>
     Left (Number 0) >>= \n2 ->
      return (5 + n2)

=>
    Left (Number 0)


go (Div (Number 10) (Number 0))
=>>
Left (Number 0)

-}





----------------------------------------------------------------------
-- | A "Profiling" monad
----------------------------------------------------------------------


type Profile a = State Int a

tick :: (MonadState Int m) => m ()
tick = do
  n <- get
  put (n+1)

-- >>> runState (evalPr exp0) 10000000
-- (100,10000002)

-- evalPr :: (MonadState Int m) => Expr -> m Int

runProf :: Show v => Prof v -> String
runProf st = showValCount z
  where
    z = unId (runStateT st 0)


data SillyFun = MkSillyFun (Int -> Int)

silly :: SillyFun
silly = MkSillyFun (\n -> n + 10)

runSilly :: SillyFun -> Int -> Int
runSilly (MkSillyFun f) n = f n
-- >>> runSilly silly 10
-- 20

-- Couldn't match expected type `t0_ao2UU[tau:1] -> t_ao2UW[sk:1]'
--             with actual type `SillyFun'
-- The function `silly' is applied to one value argument,
--   but its type `SillyFun' has none
-- In the expression: silly 10
-- In an equation for `it_ao2TB': it_ao2TB = silly 10
-- Relevant bindings include
--   it_ao2TB :: t_ao2UW[sk:1]
--     (bound at /Users/rjhala/teaching/fa23/static/code/src/lec_11_28_23.hs:218:2)

-- evalPr :: Expr -> Prof Int
evalPr :: Expr -> State Int Int
evalPr = go
  where
    go (Number n)   =
      return n
    go (Plus e1 e2) = do
      n1 <- go e1
      n2 <- go e2
      tick
      return (n1 + n2)
    go (Div e1 e2)  = do
      n1 <- go e1
      n2 <- go e2
      tick
      return (div n1 n2)

type Prof a = StateT Int Identity a

    --   if n2 /= 0 then return (div n1 n2) else throwError e2
    -- go (Try e n) = catchError (go e) (\_ -> return n)




runProfile :: (Show a) => Profile a -> String
runProfile st = showValCount (runState st 0)

showValCount :: (Show v) => (v, Int) -> String
showValCount (val, n) = Printf.printf "value: %s, #ops: %d" (show val) n

-- >>> exp0
-- Div (Number 200) (Plus (Number 7) (Number (-5)))

-- >>> Plus (Number 10) exp1
-- Plus (Number 10) (Div (Number 200) (Plus (Number 5) (Number (-5))))

-- >>> runProfile (evalPr (Plus (Number 10) exp1))
-- divide by zero


{-

@logger
@cache
def foo(x,y,z):
  BODY

def logger(f):
  def wrap_f(args):
    res = f(args)
    print ("called with {args:?} got {res:?}")
    return res
  return wrap_f

def cache(f):
  table = {}
  def wrap_f(args):
    if args in table:
      return table[args]
    else:
      res = f(args)
      table[args] = res
      return res
  return wrap_f

cache :: Function -> Function



-}













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
{-

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


-- type Prof = StateT Int Ident

type Exn  = ExceptT Expr Ident

type ExProf = ExceptT Expr Prof

type ProfEx = StateT  Int Exn

-}


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
