{-# LANGUAGE GADTs, KindSignatures #-}

module Eval where

import Control.Monad
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Fix

import Types
import Examples

instance Functor Expr where
    fmap  = liftM

instance Applicative Expr where
    pure  = Return
    (<*>) = ap  {- defined in Control.Monad -}
    (*>) = Chain

instance Monad Expr where
    return = Return
    (>>=)  = Bind
    (>>)   = Chain

runExpr :: Expr a -> StateT Store IO a
runExpr (Print str) = liftIO (putStrLn str)
runExpr (GetVar str) = do
						dict <- get
						let val = getFromDict str dict
						return val
runExpr (SetVar str val) = do
						dict <- get
						let new_dict = updateDict str val dict
						put new_dict
						return ()


runExpr GetLine     = liftIO getLine
runExpr (While p x) =
    fix $ \again -> do
        b <- runExpr p
        when b (runExpr x >> again)


getFromDict :: Name -> Store -> Value
getFromDict n ((k, v):kvs) =
	if n == k then v else 0
getFromDict _ [] = 0

updateDict :: Name -> Value -> Store -> Store
updateDict n v ((k, v'):kvs) =
	if n == k then (k, v):kvs
		else (k, v'):(updateDict n v kvs)
updateDict n v [] = [(n, v)]
