{-# LANGUAGE GADTs, KindSignatures #-}

module Eval where

import Control.Monad
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Fix

import Types
import Examples
import Parsing

runExpr :: Expr a -> StateT Store IO a
runExpr (Print exp) = do
                      dict <- get
                      y <- liftIO(runStateT (runExpr exp) dict)
                      let str = fst y
                      liftIO (putStrLn . toS $ str)
                      return $ str
runExpr (PrimString str) = do
                      return $ S str
runExpr (GetVar str) = do
						dict <- get
						let val = getFromDict str dict
						return val
runExpr (SetVar str expr) = do
            dict <- get
            y <- liftIO(runStateT (runExpr expr) dict)
            let new_val = fst y
            let new_dict = updateDict str new_val dict
            put new_dict
            return new_val

runExpr GetLine  = liftIO getLine
runExpr (While p x) =
    fix $ \again -> do
        b <- runExpr p
        when b (runExpr x >> again)

getFromDict :: Name -> Store -> Value
getFromDict n ((k, v):kvs) =
	if n == k then v else getFromDict n kvs
getFromDict n [] = I (length n)

updateDict :: Name -> Value -> Store -> Store
updateDict n v ((k, v'):kvs) =
	if n == k then (k, v):kvs
		else (k, v'):(updateDict n v kvs)
updateDict n v [] = [(n, v)]



makeAST :: TokenSeq -> Expr Value
makeAST (sub@(Symbol x):(SayOperator):phr@(String y):[]) = Print (PrimString y)
makeAST (sub@(Symbol x):(SayOperator):_) = Print (GetVar x)
makeAST (sub@(Symbol x):(AssignOperator):tks) = SetVar x (makeAST tks)
makeAST [(String x)] = PrimString x

makeASTs = map makeAST . tokenize
--
-- eval :: TokenSeq -> Expr String

debugMakeAST :: String -> [String]
debugMakeAST = map (pp . makeAST) . tokenize

--
runProgram = (\st -> runStateT st []) . runExpr . head . makeASTs

f :: Store -> StateT Store IO a -> IO Store
f store stateT = liftM (snd) (runStateT stateT store)
makeSTs = map runExpr . makeASTs

empty :: Store
empty = []


runAllLines xs = foldM f empty (makeSTs xs)
