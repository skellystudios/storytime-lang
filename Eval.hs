{-# LANGUAGE GADTs, KindSignatures #-}

module Eval where

import Control.Monad
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Fix
import Data.HashMap.Strict hiding (map)
import Data.Char

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
                          y <- liftIO(runStateT (runExpr str) dict)
                          let val = getValue (fst y) dict
                          return val

runExpr (SetVar name_expr val_expr) = do
            dict <- get
            val_res <- liftIO(runStateT (runExpr val_expr) dict)
            name_res <- liftIO(runStateT (runExpr name_expr) dict)
            let new_val = fst val_res
            let name_str = fst name_res
            let new_dict = setValue name_str new_val dict
            put new_dict
            return new_val

runExpr (DerefSymbol str) = do
            dict <- get
            let someone_deref = (getValue "someone_val" dict)
            return $ case str of
                              "someone" -> toS someone_deref
                              _         -> str

runExpr (MethodDec name_str method_ast) = do
            dict <- get
            let newDict = (setMethod name_str method_ast dict)
            put newDict
            return $ S name_str


runExpr (MethodApp method_name arg_name) = do
            dict <- get
            let method = (getMethod method_name dict)
            let arg_val = (getValue arg_name dict)
            let new_dict = setValue "someone_val" (S arg_name) dict
            val_res <- liftIO(runStateT (runExpr method) new_dict)
            let new_val = fst val_res
            return $ new_val

runExpr GetLine =
            liftIO getLine

runExpr (While p x) =
    fix $ \again -> do
        b <- runExpr p
        when b (runExpr x >> again)


getValue :: Name -> Store -> Value
getValue n (Store values methods) =
  lookupDefault (I (length n)) n values

getMethod :: Name -> Store -> Expr Value
getMethod n (Store values methods) =
  let full_n = (map toLower n) ++ "ing" in
    lookupDefault (PrimString full_n) full_n methods

setValue :: Name -> Value -> Store -> Store
setValue n val (Store values methods) =
  let new_values = insert n val values in
    Store new_values methods

setMethod :: Name -> Expr Value -> Store -> Store
setMethod n val (Store values methods) =
  let new_methods = insert (map toLower n) val methods in
    Store values new_methods


makeAST :: TokenSeq -> Expr Value
makeAST (sub@(Symbol x):(SayOperator):phr@(String y):[]) = Print (PrimString y)
makeAST (sub@(Symbol x):(Atom a):[]) = MethodApp a x
makeAST (sub@(UnboundVariable x):(SayOperator):phr@(String y):[]) = Print (PrimString y)
makeAST (sub@(UnboundVariable x):(SayOperator):_) = Print (GetVar (DerefSymbol x))
makeAST (sub@(Symbol x):(SayOperator):_) = Print (GetVar (DerefSymbol x))
makeAST (sub@(Symbol x):(AssignOperator):tks) = SetVar (DerefSymbol x) (makeAST tks)
makeAST (sub@(Symbol x):[]) = GetVar (DerefSymbol x)
makeAST [(String x)] = PrimString x
makeAST ((MethodDecOp x):tks) = MethodDec x (makeAST tks)

makeASTs = map makeAST . tokenize

debugMakeAST :: String -> [String]
debugMakeAST = map (pp . makeAST) . tokenize

f :: Store -> StateT Store IO a -> IO Store
f store stateT = liftM (snd) (runStateT stateT store)
makeSTs = map runExpr . makeASTs


run xs = foldM f (Store empty empty) (makeSTs xs)
