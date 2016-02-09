{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving #-}

module Types where

import Control.Monad
import Data.HashMap.Strict hiding (map)

type Name = String
data Value =  I Int | S String
data Store = Store ValueStore  MethodStore
type ValueStore = (HashMap Name Value)
type MethodStore = (HashMap Name (Expr Value))

data Expr :: * -> * where
    -- Expr is a monad
    Return :: a -> Expr a
    Bind   :: Expr a -> (a -> Expr b) -> Expr b
    Chain  :: Expr a -> Expr b -> Expr b

    -- Commands
    Print   :: Expr Value -> Expr Value
    GetLine :: Expr String

    PrimString :: String -> Expr Value

    -- Variables (created on demand)
    GetVar :: Expr Name -> Expr Value
    OutputAscii :: Expr Name -> Expr Value
    SetVar :: Expr Name -> Expr Value -> Expr Value
    DerefSymbol :: String -> Expr Name

    -- Methods
    MethodDec :: String -> Expr Value -> Expr Value
    MethodApp :: String -> Name -> Expr Value
    RepeatedMethodApp :: String -> Name -> Expr Value

    -- Loop constructs
    While :: Expr Bool -> Expr a -> Expr ()
    For   :: Expr a -> Expr Bool -> Expr b -> Expr c -> Expr ()

    -- Chained Statements
    Sequence :: [Expr Value] -> Expr Value

    -- Binary
    Combine :: Expr Name -> Expr Name -> Expr Value

toS (I i) = show i
toS (S s) = s

pp :: Expr a -> String
pp (Print x) = "Print " ++ pp x
pp (PrimString x ) = x

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
