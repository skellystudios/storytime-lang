{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving #-}

module Types where

import Control.Monad

type Name = String
data Value =  I Int | S String
type Store = [(Name, Value)]

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
    GetVar :: Name -> Expr Value
    SetVar :: Name -> Expr Value -> Expr Value

    -- Loop constructs
    While :: Expr Bool -> Expr a -> Expr ()
    For   :: Expr a -> Expr Bool -> Expr b -> Expr c -> Expr ()

    -- Chained Statements
    Sequence :: [Expr a] -> Expr ()

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
