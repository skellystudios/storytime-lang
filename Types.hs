{-# LANGUAGE GADTs, KindSignatures #-}

module Types where

type Name = String
type Value = Int
type Store = [(Name, Value)]

data Expr :: * -> * where
    -- Expr is a monad
    Return :: a -> Expr a
    Bind   :: Expr a -> (a -> Expr b) -> Expr b
    Chain  :: Expr a -> Expr b -> Expr b

    -- Commands
    Print   :: String -> Expr ()
    GetLine :: Expr String

    -- Variables (created on demand)
    GetVar :: Name -> Expr Value
    SetVar :: Name -> Value -> Expr ()

    -- Loop constructs
    While :: Expr Bool -> Expr a -> Expr ()
    For   :: Expr a -> Expr Bool -> Expr b -> Expr c -> Expr ()

    -- Chained Statements
    Sequence :: [Expr a] -> Expr ()
