module CPasses.C0 where

import Data.Int (Int64)

data Atom = Var String | Int Int64
  deriving (Show, Eq)

data Exp
  = Atm Atom
  | Add Atom Atom
  | Sub Atom
  | Read
  deriving (Show, Eq)

data Stmt = Assign String Exp
  deriving (Show, Eq)

data Tail
  = Return Exp
  | Seq Stmt Tail
  deriving (Show, Eq)

data C0 = Program [(String, Tail)]
  deriving (Show, Eq)