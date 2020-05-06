module Lib where

import qualified Data.Map as Map

type Name = String

data Exp
    = Lit Integer
    | Var Name
    | Plus Exp
           Exp
    | Abs Name
          Exp
    | App Exp
          Exp
    deriving (Show)

data Value
    = IntVal Integer
    | FunVal Env
             Name
             Exp
    deriving (Show)

type Env = Map.Map Name Value

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
