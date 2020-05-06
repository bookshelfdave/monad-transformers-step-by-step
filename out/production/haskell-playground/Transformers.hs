module Transformers where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Function
import qualified Data.Map                   as Map
import           Data.Maybe

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

type Eval a = ReaderT Env (ExceptT String
                          (WriterT [String] (StateT Integer IO))) a

runEval :: Env -> Integer -> Eval a -> IO ((Either String a, [String]), Integer)
runEval env st ev =
    runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval :: Exp -> Eval Value
eval (Lit i) = do
    tick
    liftIO $ print i
    return $ IntVal i
eval (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Just n' -> return n'
        Nothing -> throwError ("undefined variable: " ++ n)
eval (Plus a b) = do
    tick
    ia <- eval a
    ib <- eval b
    case (ia, ib) of
        (IntVal a', IntVal b') -> return $ IntVal (a' + b')
        _                      -> throwError "type error in addition"
eval (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval (App a b) = do
    tick
    a' <- eval a
    b' <- eval b
    case a' of
      FunVal env' n body -> local (const $ Map.insert n b' env') (eval body)
      _                  -> throwError "type error in application"