module Ch01 where

import Control.Monad.State (MonadState (get), StateT, evalStateT, get, gets, lift, put)
import Data.Maybe (fromMaybe)
import Prelude hiding (exp)

----------------------------------------
---------- Data Types
----------------------------------------

type Id = String

data BinOp = Plus | Minus | Times | Div
  deriving (Eq, Show)

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]
  deriving (Eq, Show)

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp BinOp Exp
  | EseqExp Stm Exp
  deriving (Eq, Show)

----------------------------------------
---------- Sample Program
----------------------------------------

prog :: Stm
prog =
  CompoundStm
    ( AssignStm
        "a"
        (OpExp (NumExp 5) Plus (NumExp 3))
    )
    ( CompoundStm
        ( AssignStm
            "b"
            ( EseqExp
                ( PrintStm
                    [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)]
                )
                (OpExp (NumExp 10) Times (IdExp "a"))
            )
        )
        ( PrintStm
            [IdExp "b"]
        )
    )

----------------------------------------
---------- Straight Line Program Interpreter
----------------------------------------

maxargs :: Stm -> Int
maxargs = go
  where
    go (CompoundStm a b) = max (maxargs a) (maxargs b)
    go (AssignStm _ e) = emaxargs e
    go (PrintStm es) = max (length es) (maximum $ map emaxargs es)
    emaxargs (IdExp _) = 0
    emaxargs (NumExp _) = 0
    emaxargs (OpExp a _ b) = max (emaxargs a) (emaxargs b)
    emaxargs (EseqExp s e) = max (maxargs s) (emaxargs e)

type Env = [(Id, Int)]

interp :: Stm -> IO ()
interp p = evalStateT (interpStm p) []
  where
    interpStm :: Stm -> StateT Env IO ()
    interpStm (CompoundStm a b) = do
      interpStm a
      interpStm b
    interpStm (AssignStm k exp) = do
      v <- interpExp exp
      env <- get
      put ((k, v) : env)
      return ()
    interpStm (PrintStm exps) = do
      vals <- mapM interpExp exps
      lift $ print vals
    interpExp :: Exp -> StateT Env IO Int
    interpExp (IdExp i) = do
      gets (fromMaybe 0 . lookup i)
    interpExp (NumExp val) = return val
    interpExp (OpExp a op b) = do
      aval <- interpExp a
      bval <- interpExp b
      return $ case op of
        Plus -> aval + bval
        Minus -> aval - bval
        Times -> aval * bval
        Div -> aval `div` bval
    interpExp (EseqExp s e) = do
      interpStm s
      interpExp e
