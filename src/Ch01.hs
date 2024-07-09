module Ch01 where

import Control.Monad (foldM, void)
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
interp stm = void $ interpStm (stm, [])
  where
    interpStm :: (Stm, Env) -> IO Env
    interpStm (CompoundStm a b, env) = interpStm (a, env) >>= \env' -> interpStm (b, env')
    interpStm (AssignStm k exp, env) = interpExp (exp, env) >>= \(v, env') -> pure ((k, v) : env')
    interpStm (PrintStm exps, env) = foldM go ([], env) exps >>= \(vals, newEnv) -> print vals >> pure newEnv
      where
        go (vals, env') exp = interpExp (exp, env') >>= \(val, newEnv) -> pure (vals ++ [val], newEnv)

    interpExp :: (Exp, Env) -> IO (Int, Env)
    interpExp (IdExp i, env) = pure $ case lookup i env of
      Just v -> (v, env)
      Nothing -> (0, env)
    interpExp (NumExp val, env) = pure (val, env)
    interpExp (OpExp a op b, env) =
      interpExp (a, env) >>= \(aval, env') ->
        interpExp (b, env') >>= \(bval, env'') ->
          pure
            ( case op of
                Plus -> aval + bval
                Minus -> aval - bval
                Times -> aval * bval
                Div -> aval `div` bval,
              env''
            )
    interpExp (EseqExp stm exp, env) = interpStm (stm, env) >>= \env' -> interpExp (exp, env')
