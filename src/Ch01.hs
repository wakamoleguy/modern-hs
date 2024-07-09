module Ch01 where

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
