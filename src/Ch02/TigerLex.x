{
module Ch02.TigerLex where
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \(p, _, _, s) _ -> return Let }
  in                             { \(p, _, _, s) _ -> return In }
  $digit+                        { \(p, _, _, s) len -> return $ Int (read (take len s)) }
  [\=\+\-\*\/\(\)]               { \(p, _, _, s) len -> return $ Sym (head s) }
  $alpha [$alpha $digit \_ \']*  { \(p, _, _, s) len -> return $ Var (take len s) }

{
-- Each action has type :: AlexInput -> Int -> AlexResult
alexEOF :: Alex Token
alexEOF = return EOF

-- The token type:
data Token
  = Let
  | In
  | Sym Char
  | Var String
  | Int Int
  | EOF
  deriving (Eq, Show)

}
