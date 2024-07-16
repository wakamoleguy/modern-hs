module Ch02.Tokens where

type Linenum = Int

class Token a where
  typeT :: Linenum -> Linenum -> a
  varT :: Linenum -> Linenum -> a
  functionT :: Linenum -> Linenum -> a
  breakT :: Linenum -> Linenum -> a
  ofT :: Linenum -> Linenum -> a
  endT :: Linenum -> Linenum -> a
  inT :: Linenum -> Linenum -> a
  nilT :: Linenum -> Linenum -> a
  letT :: Linenum -> Linenum -> a
  doT :: Linenum -> Linenum -> a
  toT :: Linenum -> Linenum -> a
  forT :: Linenum -> Linenum -> a
  whileT :: Linenum -> Linenum -> a
  elseT :: Linenum -> Linenum -> a
  thenT :: Linenum -> Linenum -> a
  ifT :: Linenum -> Linenum -> a
  arrayT :: Linenum -> Linenum -> a
  assignT :: Linenum -> Linenum -> a
  orT :: Linenum -> Linenum -> a
  andT :: Linenum -> Linenum -> a
  geT :: Linenum -> Linenum -> a
  gtT :: Linenum -> Linenum -> a
  leT :: Linenum -> Linenum -> a
  ltT :: Linenum -> Linenum -> a
  neqT :: Linenum -> Linenum -> a
  eqT :: Linenum -> Linenum -> a
  divideT :: Linenum -> Linenum -> a
  timesT :: Linenum -> Linenum -> a
  minusT :: Linenum -> Linenum -> a
  plusT :: Linenum -> Linenum -> a
  dotT :: Linenum -> Linenum -> a
  rbraceT :: Linenum -> Linenum -> a
  lbraceT :: Linenum -> Linenum -> a
  rbrackT :: Linenum -> Linenum -> a
  lbrackT :: Linenum -> Linenum -> a
  rparenT :: Linenum -> Linenum -> a
  lparenT :: Linenum -> Linenum -> a
  semicolonT :: Linenum -> Linenum -> a
  colonT :: Linenum -> Linenum -> a
  commaT :: Linenum -> Linenum -> a
  stringT :: String -> Linenum -> Linenum -> a
  intT :: Int -> Linenum -> Linenum -> a
  idT :: String -> Linenum -> Linenum -> a
  eofT :: Linenum -> Linenum -> a
