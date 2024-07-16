module Ch02.Harness where

import Ch02.TigerLex

alexScanTokens :: String -> Either String [Token]
alexScanTokens inp = runAlex inp gather
  where
    gather :: Alex [Token]
    gather = do
      t <- alexMonadScan
      case t of
        EOF -> return [EOF]
        _ -> fmap (t :) gather

lexStdio :: IO ()
lexStdio = do
  s <- getContents
  print $ alexScanTokens s
