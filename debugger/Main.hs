{-# LANGUAGE TupleSections #-}
module Main where

import GHC.Debug.Client
import GHC.Debug.Count
import GHC.Debug.Retainers

main :: IO ()
main = withDebuggeeConnect "/tmp/ghc-debug" retainers

prog e = do
  pause e
  res <- run e $ do
    rs <- gcRoots
    count rs
  resume e
  print res


retainers e = do
  pause e
  res <- run e $ do
    roots <- gcRoots
    rs <- requestRetainers roots
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) rs
  resume e
  displayRetainerStack res

requestRetainers rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ ps _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == "Request"
        _ -> return $ False



