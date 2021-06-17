{- Some simple examples which can be inspecting using the ghc-debug-tui. -}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Debug.Stub
import qualified Data.Map as M

data StrictBox a = StrictBox !a

main :: IO ()
main = withGhcDebug $ do
  putStrLn "Enter a number: "
  len <- readLn

  -- example1 is an unevaluated THUNK with one pointer field
  let example1 = length [0..len]

  -- The evaluation of example2 is forced by the bang pattern, so the value
  -- is the evaluated length of the list.
  let !example2 = length [0..len]

  -- A simple list structure
  let !example3  = [0, 1, 2, 3, 4, 5]

  -- A simple pair structure
  let !example4  = ('a', 'b')

  -- A simple function
  let !example5  = \x -> print len

  -- The internal structure of data structures can be inspected using
  -- ghc-debug, for example, we can look at what the inside of a Map looks
  -- like.
  let !example6  = M.fromList [(n, ()) | n <- [0..len]]

  -- StrictBox has a strict field, so forcing the value also causes the
  -- boxed value to be forced. This can be verified in ghc-debug
  let !example7 = StrictBox (length [0..len])

  -- Subtle point about strict fields! Doesn't mean we always avoid
  -- thunks. You have to make sure the constructor is forced in order
  -- to force the strict boxes. Just has a lazy field so
  let !example8 = Just (StrictBox (length [0..len]))

  -- Ex: How could you modify example5 to force the inner thunk?

  -- Mark the closures we care about, so we can find them in the tui
  saveClosures [ Box example1
               , Box example2
               , Box example3
               , Box example4
               , Box example5
               , Box example6
               , Box example7
               , Box example8
               ]

  -- Stop executing, so we can pause
  putStrLn "Pausing for interruption by ghc-debug"
  getLine
  return ()
