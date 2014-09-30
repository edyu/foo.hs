module Foo where

import Data.List

printHisto xs = putStrLn (histogram xs)

histogram :: [Integer] -> String
histogram xs = (unlines . reverse) (buildhisto xs) ++ "==========\n0123456789"

buildhisto :: [Integer] -> [String]
buildhisto [] = []
buildhisto xs = histoline xs : buildhisto xs'
  where xs' = xs \\ [0..9]

histoline :: [Integer] -> String
histoline xs = map mark [0..9]
  where mark x | x `elem` xs = '*'
               | otherwise   = ' '
