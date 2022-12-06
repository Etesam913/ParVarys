module Lib
  ( mainLib,
  )
where

mainLib :: IO ()
mainLib = testFib

helloWorld :: IO ()
helloWorld = putStrLn "Hello ParVarys 👋"

testFib :: IO ()
testFib = print $ nfib1 9

nfib1 :: Integer -> Integer
nfib1 n | n < 2 = 1
nfib1 n = nfib1 (n -1) + nfib1 (n -2) + 1