module Lib
  ( mainLib,
  )
where

import Sequential (getSequentialOrdering)

mainLib :: IO ()
mainLib = do
  getSequentialOrdering