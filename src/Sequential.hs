{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Sequential
  ( getSequentialOrdering,
  )
where

import System.Random (randomRIO)
import Text.Pretty.Simple (pPrint)

-- DATA TYPES:
data IngressPort = IngressPort
  { iId :: Integer,
    -- The 4 rows for a virtual output queue
    rows :: (Row, Row, Row, Row),
    iBandwidth :: Float
  }
  deriving (Show)

data EgressPort = EgressPort
  { eId :: Integer,
    eBandwidth :: Float
  }
  deriving (Show)

data Flow = Flow
  { coflowId :: Integer,
    size :: Float,
    -- the egress port id that the flow goes to
    destinationId :: Integer
  }
  deriving (Show)

data Row where
  Row :: {flows :: [Flow]} -> Row
  deriving (Show)

-- GENERAL VARIABLES:
numOfPorts :: Integer
numOfPorts = 2

numOfRows :: Integer
numOfRows = 4

numOfCoflows :: Integer
numOfCoflows = 4

maxNumOfFlowsInRow :: Integer
maxNumOfFlowsInRow = 5

-- FUNCTIONS:
-- Generates random Integer from lb to ub (inclusive? Yes)
generateRandomNum :: Integer -> Integer -> IO Integer
generateRandomNum lb ub = do
  randomRIO (lb, ub :: Integer)

addFlows :: (Eq t, Num t) => [Flow] -> t -> IO [Flow]
addFlows arr 0 = do return arr
addFlows arr n = do
  randomCoflowId <- generateRandomNum 1 numOfCoflows
  randomEgressPortId <- generateRandomNum 1 numOfPorts
  addFlows (Flow randomCoflowId 2 randomEgressPortId : arr) (n - 1)

-- Generates Flows that go in each row
generateFlows :: IO [Flow]
generateFlows = do
  numOfFlows <- generateRandomNum 0 maxNumOfFlowsInRow
  addFlows [] numOfFlows

addRows :: Monad m => [Row] -> [m [Flow]] -> m [Row]
addRows arr [] = do return arr
addRows arr (f : fs) = do
  currentFlows <- f
  addRows (Row currentFlows : arr) fs

generateRows :: IO [Row]
generateRows = do
  -- generate flows for each row
  let rowFlows = [generateFlows | _ <- [1 .. numOfRows]]
  addRows [] rowFlows

generateIngressPorts :: [IngressPort] -> Integer -> IO [IngressPort]
generateIngressPorts arr 0 = return arr
generateIngressPorts arr n = do
  (r1 : r2 : r3 : r4 : _) <- generateRows
  generateIngressPorts (IngressPort n (r1, r2, r3, r4) 2 : arr) (n -1)

-- This the func that gets exports
getSequentialOrdering :: IO ()
getSequentialOrdering = do
  ingressPorts <- generateIngressPorts [] numOfPorts
  let egressPorts = [EgressPort x 2 | x <- [1 .. numOfPorts]]

  pPrint ingressPorts
  pPrint egressPorts
