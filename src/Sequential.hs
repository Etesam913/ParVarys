{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Sequential
  ( getSequentialOrdering,
  )
where

import System.Random (randomRIO)
import Text.Pretty.Simple (pPrint)

data IngressPort = IngressPort
  { iId :: Int,
    -- The 4 rows for a virtual output queue
    rows :: (Row, Row, Row, Row),
    iBandwidth :: Float
  }
  deriving (Show)

data EgressPort = EgressPort
  { eId :: Int,
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
  Row :: {flows :: Maybe [Flow]} -> Row
  deriving (Show)

numOfPorts :: Integer
numOfPorts = 10

numOfRows :: Integer
numOfRows = 4

numOfCoflows :: Integer
numOfCoflows = 4

maxNumOfFlowsInRow :: Integer
maxNumOfFlowsInRow = 10

-- Generates random Integer from lb to ub (inclusive?)
generateRandomNum :: Integer -> Integer -> IO Integer
generateRandomNum lb ub = do
  randomRIO (lb, ub :: Integer)

addFullFlows :: (Eq t, Num t) => [Maybe Flow] -> t -> IO [Maybe Flow]
addFullFlows arr 0 = do return arr
addFullFlows arr n = do
  randomCoflowId <- generateRandomNum 0 numOfCoflows
  randomEgressPortId <- generateRandomNum 0 numOfPorts
  addFullFlows (Just (Flow randomCoflowId 2 randomEgressPortId) : arr) (n -1)

addEmptyFlows :: [Maybe Flow] -> Integer -> [Maybe Flow]
addEmptyFlows arr 0 = arr
addEmptyFlows arr n = addEmptyFlows (Nothing : arr) (n -1)

-- Generates Flows that go in each row

generateFlows :: IO [Maybe Flow]
generateFlows = do
  numOfFlows <- generateRandomNum 0 maxNumOfFlowsInRow
  let remainingFlowSpots = maxNumOfFlowsInRow - numOfFlows
  addFullFlows (addEmptyFlows [] remainingFlowSpots) numOfFlows

--addRows [x:xs] = do

generateRows :: IO ()
generateRows = do
  generatedFlows <- generateFlows
  --generateRows (Row generatedFlows : arr) (n -1)
  putStrLn "bob"

{- addRows arr 0 = arr
addRows arr n =  -}

getSequentialOrdering :: IO ()
getSequentialOrdering = do
  -- Recreating first ingress port from example: https://etesam.nyc3.digitaloceanspaces.com/virtual-output-queue.png
  let r1 = Row $ Just [Flow 0 2.01 1, Flow 1 1 1]
  let r2 = Row $ Just [Flow 1 1 1]
  let r3 = Row Nothing
  let r4 = Row Nothing

  let firstIPort = IngressPort 0 (r1, r2, r3, r4) 100

  let firstEPort = EgressPort 0 50

  pPrint firstIPort
