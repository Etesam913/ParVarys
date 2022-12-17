{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Generator
  ( RandomFlowSpec(..),
    RandomSwitchSpec(..),
    Flow(..),
    Switch(..),
    CSP(..),
    generateProblem,
  )
where

import System.Random (randomRIO)

data Flow = Flow
  { coflowId :: Int,
    size :: Int,
    destinationId :: Int
  }
  deriving (Show)

data Switch = Switch
  { iId :: Int,
    flows :: [Flow],
    iBandwidth :: Int,
    eBandwidth :: Int
  }
  deriving (Show)

-- Coflow Scheduling Problem
data CSP = CSP
  { ingressSwitches :: [Switch],
    egressSwitches  :: [Switch]
  }
  deriving (Show)

data RandomFlowSpec = RandomFlowSpec {
  minSwitchId :: Int,
  maxSwitchId :: Int,
  minCoflowId :: Int,
  maxCoflowId :: Int,
  minFlowSize :: Int,
  maxFlowSize :: Int
}

data RandomSwitchSpec = RandomSwitchSpec {
  minFlows :: Int,
  maxFlows :: Int,
  ingressBandwidth :: Int,
  egressBandwidth :: Int
}

-- FUNCTIONS:
-- Generates random Integer from lb to ub (inclusive? Yes)
generateRandomNum :: Int -> Int -> IO Int
generateRandomNum lb ub = do
  randomRIO (lb, ub)

generateFlows :: RandomFlowSpec -> Int -> IO [Flow]
generateFlows spec n =
  if n <= 0 then do
    return []
  else do
    flows <- generateFlows spec $ n - 1
    coflowId <- generateRandomNum (minCoflowId spec) (maxCoflowId spec)
    egressSwitchId <- generateRandomNum (minSwitchId spec) (maxSwitchId spec)
    flowSize <- generateRandomNum (minFlowSize spec) (maxFlowSize spec)

    return $ Flow coflowId flowSize egressSwitchId : flows

generateSwitches :: RandomFlowSpec -> RandomSwitchSpec -> Int -> Int -> IO [Switch]
generateSwitches flowSpec switchSpec minId maxId =
  if maxId - minId < 0 then do
    return []
  else do
    numOfFlows <- generateRandomNum (minFlows switchSpec) (maxFlows switchSpec)
    flows <- generateFlows flowSpec numOfFlows
    let switch = Switch minId flows (ingressBandwidth switchSpec) (egressBandwidth switchSpec)

    switches <- generateSwitches flowSpec switchSpec (minId + 1) maxId
    return $ switch : switches

generateProblem :: RandomFlowSpec -> RandomSwitchSpec -> RandomSwitchSpec -> Int -> Int -> IO CSP
generateProblem flowSpec ingressSwitchSpec egressSwitchSpec numIngress numEgress = do
  let (minIngressId, maxIngressId) = (1, numIngress)
      (minEgressId, maxEgressId) = (numIngress + 1, numIngress + numEgress)

  iSwitches <- generateSwitches flowSpec ingressSwitchSpec minIngressId maxIngressId
  eSwitches  <- generateSwitches flowSpec egressSwitchSpec minEgressId maxEgressId 

  return $ CSP iSwitches eSwitches
   