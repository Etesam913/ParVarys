{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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
  { coflowId :: Integer,
    size :: Integer,
    -- the egress port id that the flow goes to
    destinationId :: Integer
  }
  deriving (Show)

data Switch = Switch
  { iId :: Integer,
    flows :: [Flow],
    iBandwidth :: Integer,
    eBandwidth :: Integer
  }
  deriving (Show)

-- Coflow Scheduling Problem
data CSP = CSP
  { ingressSwitches :: [Switch],
    egressSwitches  :: [Switch]
  }
  deriving (Show)

data RandomFlowSpec = RandomFlowSpec {
  minSwitchId :: Integer,
  maxSwitchId :: Integer,
  minCoflowId :: Integer,
  maxCoflowId :: Integer,
  minFlowSize :: Integer,
  maxFlowSize :: Integer
}

data RandomSwitchSpec = RandomSwitchSpec {
  minFlows :: Integer,
  maxFlows :: Integer,
  ingressBandwidth :: Integer,
  egressBandwidth :: Integer
}

-- FUNCTIONS:
-- Generates random Integer from lb to ub (inclusive? Yes)
generateRandomNum :: Integer -> Integer -> IO Integer
generateRandomNum lb ub = do
  randomRIO (lb, ub :: Integer)

generateFlows :: RandomFlowSpec -> Integer -> IO [Flow]
generateFlows spec n =
  if (n <= 0) then do
    return []
  else do
    flows <- generateFlows spec $ n - 1
    coflowId <- generateRandomNum (minCoflowId spec) (maxCoflowId spec)
    egressSwitchId <- generateRandomNum (minSwitchId spec) (maxSwitchId spec)
    flowSize <- generateRandomNum (minFlowSize spec) (maxFlowSize spec)

    return $ Flow coflowId flowSize egressSwitchId : flows

generateSwitches :: RandomFlowSpec -> RandomSwitchSpec -> Integer -> Integer -> IO [Switch]
generateSwitches flowSpec switchSpec minId maxId =
  if (maxId - minId < 0) then do
    return []
  else do
    numOfFlows <- generateRandomNum (minFlows switchSpec) (maxFlows switchSpec)
    flows <- generateFlows flowSpec numOfFlows
    let switch = Switch minId flows (ingressBandwidth switchSpec) (egressBandwidth switchSpec)

    switches <- generateSwitches flowSpec switchSpec (minId + 1) maxId
    return $ switch : switches

generateProblem :: RandomFlowSpec -> RandomSwitchSpec -> RandomSwitchSpec -> Integer -> Integer -> IO CSP
generateProblem flowSpec ingressSwitchSpec egressSwitchSpec numIngress numEgress = do
  let (minIngressId, maxIngressId) = (1, numIngress)
      (minEgressId, maxEgressId) = (numIngress + 1, numIngress + numEgress)

  iSwitches <- generateSwitches flowSpec ingressSwitchSpec minIngressId maxIngressId
  eSwitches  <- generateSwitches flowSpec egressSwitchSpec minEgressId maxEgressId 

  return $ CSP iSwitches eSwitches
   