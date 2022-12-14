import qualified Data.Map as Map

import Text.Pretty.Simple (pPrint)

import Generator
  ( RandomFlowSpec(..),
    RandomSwitchSpec(..),
    generateProblem)
import Controller
  ( toCoflows,
    getSwitchBandwidth,
    getGamma,
    sebf
  )

main :: IO ()
main = do
  let
    numCoflows = 5
    numIngress = 2
    numEgress = 2
    flowSize = 2
    minFlowsInSwitch = 0
    maxflowsInSwitch = 4
    iBandwidth = 2
    eBandwidth = 2

    flowSpec = RandomFlowSpec {
        minSwitchId = numIngress + 1,
        maxSwitchId = numIngress + numEgress,
        minCoflowId = 1,
        maxCoflowId = numCoflows,
        minFlowSize = flowSize,
        maxFlowSize = flowSize
    }

    ingressSwitchSpec = RandomSwitchSpec {
        minFlows = minFlowsInSwitch,
        maxFlows = maxflowsInSwitch,
        ingressBandwidth = iBandwidth,
        egressBandwidth = eBandwidth
    }

    egressSwitchSpec = RandomSwitchSpec {
        minFlows = 0,
        maxFlows = 0,
        ingressBandwidth = iBandwidth,
        egressBandwidth = eBandwidth
    }

  problem <- generateProblem flowSpec ingressSwitchSpec egressSwitchSpec numIngress numEgress
  pPrint problem

  let coflows = toCoflows problem
      switchBwTbl = getSwitchBandwidth problem
  pPrint coflows
  pPrint switchBwTbl

  pPrint $ sebf problem