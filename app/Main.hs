import Generator
  ( RandomFlowSpec(..),
    RandomSwitchSpec(..),
    generateProblem)
import Controller (sebf)

main :: IO ()
main = do
  let
    numCoflows = 1000
    numIngress = 1000
    numEgress = 1000
    flowSize = 2
    minFlowsInSwitch = 0
    maxflowsInSwitch = 1000
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
  mapM_ print $ sebf problem
