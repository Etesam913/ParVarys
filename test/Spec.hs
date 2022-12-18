import Data.Ratio ( (%) )
import System.Random ( mkStdGen, setStdGen )
import Test.HUnit

import Controller ( sebf, parSebf )
import Generator
  ( Switch(..),
    Flow(..),
    CSP(..),
    RandomFlowSpec(..),
    RandomSwitchSpec(..),
    generateProblem,
  )

testSebf1 :: Test
testSebf1 = TestCase (do
  let
    csp = CSP {
      ingressSwitches = [
        Switch {
          iId = 1, iBandwidth = 1, eBandwidth = 1,
          flows = [ Flow { coflowId = 1, size = 4, destinationId = 5 } ]
        },
        Switch {
          iId = 2, iBandwidth = 1, eBandwidth = 1,
          flows = [
            Flow { coflowId = 1, size = 1, destinationId = 6 },
            Flow { coflowId = 2, size = 2, destinationId = 6 }
          ]
        },
        Switch {
          iId = 3, iBandwidth = 1, eBandwidth = 1,
          flows = [
            Flow { coflowId = 1, size = 2, destinationId = 4 },
            Flow { coflowId = 2, size = 2, destinationId = 4 }
          ]
        }
      ],

      egressSwitches = [
        Switch { iId = n, iBandwidth = 1, eBandwidth = 1, flows = [] } |
        n <- [4 .. 6]
      ]
    }

  assertEqual "" [(2, 2 % 1), (1, 4 % 1)] $ sebf csp)

testSebf2 :: Test
testSebf2 = TestCase (do
  let
    csp = CSP {
      ingressSwitches = [
        Switch {
          iId = 1, iBandwidth = 2, eBandwidth = 1,
          flows = [ Flow { coflowId = 1, size = 4, destinationId = 5 } ]
        },
        Switch {
          iId = 2, iBandwidth = 1, eBandwidth = 1,
          flows = [
            Flow { coflowId = 1, size = 1, destinationId = 6 },
            Flow { coflowId = 2, size = 2, destinationId = 4 }
          ]
        },
        Switch {
          iId = 3, iBandwidth = 1, eBandwidth = 1,
          flows = [
            Flow { coflowId = 1, size = 2, destinationId = 4 },
            Flow { coflowId = 2, size = 2, destinationId = 4 }
          ]
        }
      ],

      egressSwitches = [
        Switch { iId = 4, iBandwidth = 1, eBandwidth = 1, flows = [] },
        Switch { iId = 5, iBandwidth = 1, eBandwidth = 4, flows = [] },
        Switch { iId = 6, iBandwidth = 1, eBandwidth = 1, flows = [] }
      ]
    }

  assertEqual "" [(1, 2 % 1), (2, 4 % 1)] $ sebf csp)

-- validate the correctness of parallel implementation using
-- the sequential implementation of SEBF
testParSebf1 :: Test
testParSebf1 = TestCase (do
  let seed = 91845734 
      flowSpec = RandomFlowSpec {
        minSwitchId=201, maxSwitchId=400,
        minCoflowId=1, maxCoflowId=1000, minFlowSize=0, maxFlowSize=100 }
      ingressSpec = RandomSwitchSpec 0 100 40 40
      egressSpec  = RandomSwitchSpec 0 0 40 40

  setStdGen $ mkStdGen seed
  problem <- generateProblem flowSpec ingressSpec egressSpec 200 200
  assertEqual "" (sebf problem) (parSebf problem))


main :: IO Counts
main =
  runTestTT $
    TestList [ "SEBF Sequential (varys_paper_fig1)" ~: testSebf1,
               "SEBF Sequential (variable_link_rates)" ~: testSebf2,
               "SEBF Parallel   (parMap)" ~: testParSebf1 ]