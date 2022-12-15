import Data.Ratio ( (%) )
import Test.HUnit

import Controller
  ( sebf,
  )
import Generator
  ( Switch(..),
    Flow(..),
    CSP(..),
  )

sebf1 :: Test
sebf1 = TestCase (do
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

sebf2 :: Test
sebf2 = TestCase (do
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


main :: IO Counts
main =
  runTestTT $
    TestList [ "SEBF Sequential (varys_paper_fig1)" ~: sebf1,
               "SEBF Sequential (variable_link_rates)" ~: sebf2 ]