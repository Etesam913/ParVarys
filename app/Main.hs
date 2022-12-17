import Controller (parSebf, sebf)
import Control.Monad (join)
import Formatting (fprintLn)
import Formatting.Clock (timeSpecs)
import Generator
  ( RandomFlowSpec (..),
    RandomSwitchSpec (..),
    generateProblem,
  )
import Options.Applicative
import System.Clock
import System.Exit (die)


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "ParVarys: Parallel Varys Coflow Scheduling Using SEBF "
  <> progDesc ("Generates an offline coflow scheduling problem, and uses the " ++
               "Varys Shortest Effective Bottlneck First heuristic to order "  ++
               "the coflows.")
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> strOption
            (  long "type"
            <> short 't'
            <> metavar "STRING"
            <> help "varys mode: seq, parMap"
            <> value "parMap"
            <> showDefault
            )
        <*> option auto
            (  long "number_param"
            <> short 'n'
            <> metavar "NUMBER"
            <> help "number parameter"
            <> value 1
            <> showDefault
            )

work :: String -> Int -> IO ()
work mode _ = do
  sebfImpl <-
    case mode of
      "seq"    -> return sebf
      "parMap" -> return parSebf
      _        -> die $
                    "Unrecognized varys mode: " ++
                    "expect one of seq, parMap, got " ++ show mode

  let numCoflows = 4000
      numIngress = 1000
      numEgress = 1000
      flowSize = 2
      minFlowsInSwitch = 0
      maxflowsInSwitch = 5000
      iBandwidth = 2
      eBandwidth = 2

      flowSpec =
        RandomFlowSpec
          { minSwitchId = numIngress + 1,
            maxSwitchId = numIngress + numEgress,
            minCoflowId = 1,
            maxCoflowId = numCoflows,
            minFlowSize = flowSize,
            maxFlowSize = flowSize
          }

      ingressSwitchSpec =
        RandomSwitchSpec
          { minFlows = minFlowsInSwitch,
            maxFlows = maxflowsInSwitch,
            ingressBandwidth = iBandwidth,
            egressBandwidth = eBandwidth
          }

      egressSwitchSpec =
        RandomSwitchSpec
          { minFlows = 0,
            maxFlows = 0,
            ingressBandwidth = iBandwidth,
            egressBandwidth = eBandwidth
          }

  problem <- generateProblem flowSpec ingressSwitchSpec egressSwitchSpec numIngress numEgress
  start <- getTime Monotonic
  print $ sebfImpl problem
  end <- getTime Monotonic
  putStrLn "Calculation Time"
  fprintLn timeSpecs start end
