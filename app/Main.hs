{-# LANGUAGE NamedFieldPuns #-}

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

-- Arg Parser template adapted from:
--  https://ro-che.info/articles/2016-12-30-optparse-applicative-quick-start
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
            <> help "Varys mode: seq, parMap"
            <> value "parMap"
            <> showDefault
            )
        <*> option auto
            (  long "coflows"
            <> short 'n'
            <> metavar "NUMBER"
            <> help "Number of coflows"
            <> value 4000
            <> showDefault
            )
        <*> option auto
            (  long "ingress"
            <> short 'i'
            <> metavar "NUMBER"
            <> help "Number of ingress switches"
            <> value 1000
            <> showDefault
            )
        <*> option auto
            (  long "egress"
            <> short 'e'
            <> metavar "NUMBER"
            <> help "Number of egress switches"
            <> value 1000
            <> showDefault
            )
        <*> option auto
            (  long "min-flow-size"
            <> short 's'
            <> metavar "NUMBER"
            <> help "Smallest flow size in bytes"
            <> value 0
            <> showDefault
            )
        <*> option auto
            (  long "max-flow-size"
            <> short 'S'
            <> metavar "NUMBER"
            <> help "Largest flow size in bytes"
            <> value 1000
            <> showDefault
            )
        <*> option auto
            (  long "min-switch-flows"
            <> short 'f'
            <> metavar "NUMBER"
            <> help "Minimum number of flows arriving at an ingress switch"
            <> value 0
            <> showDefault
            )
        <*> option auto
            (  long "max-switch-flows"
            <> short 'F'
            <> metavar "NUMBER"
            <> help "Maximum number of flows arriving at an ingress switch"
            <> value 5000
            <> showDefault
            )
        <*> option auto
            (  long "ingress-bandwith"
            <> short 'b'
            <> metavar "NUMBER"
            <> help "Ingress bandwidth (Gb/s) of a switch"
            <> value 40
            <> showDefault
            )
        <*> option auto
            (  long "egress-bandwith"
            <> short 'B'
            <> metavar "NUMBER"
            <> help "Egress bandwidth (Gb/s) of a switch"
            <> value 40
            <> showDefault
            )


work :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
work mode numCoflows numIngress numEgress minFlowSize maxFlowSize minFlows
  maxFlows ingressBandwidth egressBandwidth = do
  sebfImpl <-
    case mode of
      "seq"    -> return sebf
      "parMap" -> return parSebf
      _        -> die $
                    "Unrecognized varys mode: " ++
                    "expect one of seq, parMap, got " ++ show mode

  let
    flowSpec = RandomFlowSpec
      { minSwitchId = numIngress + 1,
        maxSwitchId = numIngress + numEgress,
        minCoflowId = 1,
        maxCoflowId = numCoflows,
        minFlowSize,
        maxFlowSize
      }

    ingressSwitchSpec = RandomSwitchSpec
      { minFlows, maxFlows, ingressBandwidth, egressBandwidth }

    egressSwitchSpec = RandomSwitchSpec
      { minFlows = 0, maxFlows = 0, ingressBandwidth, egressBandwidth }

  problem <- generateProblem flowSpec ingressSwitchSpec egressSwitchSpec numIngress numEgress

  start <- getTime Monotonic
  print $ sebfImpl problem
  end <- getTime Monotonic

  putStr "Calculation Time: "
  fprintLn timeSpecs start end
