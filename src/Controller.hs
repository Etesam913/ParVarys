module Controller
  ( Coflow(..),
  )
where

import Data.Map (Map)

import Generator
  ( CSP(..),
    Flow(..),
  )

data Coflow = Coflow {
    cfId           :: Integer,
    flows          :: [Flow],
    flowsByIngress :: Map Integer [Flow],
    flowsByEgress  :: Map Integer [Flow]
}

-- toCoflows :: CSP ->
