{-# LANGUAGE GADTs #-}

module Sequential
  ( getSequentialOrdering,
  )
where

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
  { coflowId :: Int,
    size :: Float,
    -- the egress port id that the flow goes to
    destinationId :: Int
  }
  deriving (Show)

data Row where
  Row :: {flows :: Maybe [Flow]} -> Row
  deriving (Show)

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
