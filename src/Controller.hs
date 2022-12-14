module Controller
  ( Coflow(..),
    toCoflows,
  )
where

import qualified Data.Map as Map

import Generator
  ( CSP(..),
    Flow(..),
    Switch(..),
  )

data Coflow = Coflow {
    cfId           :: Integer,
    coflows          :: [Flow],
    flowsByIngress :: Map.Map Integer [Flow],
    flowsByEgress  :: Map.Map Integer [Flow]
  } deriving (Show)

toCoflows :: CSP -> Map.Map Integer Coflow
toCoflows csp =
  foldl update Map.empty $ ingressSwitches csp
  where
    updateMap :: Integer -> Flow -> Map.Map Integer [Flow] -> Map.Map Integer [Flow]
    updateMap k v =
      Map.alter f k
      where f pv = case pv of Nothing -> Just [v]
                              Just vs -> Just $ v : vs

    addFlow :: Map.Map Integer Coflow -> (Integer, Flow) -> Map.Map Integer Coflow
    addFlow currMap (switchId,flow) =
       Map.alter f (coflowId flow) currMap
       where f val =
              case val of
                Nothing -> Just $ Coflow
                            (coflowId flow) [flow]
                            (Map.singleton switchId [flow])
                            (Map.singleton (destinationId flow) [flow])
                Just coflow -> Just $ Coflow
                                (cfId coflow) (flow : coflows coflow)
                                (updateMap switchId flow $ flowsByIngress coflow)
                                (updateMap (destinationId flow) flow $ flowsByEgress coflow)

    update :: Map.Map Integer Coflow -> Switch -> Map.Map Integer Coflow
    update currMap switch = foldl addFlow currMap $ zip (repeat $ iId switch) (flows switch)
