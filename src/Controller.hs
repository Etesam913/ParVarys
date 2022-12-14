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

type Switch2Flow = Map.Map Integer [Flow]
type CoflowMap = Map.Map Integer Coflow

data Coflow = Coflow
  Integer     -- coflow id 
  [Flow]      -- all flows belonging to this coflow
  Switch2Flow -- flows grouped by ingress switch
  Switch2Flow -- flows grouped by egress switch
  deriving (Show)

toCoflows :: CSP -> Map.Map Integer Coflow
toCoflows csp =
  foldl update Map.empty $ ingressSwitches csp
  where
    updateMap :: Integer -> Flow -> Switch2Flow -> Switch2Flow
    updateMap k v =
      Map.alter f k
      where f pv = case pv of Nothing -> Just [v]
                              Just vs -> Just $ v : vs

    addFlow :: CoflowMap -> (Integer, Flow) -> CoflowMap
    addFlow currMap (ingressPort,flow) =
       Map.alter f (coflowId flow) currMap
       where egressPort = destinationId flow
             f val =
              case val of
                Nothing -> Just $
                  Coflow (coflowId flow) [flow]
                         (Map.singleton ingressPort [flow])
                         (Map.singleton egressPort  [flow])
                Just (Coflow cid coflow flowsByISwitch flowsByESwitch) -> Just $
                  Coflow cid (flow : coflow)
                         (updateMap ingressPort flow flowsByISwitch)
                         (updateMap egressPort  flow flowsByESwitch)

    update :: CoflowMap -> Switch -> CoflowMap
    update currMap switch =
      foldl addFlow currMap $ zip (repeat $ iId switch) (flows switch)
