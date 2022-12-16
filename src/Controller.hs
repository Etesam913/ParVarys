module Controller
  ( Coflow(..),
    toCoflows,
    getSwitchBandwidth,
    getGamma,
    sebf,
    parSebf,
  )
where

import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map as Map
import qualified Data.List.Key as Key
import Data.Maybe (fromMaybe)
import Data.Ratio ( (%) )

import Generator
  ( CSP(..),
    Flow(..),
    Switch(..),
  )

data FlowDirection = Ingress | Egress deriving (Eq, Show)

instance Ord FlowDirection where
  a <= b = case (a, b) of { (Egress, Ingress) -> False; _ -> True }

data Coflow = Coflow
  Integer     -- coflow id 
  [Flow]      -- all flows belonging to this coflow
  Switch2Flow -- flows grouped by ingress switch
  Switch2Flow -- flows grouped by egress switch
  deriving (Show)

type Switch2Flow = Map.Map Integer [Flow]
type CoflowMap = Map.Map Integer Coflow
type BandwidthTable = Map.Map (Integer, FlowDirection) Integer


toCoflows :: CSP -> CoflowMap
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


getSwitchBandwidth :: CSP -> BandwidthTable
getSwitchBandwidth csp =
  Map.fromList $ concatMap f switches where
    f (Switch sid _ iBw eBw) = [((sid, Ingress), iBw), ((sid, Egress), eBw)]
    switches = ingressSwitches csp ++ egressSwitches csp


getGamma :: BandwidthTable -> Coflow -> Rational
getGamma bwTbl (Coflow _ _ ingressFlows egressFlows) =
  max (maximum ingressTimes) (maximum egressTimes)
  where
    sumFlows :: (Integer, [Flow]) -> (Integer, Integer)
    sumFlows (switchId, flows) =  (switchId, foldl (\a el -> a + size el) 0 flows)

    calcTime :: FlowDirection -> (Integer, Integer) -> Rational
    calcTime flowDir (switchId, flowSize) =
      fromInteger flowSize % fromInteger bandwidth
      where bandwidth = fromMaybe 0 $ Map.lookup (switchId, flowDir) bwTbl

    ingressTimes = map (calcTime Ingress . sumFlows) $ Map.toList ingressFlows
    egressTimes  = map (calcTime Egress  . sumFlows) $ Map.toList egressFlows


-- Given a Coflow Scheduling Problem, use Shortest Effective Bottleneck First
-- heuristic to order the Coflows.
--
-- Returns a list of (coflow id, effective bottleneck)
sebf :: CSP -> [(Integer, Rational)]
sebf csp =
  Key.sort snd $ map f coflows
  where
    switchLinkRates = getSwitchBandwidth csp
    coflows = Map.toList $ toCoflows csp
    f (cid, coflow) = (cid, getGamma switchLinkRates coflow)

-- Parallel version of sebf
parSebf :: CSP -> [(Integer, Rational)]
parSebf csp =
  Key.sort snd $ parMap rdeepseq f coflows
  where
    switchLinkRates = getSwitchBandwidth csp
    coflows = Map.toList $ toCoflows csp
    f (cid, coflow) = (cid, getGamma switchLinkRates coflow)
