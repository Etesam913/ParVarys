{-# LANGUAGE NamedFieldPuns #-}

module Controller
  ( Coflow(..)
  , toCoflows
  , parToCoflows
  , getSwitchBandwidth
  , getGamma
  , sebf
  , parSebf
  ) where

import           Control.DeepSeq                ( NFData
                                                , rnf
                                                )
import           Control.Parallel.Strategies    ( parBuffer
                                                , rdeepseq
                                                , using
                                                )
import qualified Data.IntMap.Lazy              as IntMap
import qualified Data.List.Key                 as Key
import           Data.List.Split                ( chunksOf )
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Ratio                     ( (%) )

import           Generator                      ( CSP(..)
                                                , Flow(..)
                                                , Switch(..)
                                                )


data FlowDirection = Ingress | Egress deriving (Eq, Show)

instance NFData FlowDirection where
  rnf dir = dir `seq` ()

instance Ord FlowDirection where
  a <= b = case (a, b) of
    (Egress, Ingress) -> False
    _                 -> True

-- coflow: id  flows  flowsByIngress  flowsByEgress
data Coflow = Coflow Int [Flow] Switch2Flow Switch2Flow
  deriving Show

instance NFData Coflow where
  rnf (Coflow cid flows iFlows eFlows) =
    cid `seq` rnf flows `seq` rnf iFlows `seq` rnf eFlows


type Switch2Flow = IntMap.IntMap [Flow]
type CoflowMap = IntMap.IntMap Coflow
type BandwidthTable = Map.Map (Int, FlowDirection) Int


maxParSparks :: Int
maxParSparks = 4000

updateMap :: Int -> Flow -> Switch2Flow -> Switch2Flow
updateMap k v = IntMap.alter f k
 where
  f pv = case pv of
    Nothing -> Just [v]
    Just vs -> Just $ v : vs

addFlow :: CoflowMap -> (Int, Flow) -> CoflowMap
addFlow currMap (ingressPort, flow) = IntMap.alter f (coflowId flow) currMap
 where
  egressPort = destinationId flow
  f val = case val of
    Nothing -> Just $ Coflow (coflowId flow)
                             [flow]
                             (IntMap.singleton ingressPort [flow])
                             (IntMap.singleton egressPort [flow])
    Just (Coflow cid coflow flowsByISwitch flowsByESwitch) -> Just $ Coflow
      cid
      (flow : coflow)
      (updateMap ingressPort flow flowsByISwitch)
      (updateMap egressPort flow flowsByESwitch)

update :: CoflowMap -> Switch -> CoflowMap
update currMap switch =
  foldl addFlow currMap $ zip (repeat $ iId switch) (flows switch)

-- Assumes that the coflowId of the two coflows passed in are the same
mergeCoflow :: Coflow -> Coflow -> Coflow
mergeCoflow (Coflow cid flows ingress egress) (Coflow _ flows' ingress' egress')
  = Coflow cid
           (flows ++ flows')
           (IntMap.unionWith (++) ingress ingress')
           (IntMap.unionWith (++) egress egress')

toCoflows :: CSP -> CoflowMap
toCoflows csp = foldl update IntMap.empty $ ingressSwitches csp

parToCoflows :: CSP -> CoflowMap
parToCoflows csp = IntMap.unionsWith
  mergeCoflow
  (map f switchess `using` parBuffer maxParSparks rdeepseq)
 where
  f         = IntMap.unionsWith mergeCoflow . map (update IntMap.empty)
  switchess = chunksOf 10 $ ingressSwitches csp

getSwitchBandwidth :: CSP -> BandwidthTable
getSwitchBandwidth csp = Map.fromList $ concatMap f switches where
  f (Switch sid _ iBw eBw) = [((sid, Ingress), iBw), ((sid, Egress), eBw)]
  switches = ingressSwitches csp ++ egressSwitches csp

parGetSwitchBandwidth :: CSP -> BandwidthTable
parGetSwitchBandwidth csp = Map.fromList $ concat
  (map (concatMap f) switchess `using` parBuffer maxParSparks rdeepseq)
 where
  f (Switch sid _ iBw eBw) = [((sid, Ingress), iBw), ((sid, Egress), eBw)]
  switchess = chunksOf 20 $ ingressSwitches csp ++ egressSwitches csp

getGamma :: BandwidthTable -> Coflow -> Rational
getGamma bwTbl (Coflow _ _ ingressFlows egressFlows) = max
  (maximum ingressTimes)
  (maximum egressTimes)
 where
  sumFlows :: (Int, [Flow]) -> (Int, Int)
  sumFlows (switchId, flows) = (switchId, foldl (\a el -> a + size el) 0 flows)

  calcTime :: FlowDirection -> (Int, Int) -> Rational
  calcTime flowDir (switchId, flowSize) = fromIntegral flowSize
    % fromIntegral bandwidth
    where bandwidth = fromMaybe 0 $ Map.lookup (switchId, flowDir) bwTbl

  ingressTimes = map (calcTime Ingress . sumFlows) $ IntMap.toList ingressFlows
  egressTimes  = map (calcTime Egress . sumFlows) $ IntMap.toList egressFlows


-- Given a Coflow Scheduling Problem, use Shortest Effective Bottleneck First
-- heuristic to order the Coflows.
--
-- Returns [(coflow id, effective bottleneck)]
sebf :: CSP -> [(Int, Rational)]
sebf csp = Key.sort snd $ map f coflows
 where
  switchLinkRates = getSwitchBandwidth csp
  coflows         = IntMap.toList $ toCoflows csp
  f (cid, coflow) = (cid, getGamma switchLinkRates coflow)

-- Parallel version of sebf
parSebf :: CSP -> [(Int, Rational)]
parSebf csp = Key.sort
  snd
  (map f coflows `using` parBuffer maxParSparks rdeepseq)
 where
  switchLinkRates = parGetSwitchBandwidth csp
  coflows         = IntMap.toList $ parToCoflows csp
  f (cid, coflow) = (cid, getGamma switchLinkRates coflow)
