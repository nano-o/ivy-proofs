{-# OPTIONS_GHC "-Wno-unused-imports" #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RecordWildCards #-}
module SCP where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet

import Data.Containers.ListUtils qualified as ListUtils
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Maybe qualified as Maybe

-- $setup
-- >>> s = Set.fromList
-- >>> m = Map.fromList
-- >>> q = NESet.unsafeFromSet . Set.fromList
-- >>> q_ = Foldable.toList
-- >>> q__ = map Foldable.toList . Foldable.toList . unQSlices
-- >>> ne = maybe (error "list was empty") id . NonEmpty.nonEmpty
-- >>> ne_ = NonEmpty.toList
-- >>> ne__ = map NonEmpty.toList . NonEmpty.toList



-- | Nodes with which to agree
type Slice nid = NESet nid

-- | Quorum slices of a node
newtype QSlices nid = QSlices {unQSlices :: NESet (Slice nid)} deriving Show

-- | Mapping of nodes to their quorum slices
type NodesSlices nid = Map nid (QSlices nid)

class QuorumSlices q nid where

    -- | Is the candidate-slice a quorum for all the nodes' quorum-slices?
    isQuorum :: Foldable f => Slice nid -> f (q nid) -> Bool

    -- | Is the candidate-slice a quorum for a single node's quorum-slices?
    -- (I.e. Is any one of the quorum-slices a subset of the candidate slice?)
    isQuorumSlice :: Slice nid -> q nid -> Bool

    -- | Search for a quorum among the given nodes (and their last known
    -- quorum-slices).
    findQuorum :: Map nid (q nid) -> Maybe (Slice nid)



-- | @isQuorum candidate slices@ computes whether @candidate@ is a quorum based
-- on the @slices@ learned in protocol messages: Is at least one slice from
-- each node in the @candidate@ quorum?
--
-- >>> aSlices = QSlices $ q [q [1,2], q [2,3], q [3,4]]
-- >>> bSlices = QSlices $ q [q [0,1,2], q [2,3,4]]
-- >>> cSlices = QSlices $ q [q [0,1], q [2,3], q [4,5]]
--
-- >>> q [1,2,3] `isQuorum` [aSlices, bSlices]
-- False
--
-- >>> q [1,2,3] `isQuorum` [bSlices, cSlices]
-- False
--
-- >>> q [1,2,3] `isQuorum` [cSlices, aSlices]
-- True
--
-- >>> q [1,2,3] `isQuorum` ([] :: [QSlices Int]) -- Edge case: no nodes
-- False
instance Ord nid => QuorumSlices QSlices nid where
    isQuorum candidate nodesSlices =
        not (null nodesSlices) &&
        all (isQuorumSlice candidate) nodesSlices

    isQuorumSlice candidate (QSlices quorumSlices) =
        any (`NESet.isSubsetOf` candidate) quorumSlices

    findQuorum m = do
        candidate <- NESet.nonEmptySet $ Map.keysSet m
        let m' = Map.filter (isQuorumSlice candidate) m
        if Map.size m == Map.size m'
        then return candidate
        else findQuorum m'



-- | @allNchooseK xs k@ returns all unique choices of @k@ elements from @xs@,
-- if @k@ is in the range [0, @length xs@]. Not efficient.
--
-- >>> allNchooseK [] 0
-- [[]]
--
-- >>> allNchooseK [1,2,3] 0
-- [[]]
--
-- >>> allNchooseK [1,2,3] 1
-- [[1],[2],[3]]
--
-- >>> allNchooseK [1,2,3] 2
-- [[1,2],[1,3],[2,3]]
--
-- >>> allNchooseK [1,2,3] 3
-- [[1,2,3]]
--
-- >>> allNchooseK [1,2,3] 4
-- ...cannot choose 4 elements from a list of length 3
-- ...
allNchooseK :: [a] -> Int -> [[a]]
allNchooseK pool k
    | 0 < k, k < n, x:xs <- pool = ((x :) <$> allNchooseK xs (k - 1)) ++ allNchooseK xs k
    | 0 == k = [[]]
    | n == k = [pool]
    | otherwise = error $ "cannot choose "++show k++" elements from a list of length "++show (n)
  where
    n = length pool



data QSet nid
    = QSetNode { threshold::Int, validators::[nid], inner::[QSet nid] }
    | QSetLeaf { threshold::Int, validators::[nid] }

intoQSet_ :: [Slice nid] -> QSet nid
-- i have two sets AB and BC
--
-- i could represent this as "either set" with
-- (QSetNode 1 [] [QSetLeaf 2 [AB], QSetLeaf 2 [BC])
--
-- but B is in common between them so i could pull it out
-- (QSetNode 2 [B] [QSetLeaf 1 [A], QSetLeaf 1 [C]])
--
-- but the singleton leafs can be combined
-- (QSetNode 2 [B] [QSetLeaf 1 [AC]])
--
-- this example does not address how we might end up with alternatives
intoQSet_ = undefined


-- | Threshold-clamped interpretation of @QSet@ to a list of all possible lists.
--
-- >>> fromQSet_ (QSetLeaf 99 "") -- Edge case: threshold greater than size of validator list
-- [""]
--
-- >>> fromQSet_ (QSetLeaf 0 "abc") -- Edge case: threshold zero despite validators being present
-- [""]
--
-- >>> fromQSet_ (QSetLeaf 2 "abc")
-- ["ab","ac","bc"]
--
-- >>> fromQSet_ (QSetNode 2 "x" [QSetLeaf 2 "abc"])
-- ...
-- ["xab","xac","xbc"]
--
-- >>> fromQSet_ (QSetNode 2 "xy" [QSetLeaf 2 "abc"])
-- ...
-- ["xy","xab","xac","xbc","yab","yac","ybc"]
--
-- >>> fromQSet_ (QSetNode 2 "" [QSetLeaf 2 "abc", QSetLeaf 1 "e", QSetLeaf 1 "f"])
-- ["abe","ace","bce","abf","acf","bcf","ef"]
fromQSet_ :: QSet nid -> [[nid]]
fromQSet_ = \case
    QSetLeaf{..} ->
        allNchooseK validators (clampThreshold validators threshold)
    QSetNode{..} ->
        let pool = (Left <$> validators) ++ (Right . fromQSet_ <$> inner) in
        concatMap flatten $ allNchooseK pool (clampThreshold pool threshold)
  where
    -- FIXME: use different clamps in the two cases above; the node case should
    -- be |validators|+|pool| but also we need to filter the pool?
    clampThreshold validators =
        max 0 . min (length validators)
    flatten :: [Either nid [[nid]]] -> [[nid]]
    flatten =
        foldr (flip cross) [[]]
    cross :: [[nid]] -> Either nid [[nid]] -> [[nid]]
    cross slices = \case
        Left x -> (x:) <$> slices
        Right xs -> (++) <$> xs <*> slices

-- | Reify the sets represented by a QSet.
--
-- >>> q__ <$> fromQSet (QSetLeaf 2 [1,2,3])
-- Just [[1,2],[1,3],[2,3]]
--
-- >>> q__ <$> fromQSet (QSetNode 2 "ab" [QSetLeaf 2 "cde"])
-- Just ["ab","acd","ace","ade","bcd","bce","bde"]
--
-- !>>> q__ <$> fromQSet (QSetNode 2 "ab" [QSetLeaf 0 "cde"])
-- !Just [FIXME
fromQSet :: (Show nid, Ord nid) => QSet nid -> Maybe (QSlices nid)
fromQSet = fmap QSlices . f . Maybe.mapMaybe f . fromQSet_
  where
    f :: Ord a => [a] -> Maybe (NESet a)
    f = fmap NESet.fromList . NonEmpty.nonEmpty

-- |
instance Ord nid => QuorumSlices QSet nid where
    isQuorum candidate nodesSlices =
        not (null nodesSlices) &&
        all (`anySliceIsSubsetOf` candidate) nodesSlices

    isQuorumSlice = flip anySliceIsSubsetOf

    findQuorum = _1

-- | Is any slice represented by the QSet a subset of the given slice?
anySliceIsSubsetOf :: Ord nid => QSet nid -> Slice nid -> Bool
anySliceIsSubsetOf qs candidate =
    case qs of
        QSetLeaf{..} ->
            threshold <= ctCandidatesIn validators
        QSetNode{..} ->
            let ctInnersSat = length . filter id $ map (`anySliceIsSubsetOf` candidate) inner in
            threshold <= ctCandidatesIn validators + ctInnersSat
  where
    ctCandidatesIn validators =
        Set.size $
            Set.intersection
                (Set.fromList validators)
                (NESet.toSet candidate)



newtype QSetFlat nid = QSetFlat (Int, [Either (QSetFlat nid) nid])

intoQSetFlat :: QSet nid -> QSetFlat nid
intoQSetFlat = undefined -- TODO

fromQSetFlat :: QSetFlat nid -> QSet nid
fromQSetFlat = undefined -- TODO
