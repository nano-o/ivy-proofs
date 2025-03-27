{-# OPTIONS_GHC "-Wno-unused-imports" #-}
{-# LANGUAGE RecordWildCards #-}
module SCP where
import Debug.Trace

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

-- $setup
-- >>> s = Set.fromList
-- >>> m = Map.fromList
-- >>> q = NESet.unsafeFromSet . Set.fromList
-- >>> q_ = Foldable.toList
-- >>> q__ = map Foldable.toList . Foldable.toList
-- >>> ne = maybe (error "list was empty") id . NonEmpty.nonEmpty
-- >>> ne_ = NonEmpty.toList
-- >>> ne__ = map NonEmpty.toList . NonEmpty.toList



-- | Nodes with which to agree
type Slice nid = NESet nid

-- | Quorum slices of a node
type QSlices nid = NESet (Slice nid)

-- | Mapping of nodes to their quorum slices
type NodesSlices nid = Map nid (QSlices nid)



-- | @isQuorum candidate slices@ computes whether @candidate@ is a quorum based
-- on the @slices@ learned in protocol messages: Is at least one slice from
-- each node in the @candidate@ quorum?
--
-- >>> aSlices = q [q [1,2], q [2,3], q [3,4]]
-- >>> bSlices = q [q [0,1,2], q [2,3,4]]
-- >>> cSlices = q [q [0,1], q [2,3], q [4,5]]
--
-- >>> q [1,2,3] `isQuorum` [aSlices, bSlices]
-- Just False
--
-- >>> q [1,2,3] `isQuorum` [bSlices, cSlices]
-- Just False
--
-- >>> q [1,2,3] `isQuorum` [cSlices, aSlices]
-- Just True
--
-- >>> q [1,2,3] `isQuorum` [] -- no nodes
-- Nothing
isQuorum :: (Foldable f, Ord nid) => NESet nid -> f (QSlices nid) -> Maybe Bool
isQuorum candidate nodesSlices
    | null nodesSlices = Nothing
    | otherwise = Just $ all (any (\slice -> slice `NESet.isSubsetOf` candidate)) nodesSlices



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
allNchooseK :: Ord a => [a] -> Int -> [[a]]
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
fromQSet_ :: (Show nid, Ord nid) => QSet nid -> [[nid]]
fromQSet_ = \case
    QSetLeaf{..} ->
        allNchooseK validators (clampThreshold validators threshold)
    QSetNode{..} ->
        let pool = (Left <$> validators) ++ (Right . fromQSet_ <$> inner) in
        concatMap flatten $ allNchooseK pool (clampThreshold pool threshold)
  where
    clampThreshold validators =
        max 0 . min (length validators)
    flatten :: [Either nid [[nid]]] -> [[nid]]
    flatten =
        foldr (flip cross) [[]]
    cross :: [[nid]] -> Either nid [[nid]] -> [[nid]]
    cross slices = \case
        Left x -> (x:) <$> slices
        Right xs -> (++) <$> xs <*> slices

---- | Reify the sets represented by a QSet.
----
---- !!>>> q__ $ fromQSet (QSetLeaf 2 (q [1,2,3]))
---- !![[1,2],[1,3],[2,3]]
----
---- >>> q__ $ fromQSet (QSetNode 2 (q "ab") (QSetLeaf 2 (q "cde")))
---- ["ab","acd","ace","ade","bcd","bce","bde"]
--fromQSet :: (Show nid, Ord nid) => QSet nid -> QSlices nid
--fromQSet = NESet.fromList . fmap NESet.fromList . fromQSet_
--
------toQSet :: QSlices nid -> QSet nid
------toQSet


newtype QSetFlat nid = QSetFlat (Int, [Either (QSetFlat nid) nid]

