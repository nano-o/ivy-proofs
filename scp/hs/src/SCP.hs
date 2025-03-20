module SCP where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet

-- $setup
-- >>> s = Set.fromList
-- >>> m = Map.fromList
-- >>> q = NESet.unsafeFromSet . Set.fromList

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


-- TODO: implement it for an FBASGraph, which represents a set of nodes and their slices
