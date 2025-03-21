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
import Data.Foldable qualified as Foldable

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



-- | Take @n@ from a non-empty list, assuming that @n@ is greather than 0. Not
-- efficient.
--
-- >>> takeSome 3 (ne [1,2,3,4])
-- 1 :| [2,3]
--
-- >>> takeSome 0 (ne [1,2,3,4])
-- ... count must be greater than 0, ...
-- ...
takeSome :: Int -> NonEmpty a -> NonEmpty a
takeSome n xs
    = maybe (error $ "takeSome: count must be greater than 0, got " ++ show n) id
    . NonEmpty.nonEmpty
    $ NonEmpty.take n xs

-- | @allNchooseK xs k@ returns all unique choices of @k@ elements from @xs@,
-- assuming that @k@ is greater than 0. Not efficient.
--
-- >>> ne__ $ allNchooseK (ne [1,2,3]) 2
-- [[1,2],[2,3],[1,3]]
--
-- >>> ne__ $ allNchooseK (ne [1,2,3]) 0
-- ...Exception...
-- ...
allNchooseK :: (Eq a, Ord a) => NonEmpty a -> Int -> NonEmpty (NonEmpty a)
allNchooseK xs k
    = NonEmpty.nub
    . fmap (NonEmpty.sort . takeSome k)
    $ NonEmpty.permutations1 xs



data QSet nid
    = QSetNode { threshold::Int, validators::NESet nid, inner::QSet nid }
    | QSetLeaf { threshold::Int, validators::NESet nid }

fromQSet_ :: (Show nid, Ord nid) => QSet nid -> NonEmpty (NonEmpty nid)
fromQSet_ = \case
    QSetLeaf{..} -> allNchooseK (NESet.toList validators) threshold
    QSetNode{..} ->
        let xs = (Left <$> NESet.toList validators) <> (Right <$> fromQSet_ inner) in
        traceShow (fmap NonEmpty.toList <$> NonEmpty.toList xs) $
        flatten . traceShowId <$> allNchooseK xs threshold
  where
    flatten = \case
        Left n        :| []   -> n :| []
        Left n        :| x:xs -> n `NonEmpty.cons` flatten (x:|xs)
        Right (n:|ns) :| []   -> n :| ns
        Right (n:|ns) :| x:xs -> (n:|ns) <> flatten (x:|xs)

-- | Reify the sets represented by a QSet.
--
-- !!>>> q__ $ fromQSet (QSetLeaf 2 (q [1,2,3]))
-- !![[1,2],[1,3],[2,3]]
--
-- >>> q__ $ fromQSet (QSetNode 2 (q "ab") (QSetLeaf 2 (q "cde")))
-- ["ab","acd","ace","ade","bcd","bce","bde"]
fromQSet :: (Show nid, Ord nid) => QSet nid -> QSlices nid
fromQSet = NESet.fromList . fmap NESet.fromList . fromQSet_

----toQSet :: QSlices nid -> QSet nid
----toQSet


--newtype QSetFlat nid = QSetFlat (Int, NESet (Either (QSetFlat nid) nid))
