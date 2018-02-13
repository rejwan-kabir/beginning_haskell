{-# LANGUAGE LambdaCase #-}

module Client.Chapter4 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Lazy as HashMap

-- Map.adjust does not change the map if the key is not present,
-- Map.insert does.
-- Map.alter
--     :: Ord k => (Maybe a -> Maybe a) -> k -> Map.Map k a -> Map.Map k a
-- Map.alter is the mother of insert, delete and adjust
maps =
  let m1 = Map.singleton "shuvo" 1
      m2 = Map.insert "arshi" 2 m1
      m3 = Map.insert "shuvo" 3 m2
      m4 = Map.insertWith (+) "arshi" 7 m3
  in (m1, m2, m3, m4)

insertMap :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
insertMap k a =
  Map.alter
    (\case
       Nothing -> Just a
       Just _ -> Just a)
    k

adjustMap :: Ord k => (a -> a) -> k -> Map.Map k a -> Map.Map k a
adjustMap f =
  Map.alter
    (\case
       Nothing -> Nothing
       Just a -> Just (f a))

deleteMap :: Ord k => k -> Map.Map k a -> Map.Map k a
deleteMap = Map.alter (\_ -> Nothing)

-- Map.union, difference, intersection, intersectionWith
-- map, foldl, foldr
-- findMin, findMax, deleteMin, deleteMax, updateMin, updateMax
mapFoldMap =
  let m = Map.fromList [("shuvo", 100), ("arshi", 10)]
  in (Map.map (* 2) m, Map.foldl (+) 0 m)

-- Set.size, union, intersection, difference
-- map , foldl, foldr
-- findMin, findMax, deleteMin, deleteMax, updateMin, updateMax




