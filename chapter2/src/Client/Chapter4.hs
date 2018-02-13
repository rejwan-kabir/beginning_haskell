{-# LANGUAGE LambdaCase #-}

module Client.Chapter4 where

import qualified Data.Map as M

-- M.adjust does not change the map if the key is not present,
-- M.insert does.
-- M.alter
--     :: Ord k => (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a
-- M.alter is the mother of insert, delete and adjust
maps =
  let m1 = M.singleton "shuvo" 1
      m2 = M.insert "arshi" 2 m1
      m3 = M.insert "shuvo" 3 m2
      m4 = M.insertWith (+) "arshi" 7 m3
  in (m1, m2, m3, m4)

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k a =
  M.alter
    (\case
       Nothing -> Just a
       Just _ -> Just a)
    k

adjust' :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' f =
  M.alter
    (\case
       Nothing -> Nothing
       Just a -> Just (f a))

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' = M.alter (\_ -> Nothing)

-- M.union, difference, intersection, intersectionWith
-- map, foldl, foldr
-- findMin, findMax, deleteMin, deleteMax, updateMin, updateMax
mapFoldMap =
  let m = M.fromList [("shuvo", 100), ("arshi", 10)]
  in (M.map (* 2) m, M.foldl (+) 0 m)
