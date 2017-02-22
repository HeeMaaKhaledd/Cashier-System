-- to be implemented.

--Hashing Table -- is it a good thing in haskell?
--perhaps just use a binary tree?
--Black and white tree?
-- Is there any other datatype that is better?
module Database(Database,deleteWithID,insert,grabWithId,empty) where

type Id = Int -- eanCode for item, userId for users...

type Database a = [(a,Id)]

empty :: Database a
empty = []

deleteWithID :: Id -> Database a -> Database a
deleteWithID i [] = error "not in Database"
deleteWithID i xs = deleteWithIDAUX i xs []
  where deleteWithIDAUX j (y:ys) n
          | j == (snd y) = n ++ ys
          | otherwise = deleteWithIDAUX j ys (n++[y])

insert :: a -> Id -> Database a -> Database a
insert a i xs = (a,i):xs

grabWithId :: Id -> Database a -> a
grabWithId i [] = error "not in our database"
grabWithId i (x:xs)
  | i == (snd x) = fst x
  | otherwise = grabWithId i xs
