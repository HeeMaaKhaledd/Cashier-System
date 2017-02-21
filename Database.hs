-- to be implemented.

--Hashing Table -- is it a good thing in haskell?
--perhaps just use a binary tree?
--Black and white tree?
-- Is there any other datatype that is better?
module Item(Database,deleteWithID,insert,grabWithID) where

type Id = Int -- eanCode for item, userId for users...

type Database a = [(a,Id)]

deleteWithID :: Id -> Database a -> Database a
deleteWithID i [] = error "not in Database"
deleteWithID i xs = deleteWithIDAUX i xs []
  where deleteWithIDAUX j (y:ys) n
          | j == (snd y) = n ++ ys
          | otherwise = deleteWithIDAUX j ys (n++[y])

insert :: a -> Id -> Database a -> Database a
insert a i xs = (a,i):xs

grabWithID :: Id -> Database a -> a
grabWithID i [] = error "not in our database"
grabWithID i xs
  | i == (snd y) = fst y
  | otherwise grabWithIDAUX i ys
