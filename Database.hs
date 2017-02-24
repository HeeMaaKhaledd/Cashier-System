-- to be implemented.

--Hashing Table -- is it a good thing in haskell?
--perhaps just use a binary tree?
--Black and white tree?
-- Is there any other datatype that is better?
module Database(Database,deleteWithID,delete,insert,grabWithId,empty) where

type Id = Int -- eanCode for item, userId for users...

type Database a = [(a,Id)]

{- empty
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
empty :: Database a
empty = []

{- deleteWithID
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
deleteWithID :: Id -> Database a -> Database a
deleteWithID i [] = error "not in Database"
deleteWithID i xs = deleteWithIDAUX i xs []
  where deleteWithIDAUX j (y:ys) n
          | j == (snd y) = n ++ ys
          | otherwise = deleteWithIDAUX j ys (n++[y])

{- delete
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
delete :: Eq a => a -> Database a -> Database a
delete a [] = error "not in Database"
delete a xs = deleteAUX a xs []
  where deleteAUX j (y:ys) n
          | j == (fst y) = n ++ ys
          | otherwise = deleteAUX j ys (n++[y])

{- insert
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
insert :: a -> Id -> Database a -> Database a
insert a i xs = (a,i):xs

{- grab
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
grab :: Eq a => a -> Database a -> a
grab a [] = error "not in Database"
grab a (x:xs)
  | a == (fst x) = fst x
  | otherwise = grab a xs

{- grabWithId
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
grabWithId :: Id -> Database a -> a
grabWithId i [] = error "not in our database"
grabWithId i (x:xs)
  | i == (snd x) = fst x
  | otherwise = grabWithId i xs
