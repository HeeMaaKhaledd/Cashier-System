-- to be implemented.

--Hashing Table -- is it a good thing in haskell?
--perhaps just use a binary tree?
--Black and white tree?
-- Is there any other datatype that is better?

type Database a = [a]

delete a [] = error "not in list"
delete a xs = deleteAUX a xs []
  where deleteAUX a (x:xs) n
          | a == x = n ++ xs
          | otherwise = deleteAUX a xs (n++x)

insert a xs = a:xs

find a xs = undefined
