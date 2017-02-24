import Interface
import System.Process

main = do
    scndMain bajs ""

scndMain i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 0 -- GET ITEMS LIST   --"
  putStrLn "-- 1 -- ADD TO CART      --"
  putStrLn "-- 2 -- REMOVE FROM CART --"
  putStrLn "-- 3 -- BUY CART         --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 9 -- ADMIN ONLY       --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn message
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"

  x <- getLine
  run (read x :: Int) i

run c i
  | c == 0 = do
    system "clear"
    putStrLn "---------------------------"
    putStrLn "--     ITEM IN SHOP      --"
    putStrLn "---------------------------"
    mapM_ putStrLn (map show(Interface.getDatabaseItem i))
    putStrLn "---------------------------"
    putStrLn "--     Type anything     --"
    putStrLn "---------------------------"
    x <- getLine
    scndMain i ""

  | c == 1 = do
    system "clear"
    putStrLn "---------------------------"
    putStrLn "-- ADD THE ITEM TO CART  --"
    putStrLn "---------------------------"
    putStrLn "--     ITEM IN SHOP      --"
    putStrLn "---------------------------"
    mapM_ putStrLn (map show(Interface.getDatabaseItem i))
    putStrLn "---------------------------"
    putStrLn "--  Write your ean code  --"
    putStrLn "---------------------------"
    x <- getLine
    scndMain (addToCart (findItem (read x :: Int) i) i) ("You added " ++  (show (findItem (read x :: Int) i)) ++ " to the Cart")

  | c == 2 = do
    system "clear"
    putStrLn "---------------------------"
    putStrLn "-- REMOVE ITEM FROM CART --"
    putStrLn "---------------------------"
    putStrLn "--     ITEM IN CART      --"
    putStrLn "---------------------------"
    mapM_ putStrLn (map show(Interface.getCart i))
    putStrLn "---------------------------"
    putStrLn "--  Write your ean code  --"
    putStrLn "---------------------------"
    x <- getLine
    scndMain (removeFromCart (findItem (read x :: Int) i) i) ("You removed " ++  (show (findItem (read x :: Int) i)) ++ " from the Cart")

  | c == 3 = do
    scndMain (buy i) ("You bought the following Cart " ++ (show (Interface.getCart i)))
  | c == 9 = undefined



-- scndMain interface = do
--   putStrLn "What do you want to do?"
--   putStrLn "[1]Buy item"
--   putStrLn "[2]Insert item"
--   putStrLn "[3]Remove item"
--   x <- getLine
--   run (read x :: Int) interface
--
-- run choice interface
--   | choice == 1 = do
--         putStrLn "enter EAN code"
--         x <- getLine
--         scndMain Interface.buy (findItem (read x :: Int) interface) interface
--
--   | choice == 2 = do
--         putStrLn "Good input"
--   | choice == 3 = do
--         putStrLn "Good input"

  -- | otherwise = do
  --         putStrLn "Bad input, try again"
  --         run (read x :: Int)
