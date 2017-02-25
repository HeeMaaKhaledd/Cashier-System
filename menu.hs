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
  putStrLn "-- 1 -- ADD TO CART      --"
  putStrLn "-- 2 -- REMOVE FROM CART --"
  putStrLn "-- 3 -- BUY CART         --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 0 -- ADMIN ONLY       --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn message
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"

  x <- getLine
  run (read x :: Int) i

run c i
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

  | c == 0 && (Interface.getUserAdmin i) = adminMain i "Access Granted"
  | c == 0 = scndMain i "You dont have access to admin features!"
  | otherwise = scndMain i "You wrote a non existing number"

adminMain i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- USER FIXES       --"
  putStrLn "-- 2 -- ITEM FIXES       --"
  putStrLn "-- 3 -- CART FIXES       --"
  putStrLn "-- 4 -- DATABASE FIXES   --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 0 -- NAVIGATE BACK    --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn message
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"

adminUserMain i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- USER FIXES       --"
  putStrLn "-- 2 -- ITEM FIXES       --"
  putStrLn "-- 3 -- CART FIXES       --"
  putStrLn "-- 4 -- DATABASE FIXES   --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 0 -- NAVIGATE BACK    --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn message
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
