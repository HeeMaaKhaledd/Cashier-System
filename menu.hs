import Interface
import System.Process

main = do
    menu bajs ""

menu i message = do
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
  runMenu (read x :: Int) i

runMenu c i
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
    menu (addToCart (findItem (read x :: Int) i) i) ("You added " ++  (show (findItem (read x :: Int) i)) ++ " to the Cart")

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
    menu (removeFromCart (findItem (read x :: Int) i) i) ("You removed " ++  (show (findItem (read x :: Int) i)) ++ " from the Cart")

  | c == 3 = do
    menu (buy i) ("You bought the following Cart " ++ (show (Interface.getCart i)))

  | c == 0 && (Interface.getUserAdmin i) = adminMenu i "Access Granted"
  | c == 0 = menu i "You dont have access to admin features!"
  | otherwise = menu i "You wrote a non existing number"

adminMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- USER FIXES       --"
  putStrLn "-- 2 -- ITEM FIXES       --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
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
  x <- getLine
  runAdminMenu (read x :: Int) i

runAdminMenu c i
  | c == 1 = adminUserMenu i ""
  | c == 2 = adminItemMenu i ""

  | c == 0 = menu i "You navigated back"
  | otherwise = adminMenu i "You wrote a non existing number"

adminUserMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- CREATE USER      --"
  putStrLn "-- 2 -- REMOVE USER      --"
  putStrLn "-- 3 -- CHANGE USER      --"
  putStrLn "-- 4 -- CHANGE WALLET    --"
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
  putStrLn (show i)
  x <- getLine
  runAdminUserMenu (read x :: Int) i

runAdminUserMenu c i
  | c == 1 = do
    putStrLn "Write name of the new user nd hit endet"
    name <- getLine
    putStrLn "Write id of the new user and hit ENTER"
    userId <- getLine
    putStrLn "Write wallet of the new user and hit ENTER"
    wallet <- getLine
    putStrLn "Write adminstatus of the new user and hit enter"
    status <- getLine

    adminUserMenu (Interface.createUser name (read userId :: Int) (read wallet :: Int) (0) (read status :: Bool) i) "Created User"

  | c == 2 = do
    putStrLn "Write id of the user you want to remove"
    x <- getLine
    adminUserMenu (Interface.removeUser (findUser (read x :: Int) i) i) "Removed a user"
  | c == 3 = do
    putStrLn "Write the id of the user you want to change"
    x <- getLine
    adminChangeUserMenu i "" (findUser (read x :: Int) i)

  | c == 4 = do
  putStrLn "Write the id of the user you want to do wallet functions on"
  x <- getLine
  adminChangeUserMenu i "" (findUser (read x :: Int) i)

  | c == 0 = adminMenu i "You navigated back"
  | otherwise = adminUserMenu i "You wrote a non existing number"

adminChangeUserMenu i message u = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- SET USERNAME     --"
  putStrLn "-- 2 -- SET USER ID      --"
  putStrLn "-- 3 -- FILL WALLET      --"
  putStrLn "-- 4 -- REDUCE WALLET    --"
  putStrLn "-- 5 -- CLEAR WALLET     --"
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
  x <- getLine
  runAdminChangeUserMenu (read x :: Int) i u

runAdminChangeUserMenu c i u
  | c == 1 = do
    putStrLn "Write the name you want this user to have and hit enter"
    name <- getLine
    let
      k = Interface.setUserName name u i
      in adminChangeUserMenu k "changed name" (Interface.getUser k)

  | c == 2 = do
    putStrLn "Write the id you want this user to have and hit enter"
    userId <- getLine
    let k = Interface.setUserId (read userId :: Int) u i
      in adminChangeUserMenu k "changed id" (Interface.getUser k)

  | c == 3 = do
    putStrLn "Write the amount you want to add for this users wallet and hit enter"
    amount <- getLine
    let
      k = Interface.fillWallet (read amount :: Int) u i
      in adminChangeUserMenu k "filled wallet" (Interface.getUser k)

  | c == 4 = do
    putStrLn "Write the amount you want to remove for this users wallet and hit enter"
    amount <- getLine
    let
      k = Interface.reduceWallet (read amount :: Int) u i
      in adminChangeUserMenu k "reduced wallet" (Interface.getUser k)

  | c == 5 = do
    let
      k = Interface.clearWallet u i
      in adminChangeUserMenu k "cleared wallet" (Interface.getUser k)

  | c == 0 = adminUserMenu i "You navigated back"
  | otherwise = adminChangeUserMenu i "You wrote a non existing number" u

adminItemMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- CREATE ITEM      --"
  putStrLn "-- 2 -- REMOVE ITEM      --"
  putStrLn "-- 3 -- CHANGE ITEM      --"
  putStrLn "-- 4 --                  --"
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
  x <- getLine
  

adminChangeItemMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn (show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- SET NAME         --"
  putStrLn "-- 2 -- SET EAN          --"
  putStrLn "-- 3 -- SET PRICE        --"
  putStrLn "-- 4 -- ADD STOCK        --"
  putStrLn "-- 5 -- REMOVE STOCK     --"
  putStrLn "-- 6 -- REPLACE STOCK    --"
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
