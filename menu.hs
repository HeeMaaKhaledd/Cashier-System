import Interface
import Data.Text.Internal
import System.Process

exit = do
  system "clear"
  putStrLn "You exited successfully!"

main :: IO()
main = do
    menu k ("Welcome " ++ (Interface.getUserName (Interface.getUser k)))
      where k = testInterface

menu :: Interface -> String -> IO()
menu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
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
  putStrLn "-- 9 -- EXIT PROGRAM     --"
  putStrLn "-- 0 -- ADMIN ONLY       --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine
  if Interface.checkIfOnlyInt x then runMenu (read x :: Int) i
    else menu i " Not a valid input! Try again."

runMenu :: Int -> Interface -> IO()
runMenu c i
  | c == 1 = do
    system "clear"
    putStrLn "---------------------------"
    putStrLn ("-- " ++ (show (Interface.getUser i)))
    putStrLn "---------------------------"
    putStrLn "--   YOUR CART CONTAIN   --"
    putStrLn "---------------------------"
    mapM_ putStrLn (map show (Interface.getCart i))
    putStrLn "---------------------------"
    putStrLn "-- ADD THE ITEM TO CART  --"
    putStrLn "---------------------------"
    putStrLn "--     ITEM IN SHOP      --"
    putStrLn "---------------------------"
    mapM_ putStrLn $ map show (map fst(Interface.getDatabaseItem i))
    putStrLn "---------------------------"
    putStrLn "-- 0 -- NAVIGATE BACK    --"
    putStrLn "---------------------------"
    putStrLn "--  Write your ean code  --"
    putStrLn "---------------------------"
    x <- getLine
    if Interface.checkIfOnlyInt x then
      if ((read x ::Int )== 0) then menu i "Navigated back"
        else menu (addToCart (findItem (read x :: Int) i) i) ("You added " ++  (show (findItem (read x :: Int) i)) ++ " to the Cart")
      else runMenu 1 i

  | c == 2 = do
    system "clear"
    putStrLn "---------------------------"
    putStrLn ("-- " ++ (show (Interface.getUser i)))
    putStrLn "---------------------------"
    putStrLn "--   YOUR CART CONTAIN   --"
    putStrLn "---------------------------"
    mapM_ putStrLn (map show (Interface.getCart i))
    putStrLn "---------------------------"
    putStrLn "-- 0 -- NAVIGATE BACK    --"
    putStrLn "---------------------------"
    putStrLn "--  Write your ean code  --"
    putStrLn "---------------------------"
    x <- getLine
    if Interface.checkIfOnlyInt x then
      if ((read x ::Int )== 0) then menu i "Navigated back"
        else menu (removeFromCart (findItem (read x :: Int) i) i) ("You removed " ++  (show (findItem (read x :: Int) i)) ++ " from the Cart")
      else runMenu 2 i

  | c == 3 = do
    menu (buy i) ("You bought the following Cart " ++ (show (Interface.getCart i)))
  |c == 9 = do
    exit
  | c == 0 && (Interface.getUserAdmin i) = adminMenu i "Access Granted"
  | c == 0 = menu i "You dont have access to admin features!"
  | otherwise = menu i "You wrote a non existing number"

adminMenu :: Interface -> String -> IO()
adminMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
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
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine
  if Interface.checkIfOnlyInt x then runAdminMenu (read x :: Int) i
    else adminMenu i "Not a valid input! Try again."

runAdminMenu :: Int -> Interface -> IO()
runAdminMenu c i
  | c == 1 = adminUserMenu i ""
  | c == 2 = adminItemMenu i ""

  | c == 0 = menu i "You navigated back"
  | otherwise = adminMenu i "You wrote a non existing number"

adminUserMenu :: Interface -> String -> IO()
adminUserMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- CREATE USER      --"
  putStrLn "-- 2 -- REMOVE USER      --"
  putStrLn "-- 3 -- CHANGE USER      --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 0 -- NAVIGATE BACK    --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn "-- USERS IN OUR DATABASE --"
  putStrLn "---------------------------"
  mapM_ putStrLn $ map show (map fst (Interface.getDatabaseUser i))
  putStrLn "---------------------------"
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine
  if Interface.checkIfOnlyInt x then runAdminUserMenu (read x :: Int) i
    else adminUserMenu i "Not a valid input! Try again."

runAdminUserMenu :: Int -> Interface -> IO()
runAdminUserMenu c i
  | c == 1 = do
    putStrLn "Write name of the new user and hit ENTER"
    name <- getLine
    putStrLn ""
    putStrLn "Write id of the new user and hit ENTER"
    userId <- getLine
    if (not (Interface.checkIfOnlyInt userId)) then adminUserMenu i "Your UserID was not valid... Try again"
      else putStrLn ""
    putStrLn "Write wallet of the new user and hit ENTER"
    wallet <- getLine
    if (not (Interface.checkIfOnlyInt wallet)) then adminUserMenu i "Your wallet amount was not valid... Try again"
      else putStrLn ""
    putStrLn "Write adminstatus of the new user and hit ENTER"
    status <- getLine
    if (not (Interface.checkIfBool status)) then adminUserMenu i "You did not give a correct adminstatus (True or False)... Try again"
      else adminUserMenu (Interface.createUser name (read userId :: Int) (read wallet :: Int) (0) (read status :: Bool) i) "Created User"

  | c == 2 = do
    putStrLn "Write id of the user you want to remove"
    x <- getLine
    if Interface.checkIfOnlyInt x then adminUserMenu (Interface.removeUser (findUser (read x :: Int) i) i) "Removed a user"
      else adminUserMenu i "Not a valid input! Try again."

  | c == 3 = do
    putStrLn "Write the id of the user you want to change"
    x <- getLine
    if Interface.checkIfOnlyInt x then adminChangeUserMenu i "" (findUser (read x :: Int) i)
      else adminUserMenu i "Not a valid input! Try again."

  | c == 0 = adminMenu i "You navigated back"
  | otherwise = adminUserMenu i "You wrote a non existing number"

adminChangeUserMenu :: Interface -> String -> User -> IO()
adminChangeUserMenu i message u = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
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
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine
  if Interface.checkIfOnlyInt x then runAdminChangeUserMenu (read x :: Int) i u
    else adminChangeUserMenu i "Not a valid input! Try again." u

runAdminChangeUserMenu :: Int -> Interface -> User -> IO()
runAdminChangeUserMenu c i u
  | c == 1 = do
    putStrLn "Write the name you want this user to have and hit ENTER"
    name <- getLine
    let
      k = Interface.setUserName name u i
      in adminChangeUserMenu k "changed name" (Interface.getUser k)

  | c == 2 = do
    putStrLn "Write the id you want this user to have and hit ENTER"
    userId <- getLine
    if Interface.checkIfOnlyInt userId then
      let k = Interface.setUserId (read userId :: Int) u i
        in adminChangeUserMenu k "changed id" (Interface.getUser k)
      else adminChangeUserMenu i "Not a valid userId! Try again." u

  | c == 3 = do
    putStrLn "Write the amount you want to add for this users wallet and hit ENTER"
    amount <- getLine
    if Interface.checkIfOnlyInt amount then
      let
        k = Interface.fillWallet (read amount :: Int) u i
        in adminChangeUserMenu k "filled wallet" (Interface.getUser k)
      else adminChangeUserMenu i "Not a valid amount! Try again." u

  | c == 4 = do
    putStrLn "Write the amount you want to remove for this users wallet and hit ENTER"
    amount <- getLine
    if Interface.checkIfOnlyInt amount then
      let
        k = Interface.reduceWallet (read amount :: Int) u i
        in adminChangeUserMenu k "reduced wallet" (Interface.getUser k)
      else adminChangeUserMenu i "Not a valid input! Try again." u

  | c == 5 = do
    let
      k = Interface.clearWallet u i
      in adminChangeUserMenu k "cleared wallet" (Interface.getUser k)

  | c == 0 = adminUserMenu i "You navigated back"
  | otherwise = adminChangeUserMenu i "You wrote a non existing number" u

adminItemMenu :: Interface -> String -> IO()
adminItemMenu i message = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
  putStrLn "---------------------------"
  putStrLn "==========================="
  putStrLn "-- 1 -- CREATE ITEM      --"
  putStrLn "-- 2 -- REMOVE ITEM      --"
  putStrLn "-- 3 -- CHANGE ITEM      --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "--   --                  --"
  putStrLn "-- 0 -- NAVIGATE BACK    --"
  putStrLn "==========================="
  putStrLn "---------------------------"
  putStrLn "--     ITEM IN SHOP      --"
  putStrLn "---------------------------"
  mapM_ putStrLn $ map show (map fst(Interface.getDatabaseItem i))
  putStrLn "---------------------------"
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine

  if Interface.checkIfOnlyInt x then runAdminItemMenu (read x :: Int) i
    else adminItemMenu i "Not a valid input! Try again."

runAdminItemMenu :: Int -> Interface -> IO()
runAdminItemMenu c i
  | c == 1 = do
    putStrLn "Write name of the new item and hit ENTER"
    name <- getLine
    putStrLn ""
    putStrLn "Write ean of the new item and hit ENTER"
    ean <- getLine
    if (not (Interface.checkIfOnlyInt ean)) then adminItemMenu i "Not a valid ean! Try again."
      else putStrLn ""
    putStrLn "Write price of the new item and hit ENTER"
    price <- getLine
    if (not (Interface.checkIfOnlyInt price)) then adminItemMenu i "Not a valid price! Try again."
      else putStrLn ""
    putStrLn "Write stock of the new item and hit ENTER"
    stock <- getLine
    if (not (Interface.checkIfOnlyInt stock)) then adminItemMenu i "Not a valid stock amount! Try again."
      else adminItemMenu (Interface.createItem name (read ean :: Int) (read price :: Int) (read stock :: Int) i) "Created item"

  | c == 2 = do
    putStrLn "Write ean of the item you want to remove"
    x <- getLine
    if Interface.checkIfOnlyInt x then adminItemMenu (Interface.removeItem (findItem (read x :: Int) i) i) "Removed a item"
      else adminItemMenu i "Not a valid item ean! Try again."
  | c == 3 = do
    putStrLn "Write ean of the item you want to change"
    x <- getLine
    if Interface.checkIfOnlyInt x
      then
        let
          k = findItem (read x :: Int) i
          in adminChangeItemMenu i "" k
        else adminItemMenu i "Not a valid ean!"

  | c == 0 = adminMenu i "You navigated back"
  | otherwise = adminItemMenu i "You wrote a non existing number"

adminChangeItemMenu :: Interface -> String -> Item -> IO()
adminChangeItemMenu i message item = do
  system "clear"
  putStrLn "---------------------------"
  putStrLn (show (Interface.getUser i))
  putStrLn "---------------------------"
  putStrLn "--   YOUR CART CONTAIN   --"
  putStrLn "---------------------------"
  mapM_ putStrLn (map show (Interface.getCart i))
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
  putStrLn "--   CURRENTLY EDITING   --"
  putStrLn ("-- " ++ (show item))
  putStrLn "---------------------------"
  putStrLn ("-- " ++ message)
  putStrLn "---------------------------"
  putStrLn "-- Pick your alternative --"
  putStrLn "---------------------------"
  x <- getLine
  if Interface.checkIfOnlyInt x then runAdminChangeItemMenu (read x :: Int) i item
    else adminChangeItemMenu i "Not a valid input! Try again." item

runAdminChangeItemMenu :: Int -> Interface -> Item -> IO()
runAdminChangeItemMenu c i item
  | c == 1 = do
    putStrLn "Write the name you want this item to have and hit ENTER"
    name <- getLine
    let
      k = Interface.setItemName name item i
      in adminChangeItemMenu k "changed name" (Interface.findItem (Interface.getItemEan item) k)

  | c == 2 = do
    putStrLn "Write the ean you want this item to have and hit ENTER"
    ean <- getLine
    if Interface.checkIfOnlyInt ean then
      let k = Interface.setItemEan (read ean :: Int) item i
        in adminChangeItemMenu k "changed ean" (Interface.findItem (read ean :: Int) k)
      else adminChangeItemMenu i "Not a valid ean! Try again." item

  | c == 3 = do
    putStrLn "Write the price you want this item to have and hit ENTER"
    price <- getLine
    if Interface.checkIfOnlyInt price then
      let k = Interface.setItemPrice (read price :: Int) item i
        in adminChangeItemMenu k "changed price" (Interface.findItem (Interface.getItemEan item) k)
      else adminChangeItemMenu i "Not a valid price! Try again." item

  | c == 4 = do
    putStrLn "Write the amount you want this item to add to its stock and hit ENTER"
    amount <- getLine
    if Interface.checkIfOnlyInt amount then
      let k = Interface.addToStock (read amount :: Int) item i
        in adminChangeItemMenu k "changed stockvalue" (Interface.findItem (Interface.getItemEan item) k)
      else adminChangeItemMenu i "Not a valid amount! Try again." item

  | c == 5 = do
    putStrLn "Write the amount you want this item to remove from its stock and hit ENTER"
    amount <- getLine
    if Interface.checkIfOnlyInt amount then
      let k = Interface.removeFromStock (read amount :: Int) item i
        in adminChangeItemMenu k "changed stockvalue" (Interface.findItem (Interface.getItemEan item) k)
      else adminChangeItemMenu i "Not a valid amount! Try again." item

  | c == 6 = do
    putStrLn "Write the amount you want this item to have in stock and hit ENTER"
    amount <- getLine
    if Interface.checkIfOnlyInt amount then
      let k = Interface.replaceStock (read amount :: Int) item i
        in adminChangeItemMenu k "changed stockvalue" (Interface.findItem (Interface.getItemEan item) k)
      else adminChangeItemMenu i "Not a valid amount! Try again." item

  | c == 0 = adminItemMenu i "You navigated back"
  | otherwise = adminChangeItemMenu i "You wrote a non existing number" item
