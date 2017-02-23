import Item
import User
import Cart
import Database
import Test.HUnit

-- END OF IMPORT

type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int
type Id   = Int
type Wallet = Int

data Interface = Interface User (Database User) (Database Item) Cart deriving (Show,Eq)

-- END OF DATASTRUCTURES

-- Functions to grab information from our Interface datastructure!
newInterface a b c d = Interface a b c d
getUser (Interface u dU dI c) = u
getDu (Interface u dU dI c)   = dU
getDi (Interface u dU dI c)   = dI
getCart (Interface u dU dI c) = c

-----------------------------------------------
-- START OF ADMIN FUNCTIONS
-----------------------------------------------
-- must be admin.
-- must have loginpromt.
-----------------------------------------------

-- user handling

createUser2 name iD wallet spent admin (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.insert (User.newUser name iD wallet spent admin) iD dU

removeUser2 user (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.delete user dU

findUser2 iD (Interface u dU dI c) = Database.grabWithId iD dU

-- uppdate user
setUserName name user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.setName name user
      k = User.getId user

setUserId iD user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.setId iD user
      k = User.getId user

makeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.makeAdmin user
      k = User.getId user

removeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeAdmin user
      k = User.getId user

-- wallet
getWallet user (Interface u dU dI c) = User.getWallet user

fillWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.fillWallet amount user
      k = User.getId user


reduceWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeWallet amount user
      k = User.getId user

clearWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.clearWallet user
      k = User.getId user

-- item handling

createItem name ean price stock (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.insert (Item.createItem name ean price stock) ean dI

removeItem2 i (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.delete i dI

findItem2 ean (Interface u dU dI c) = Database.grabWithId ean dI

-- stock handling

addToStock2 i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.addToStock i item
    k = Item.getEan item

removeFromStock2 i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.removeFromStock i item
    k = Item.getEan item

replaceStock2 i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.replaceStock i item
    k = Item.getEan item

-- Cart Handling

addToCart item (Interface u dU dI c) = Interface u dU dI (Cart.addToCart item c)

removeFromCart2 item (Interface u dU dI c) = Interface u dU dI (Cart.removeFromCart item c)

buy2 (Interface u dU dI c)
  | c == Cart.empty = Interface u dU dI c
  | otherwise = buy2 (Interface newu newdU newdI newC)
  where
    newdU = Database.insert newu (User.getId newu) (Database.delete u dU)
    newdI = Database.insert newi (Item.getEan newi) (Database.delete (fst nC) dI)
    newi  = Item.removeFromStock 1 (fst nC)
    newC  = (snd nC)
    newu  = User.removeWallet price (User.addSpent price u)
    price = Item.getPrice (fst nC)
    nC    = Cart.getFirst c

-- ANSVARIG: GRIM
-- createUser
-- gets a user and a database, adds this new user to the database and returns the new database.
createUser :: User -> Database User -> Database User
createUser u dB = Database.insert u (User.getId u) dB

-- ANSVARIG: GRIM
-- removeUser
-- gets a User and a database, removes the user from the database and returns the new database.
removeUser :: User -> Database User -> Database User
removeUser u dB = (Database.delete u) dB

findUser :: Id -> Database User -> User
findUser i dB = Database.grabWithId i dB


--ANSVARIG: GRIM
-- removeItem
-- gets a Item and a Database, removes item from the Database and returns the new Database.
removeItem :: Id -> Database Item -> Database Item
removeItem i dB = Database.deleteWithID i dB

--ANSVARIG: GRIM
-- addItem
-- gets a Item and a Database, adds the item to the Database and returns the new Database.
addItem :: Name -> Ean -> Price -> Stock -> Database Item -> Database Item
addItem a b c d dB = Database.insert (Item.createItem a b c d) b dB

--ANSVARIG: JESPER
-- removeFromStock
-- gets an amount and a item, and removes x from i's stockvalue
removeFromStock :: Int -> Item -> Item
removeFromStock x i = Item.removeFromStock x i

--ANSVARIG: JESPER
-- addStock
-- gets an amount and a item and adds x to i's stockvalue
addToStock :: Int -> Item -> Item
addToStock x i = Item.addToStock x i

--ANSVARIG: JESPER
--replaceStock
-- replaces i's stock value in the database with x (only to be used if items is recounted)
replaceStock :: Int -> Item -> Item
replaceStock x = Item.replaceStock x

-----------------------------------------------
-- END OF ADMIN FUNCTIONS
-----------------------------------------------
findItem :: Ean -> Database Item -> Item
findItem a b = Database.grabWithId a b

itemToCart :: Item -> Cart -> Cart
itemToCart i c = Cart.addToCart i c

-- remove item from Cart
removeFromCart :: Item -> Cart -> Cart
removeFromCart i c = Cart.removeFromCart i c

getSaldo :: User -> Wallet
getSaldo u = User.getWallet u

-- fetches all item in cart from database and updates stockvalues
buy :: User -> Cart -> Database User -> Database Item ->(Database User, Database Item)
buy u c dU dI = undefined
