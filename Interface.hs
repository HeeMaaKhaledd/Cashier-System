import Item
import User
import Cart
import Database
import Test.HUnit

<<<<<<< HEAD
-- END OF IMPORT

type Name = String
type Ean  = Int
=======
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int
>>>>>>> 468c23d1a7220b05d131bec61fd01ce731ab0674
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
itemToCart i c = addToCart i c

-- remove item from Cart
removeFromCart :: Item -> Cart -> Cart
removeFromCart i c = Cart.removeFromCart i c

getSaldo :: User -> Wallet
getSaldo u = User.getWallet u

-- fetches all item in cart from database and updates stockvalues
buy :: User -> Cart -> Database User -> Database Item ->(Database User, Database Item)
buy u c dU dI = undefined
