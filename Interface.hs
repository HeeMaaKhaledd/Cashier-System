import Item
import User
import Cart
import Database
import Test.HUnit

type Name = String
type Ean  = Int
type Id   = Int
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
createUser u dB = (Database.insert u (User.getId u)) ++ db

-- ANSVARIG: GRIM
-- removeUser
-- gets a User and a database, removes the user from the database and returns the new database.
removeUser = undefined

findUser :: Id -> Database User -> User
findUser = undefined

--ANSVARIG: GRIM
-- removeItem
-- gets a Item and a Database, removes item from the Database and returns the new Database.
removeItem = undefined

--ANSVARIG: GRIM
-- addItem
-- gets a Item and a Database, adds the item to the Database and returns the new Database.
addItem = undefined

--ANSVARIG: JESPER
-- removeFromStock
-- gets an amount and a item, and removes x from i's stockvalue
removeFromStock x i = undefined

--ANSVARIG: JESPER
-- addStock
-- gets an amount and a item and adds x to i's stockvalue
addStock x i = undefined

--ANSVARIG: JESPER
--newStock
-- replaces i's stock value in the database with x (only to be used if items is recounted)
newStock x = undefined

-----------------------------------------------
-- END OF ADMIN FUNCTIONS
-----------------------------------------------
findItem :: Ean -> Database Item -> Item
findItem = undefined



itemToCart :: Item -> Cart -> Cart
itemToCart i c = addToCart i c

-- remove item from Cart
delItemFromCart :: Item -> Cart -> Cart
delItemFromCart i c = removeFromCart i c

getSaldo = undefined

-- fetches all item in cart from database and updates stockvalues
buy x = undefined
