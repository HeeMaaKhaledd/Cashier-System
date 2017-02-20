import Item
import User
import Cart

type Name = String
type Ean = Int
-----------------------------------------------
-- START OF ADMIN FUNCTIONS
-----------------------------------------------
-- must be admin.
-- must have loginpromt.
-----------------------------------------------
-- create User
createUser = undefined

-- remove User
removeUser = undefined

-- removes item x from the database
removeItem x = undefined

-- add item x to the database
addItem x = undefined

-- remove x from i stock value in the database
removeFromStock x i = undefined

-- add x to i's stock value in the database
addStock x i = undefined

-- replaces i's stock value in the database with x (only to be used if items is recounted)
newStock x = undefined

-----------------------------------------------
-- END OF ADMIN FUNCTIONS
-----------------------------------------------

-- add item to Cart

--findItem :: a -> Database -> Item



itemToCart :: Item -> Cart -> Cart
itemToCart i c = addToCart i c

-- remove item from Cart
delItemFromCart :: Item -> Cart -> Cart
delItemFromCart i c = removeFromCart i c

-- fetches all item in cart from database and updates stockvalues
buy x = undefined
