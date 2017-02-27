-- Creating module
module Interface(Interface,User,Item,Cart,Database,newInterface,getUser,createUser,
                  removeUser,findUser,setUserName,setUserId,makeUserAdmin,removeUserAdmin,
                  Interface.getWallet,Interface.fillWallet,Interface.reduceWallet,
                  Interface.clearWallet,Interface.createItem,removeItem,findItem,
                  Interface.addToStock,Interface.removeFromStock,Interface.replaceStock,
                  Interface.addToCart,Interface.removeFromCart,
                  buy,testInterface,getCart,getDatabaseItem, Interface.getUserAdmin,Interface.setItemEan,
                  Interface.setItemName,Interface.setItemPrice,Interface.getItemEan,
                  Interface.calculateCartPrice,Interface.getDatabaseUser,
                  Check.checkIfOnlyInt,Check.checkIfBool, Interface.getUserName) where

import Check
import Item
import User
import Cart
import Database
import Test.HUnit

-- END OF IMPORT

type Name     = String
type Ean      = Int
type Price    = Int
type Stock    = Int
type Id       = Int
type Wallet   = Int
type Spent    = Int
type IsAdmin  = Bool

{- REPRESENTATION CONVENTION: Interface holds 4 arguments, User, (Database User), (Database Item) and Cart
      User holds the current User, Database User holds every user in our database ,
      Database Item hold every item in our shop and Cart holds the current cart!

   REPRESENTATION INVARIANT: User must be a user inside Database User
 -}
data Interface = Interface User (Database User) (Database Item) Cart deriving (Show,Eq)

--testing interface ..
testInterface = newInterface a b c d
  where
    a = (User.newUser "sebbe" 12 100 0 True)
    b = [((User.newUser "sebbe" 12 100 0 True),12),((User.newUser "grimmi" 11 100 0 True),11)]
    c = [((Item.createItem "cola" 193 10 10),193),((Item.createItem "cool" 192 10 10),192)]
    d = Cart.addToCart (Item.createItem "cola" 193 10 10) Cart.empty

-- END OF DATASTRUCTURES

-- Functions to grab information from our Interface datastructure!
{- newInterface a b c d
   PRE:  True
   POST: returns the full interface.
-}
newInterface :: User -> Database User -> Database Item -> Cart -> Interface
newInterface a b c d = Interface a b c d

{- getUser (Interface u dU dI c)
   PRE:  True
   POST: finds a user based on the information from the interface data type.

-}
getUser :: Interface -> User
getUser (Interface u dU dI c) = u
{- getCart (Interface u dU dI c)
   PRE:  True
   POST: finds a cart based on the information put in by the user in the interface.

-}
getCart :: Interface -> Cart
getCart (Interface u dU dI c) = c
{- getDatabaseItem (Interface u dU dI c)
   PRE:  True
   POST: finds an item based on the information put in by the user in the interface.
-}
getDatabaseItem :: Interface -> Database Item
getDatabaseItem (Interface u dU dI c) = dI

{- getDatabaseUser (Interface u dU dI c)
   PRE:  True
   POST: finds a user based on the information put in by the user in the interface.
-}
getDatabaseUser :: Interface -> Database User
getDatabaseUser (Interface u dU dI c) = dU

-- user handling
{- createUser name iD wallet spent admin (Interface u dU dI c)
   PRE:  True
   POST: creates a new user based in name, identification number, wallet, money spent,
-}
createUser :: Name -> Id -> Wallet -> Spent -> IsAdmin -> Interface -> Interface
createUser name iD wallet spent admin (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.insert (User.newUser name iD wallet spent admin) iD dU

{- removeUser
   PRE:  True user (Interface u dU dI c)
   POST: removes a user based on the information put in by the user in the interface.
-}
removeUser :: User -> Interface -> Interface
removeUser user (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.delete user dU
{- findUser iD (Interface u dU dI c)
   PRE:  True
   POST: Finds a user based on the identification numbers put in by the user in the interface.
-}
findUser :: Id -> Interface -> User
findUser iD (Interface u dU dI c) = Database.grabWithId iD dU

-- uppdate user
{- setUserName name user (Interface u dU dI c)
   PRE:  True
   POST: sets name of a user based on the information put in by the user in the interface.
-}
setUserName :: Name -> User -> Interface -> Interface
setUserName name user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.setName name user
      k = User.getId user

{- getUserName a
   PRE:  True
   POST: gets name of a user based on the information put in by the user in the interface.
-}
getUserName :: User -> Name
getUserName a = User.getName a
{- setUserId iD user (Interface u dU dI c)
   PRE:  True
   POST: sets ID of a user based on the information put in by the user in the interface.
-}
setUserId :: Id -> User -> Interface -> Interface
setUserId iD user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j iD (Database.delete user dU)
      j = User.setId iD user
{- getUserAdmin (Interface u dU dI c)
   PRE:  True
   POST: gets information about a users properties based on the information put in by the user in the interface. Either returns true or false, depending if the user is admin
-}
getUserAdmin :: Interface -> Bool
getUserAdmin (Interface u dU dI c) = User.getAdminStatus u
{- makeUserAdmin user (Interface u dU dI c)
   PRE:  True
   POST: makes user properties admin if user information matches with information put in by the user.
-}
makeUserAdmin:: User -> Interface -> Interface
makeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.makeAdmin user
      k = User.getId user
{- removeUserAdmin user (Interface u dU dI c)
   PRE:  True
   POST: changes or reduces user properties from admin to customer if user information matches with information put in by the user.
-}
removeUserAdmin:: User -> Interface -> Interface
removeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeAdmin user
      k = User.getId user

-- wallet
{- getWallet user (Interface u dU dI c)
   PRE:  True
   POST: gets information about users wallet.
-}
getWallet :: User -> Interface -> Wallet
getWallet user (Interface u dU dI c) = User.getWallet user

{- fillWallet amount user (Interface u dU dI c)
   PRE:  True
   POST: Adds value of users disposable money in the system
-}
fillWallet :: Int -> User -> Interface -> Interface
fillWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.fillWallet amount user
      k = User.getId user

{- reduceWallet amount user (Interface u dU dI c)
   PRE:  True
   POST: reduces value of users disposable money in the system
-}
reduceWallet :: Int -> User -> Interface -> Interface
reduceWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeWallet amount user
      k = User.getId user

{- clearWallet user (Interface u dU dI c)
   PRE:  True
   POST: clears value to a 0 in wallet of users disposable money in the system
-}
clearWallet :: User -> Interface -> Interface
clearWallet user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.clearWallet user
      k = User.getId user

-- item handling
{- createItem name ean price stock (Interface u dU dI c)
   PRE:  True
   POST: creates an item in the database based on information put in by the user.
-}
createItem :: Name -> Ean -> Price -> Stock -> Interface -> Interface
createItem name ean price stock (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.insert (Item.createItem name ean price stock) ean dI
{- removeItem i (Interface u dU dI c)
   PRE:  True
   POST: removes an item in the database based on information put in by the user.
-}
removeItem :: Item -> Interface -> Interface
removeItem i (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.delete i dI
{- setItemName name item (Interface u dU dI c)
   PRE:  True
   POST: Sets an items name based on information put in by the user.
-}
setItemName :: Name -> Item -> Interface -> Interface
setItemName name item (Interface u dU dI c) = Interface u dU newdb c
    where
      newdb = Database.insert j k (Database.delete item dI)
      j = Item.setName name item
      k = Item.getEan item
{- getItemEan i
   PRE:  True
   POST: gets an items identification code the based on information put in by the user.
-}
getItemEan :: Item -> Ean
getItemEan i = Item.getEan i
{- setItemEan ean item (Interface u dU dI c)
   PRE:  True
   POST: sets an items identification code the based on information put in by the user.
-}
setItemEan :: Ean -> Item -> Interface -> Interface
setItemEan ean item (Interface u dU dI c) = Interface u dU newdb c
    where
      newdb = Database.insert j ean (Database.delete item dI)
      j = Item.setEan ean item
{- setItemPrice price item (Interface u dU dI c)
   PRE:  True
   POST: sets an items price the based on information put in by the user.
-}
setItemPrice :: Price -> Item -> Interface -> Interface
setItemPrice price item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.setPrice price item
    k = Item.getEan item
{- findItem ean (Interface u dU dI c)
   PRE:  True
   POST: gets an items information based on identification code put in by the user.
-}
findItem :: Ean -> Interface -> Item
findItem ean (Interface u dU dI c) = Database.grabWithId ean dI

-- stock handling
{- addToStock i item (Interface u dU dI c)
   PRE:  True
   POST: adds value to an items stock based on information put in by the user.
-}
addToStock :: Int -> Item -> Interface -> Interface
addToStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.addToStock i item
    k = Item.getEan item
{- removeFromStock i item (Interface u dU dI c)
   PRE:  True
   POST: reduces value from an items stock based on information put in by the user.
-}
removeFromStock :: Int -> Item -> Interface -> Interface
removeFromStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.removeFromStock i item
    k = Item.getEan item
{- replaceStock i item (Interface u dU dI c)
   PRE:  True
   POST: replaces value from an items stock based on information put in by the user.
-}
replaceStock :: Int -> Item -> Interface -> Interface
replaceStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.replaceStock i item
    k = Item.getEan item

-- Cart Handling
{- addToCart i (Interface u dU dI c)
   PRE:  True
   POST: returns an updated cart that now also containing a new item
-}
addToCart :: Item -> Interface -> Interface
addToCart item (Interface u dU dI c) = Interface u dU dI (Cart.addToCart item c)
{- removeFromCart i (Interface u dU dI c)
   PRE:  True
   POST: removes an item from the cart
-}
removeFromCart :: Item -> Interface -> Interface
removeFromCart item (Interface u dU dI c) = Interface u dU dI (Cart.removeFromCart item c)
{- calculateCartPrice (Interface u dU dI c)
   PRE:  True
   POST: deletes an item from the cart based on the information put in by the user.
-}
calculateCartPrice :: Interface -> Price
calculateCartPrice (Interface u dU dI c) = Cart.calculatePrice c

{- buy (Interface u dU dI c)
   PRE:  True
   POST: purchases the content of the cart based on the information put in by the user.
   VARIANT: Length of c
-}
buy :: Interface -> Interface
buy (Interface u dU dI c)
  | c == Cart.empty = Interface u dU dI c
  | otherwise = buy (Interface newu newdU newdI newC)
  where
    newdU = Database.insert newu (User.getId newu) (Database.delete u dU)
    newdI = Database.insert newi (Item.getEan newi) (Database.delete (fst nC) dI)
    newi  = Item.removeFromStock 1 (fst nC)
    newC  = (Cart.cartUpdate newi (snd nC))
    newu  = User.removeWallet price (User.addSpent price u)
    price = Item.getPrice (fst nC)
    nC    = Cart.getFirst c
