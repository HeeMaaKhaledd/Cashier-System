-- Creating module
module Interface(Interface,User,Item,Cart,Database,newInterface,getUser,createUser,
                  removeUser,findUser,setUserName,setUserId,makeUserAdmin,removeUserAdmin,
                  Interface.getWallet,Interface.fillWallet,Interface.reduceWallet,
                  Interface.clearWallet,Interface.createItem,removeItem,findItem,
                  Interface.addToStock,Interface.removeFromStock,Interface.replaceStock,
                  Interface.addToCart,Interface.removeFromCart,
                  buy,bajs,getCart,getDatabaseItem, Interface.getUserAdmin,Interface.setItemEan,
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

data Interface = Interface User (Database User) (Database Item) Cart deriving (Show,Eq)

--testing interface ..
bajs = newInterface a b c d
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

{- getUser
   PRE:  True
   POST: finds a user based on the information from the interface data type.
   INVARIANT: returns an error if the user is not in the database.
-}
getUser :: Interface -> User
getUser (Interface u dU dI c) = u
{- getCart
   PRE:  True
   POST: finds a cart based on the information put in by the user in the interface.
   INVARIANT: returns an error if information doesn't exist or isn't in the database.
-}
getCart :: Interface -> Cart
getCart (Interface u dU dI c) = c
{- getDatabaseItem
   PRE:  True
   POST: finds an item based on the information put in by the user in the interface.
   INVARIANT: returns an error if information of item doesn't exist or isn't in the database.
-}
getDatabaseItem :: Interface -> Database Item
getDatabaseItem (Interface u dU dI c) = dI

{- getDatabaseUser
   PRE:  True
   POST: finds a user based on the information put in by the user in the interface.
   INVARIANT: returns an error if information of user doesn't exist or isn't in the database.
-}
getDatabaseUser :: Interface -> Database User
getDatabaseUser (Interface u dU dI c) = dU

-- user handling
{- getDatabaseUser
   PRE:  True
   POST: creates a new user based in name, identification number, wallet, money spent,  
   INVARIANT: returns an error if information of user doesn't exist or isn't in the database.
-}
createUser :: Name -> Id -> Wallet -> Spent -> IsAdmin -> Interface -> Interface
createUser name iD wallet spent admin (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.insert (User.newUser name iD wallet spent admin) iD dU

removeUser :: User -> Interface -> Interface
removeUser user (Interface u dU dI c) = Interface u newdb dI c
  where newdb = Database.delete user dU

findUser :: Id -> Interface -> User
findUser iD (Interface u dU dI c) = Database.grabWithId iD dU

-- uppdate user

setUserName :: Name -> User -> Interface -> Interface
setUserName name user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.setName name user
      k = User.getId user

getUserName :: User -> Name
getUserName a = User.getName a

setUserId :: Id -> User -> Interface -> Interface
setUserId iD user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j iD (Database.delete user dU)
      j = User.setId iD user

getUserAdmin :: Interface -> Bool
getUserAdmin (Interface u dU dI c) = User.getAdminStatus u

makeUserAdmin:: User -> Interface -> Interface
makeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.makeAdmin user
      k = User.getId user

removeUserAdmin:: User -> Interface -> Interface
removeUserAdmin user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeAdmin user
      k = User.getId user

-- wallet
getWallet :: User -> Interface -> Wallet
getWallet user (Interface u dU dI c) = User.getWallet user

fillWallet :: Int -> User -> Interface -> Interface
fillWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.fillWallet amount user
      k = User.getId user

reduceWallet :: Int -> User -> Interface -> Interface
reduceWallet amount user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.removeWallet amount user
      k = User.getId user

clearWallet :: User -> Interface -> Interface
clearWallet user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.clearWallet user
      k = User.getId user

-- item handling

createItem :: Name -> Ean -> Price -> Stock -> Interface -> Interface
createItem name ean price stock (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.insert (Item.createItem name ean price stock) ean dI

removeItem :: Item -> Interface -> Interface
removeItem i (Interface u dU dI c) = Interface u dU newdb c
  where newdb = Database.delete i dI

setItemName :: Name -> Item -> Interface -> Interface
setItemName name item (Interface u dU dI c) = Interface u dU newdb c
    where
      newdb = Database.insert j k (Database.delete item dI)
      j = Item.setName name item
      k = Item.getEan item

getItemEan :: Item -> Ean
getItemEan i = Item.getEan i

setItemEan :: Ean -> Item -> Interface -> Interface
setItemEan ean item (Interface u dU dI c) = Interface u dU newdb c
    where
      newdb = Database.insert j ean (Database.delete item dI)
      j = Item.setEan ean item

setItemPrice :: Price -> Item -> Interface -> Interface
setItemPrice price item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.setPrice price item
    k = Item.getEan item

findItem :: Ean -> Interface -> Item
findItem ean (Interface u dU dI c) = Database.grabWithId ean dI

-- stock handling
addToStock :: Int -> Item -> Interface -> Interface
addToStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.addToStock i item
    k = Item.getEan item

removeFromStock :: Int -> Item -> Interface -> Interface
removeFromStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.removeFromStock i item
    k = Item.getEan item

replaceStock :: Int -> Item -> Interface -> Interface
replaceStock i item (Interface u dU dI c) = Interface u dU newdb c
  where
    newdb = Database.insert j k (Database.delete item dI)
    j = Item.replaceStock i item
    k = Item.getEan item

-- Cart Handling

addToCart :: Item -> Interface -> Interface
addToCart item (Interface u dU dI c) = Interface u dU dI (Cart.addToCart item c)

removeFromCart :: Item -> Interface -> Interface
removeFromCart item (Interface u dU dI c) = Interface u dU dI (Cart.removeFromCart item c)

calculateCartPrice :: Interface -> Price
calculateCartPrice (Interface u dU dI c) = Cart.calculatePrice c

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
