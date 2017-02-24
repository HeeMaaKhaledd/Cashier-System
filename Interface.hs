-- Creating module
module Interface(Interface,User,Item,Cart,Database,newInterface,getUser,createUser,
                  removeUser,findUser,setUserName,setUserId,makeUserAdmin,removeUserAdmin,
                  Interface.getWallet,Interface.fillWallet,Interface.reduceWallet,Interface.clearWallet,Interface.createItem,removeItem,findItem,
                  Interface.addToStock,Interface.removeFromStock,Interface.replaceStock,Interface.addToCart,Interface.removeFromCart,buy) where

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
newInterface :: User -> Database User -> Database Item -> Cart -> Interface
newInterface a b c d = Interface a b c d

getUser :: Interface -> User
getUser (Interface u dU dI c) = u

-- user handling

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

setUserId :: Id -> User -> Interface -> Interface
setUserId iD user (Interface u dU dI c)
  | user == u = Interface j newdb dI c
  | otherwise = Interface u newdb dI c
    where
      newdb = Database.insert j k (Database.delete user dU)
      j = User.setId iD user
      k = User.getId user

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

clearWallet :: Int -> User -> Interface -> Interface
clearWallet amount user (Interface u dU dI c)
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

buy :: Interface -> Interface
buy (Interface u dU dI c)
  | c == Cart.empty = Interface u dU dI c
  | otherwise = buy (Interface newu newdU newdI newC)
  where
    newdU = Database.insert newu (User.getId newu) (Database.delete u dU)
    newdI = Database.insert newi (Item.getEan newi) (Database.delete (fst nC) dI)
    newi  = Item.removeFromStock 1 (fst nC)
    newC  = (snd nC)
    newu  = User.removeWallet price (User.addSpent price u)
    price = Item.getPrice (fst nC)
    nC    = Cart.getFirst c
