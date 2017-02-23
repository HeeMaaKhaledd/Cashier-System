# Cashier-System
A Haskell based Cashier-System
## Description of Item
Item.hs is the way we chose to represent a product of in the Cashier-System.
Our Item.hs has its own datastructure which is defined in the following way.
```Haskell
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int

data Item = Item Name Ean Price Stock deriving (Show,Eq)
```
This means that every item has the following Arguments...  
Name, simply the name in the form of a string.  
Ean, the barcode that is on the item itself.  
Price, the price we want to take for the item.  
Stock, the amount of items we have in storage.  

The following functions is reachable if Item.hs is imported.  

```Haskell
setName         :: Name   -> Item -> Item
getName         :: Item   -> Name
setEan          :: Ean    -> Item -> Item
getEan          :: Item   -> Ean
setPrice        :: Price  -> Item -> Item
getPrice        :: Item   -> Price
addToStock      :: Stock  -> Item -> Item
removeFromStock :: Stock  -> Item -> Item
getStock        :: Item   -> Stock
```
So in the structure of the function specifications for the functions above. You can see a bunch of types with an arrow pointing right. The last arrow indicates what the function returns and the other arrows is just separating arguments the function takes while being called!  

A Example would be setName "Coca-Cola" (Item "cola" 1234 10 0) -> (Item "Cola-Cola" 1234 10 0)  
Here you can see that setName is being called with two arguments, first a Name and secondly a Item and with these two arguments it returns a new Item.

## Description of User
User.hs is the way we chose to represent a User of in the Cashier-System.
Our User.hs has its own datastructure which is defined in the following way.
```Haskell
type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = User Name Id Wallet Spent IsAdmin deriving (Show, Eq)
```
This means that every User has the following Arguments...  
Name, simply the name in the form of a string.  
Id, a specific id to identify a user.
Wallet, the amount of money a user have saved.  
Spent, the amount of money a user have spent in our shop.  
IsAdmin, keeps tracks the user itself have admin properties or not.

The following functions is reachable if User.hs is imported

```Haskell
newUser       :: Name   -> Id   -> Wallet -> Spent -> IsAdmin -> User
setName       :: Name   -> User -> User
getName       :: User   -> Name
setId         :: Id     -> User -> User
getId         :: User   -> Id
fillWallet    :: Wallet -> User -> User
removeWallet  :: Wallet -> User -> User
removeSpent   :: User   -> User
makeAdmin     :: User   -> User
removeAdmin   :: User   -> User
addSpent      :: Spent  -> User -> User
getWallet     :: User   -> Wallet
clearWallet   :: User   -> User
```

So in the structure of the function specifications for the functions above. You can see a bunch of types with an arrow pointing right. The last arrow indicates what the function returns and the other arrows is just separating arguments the function takes while being called!  

If you want examples, read the example for Item.hs... Same principle!

## Description of Database
Database.hs is the way we choose to represent a Database in our Cashier-System.
Our Database.hs has its own datatypes which is defined the following way!
```Haskell
type Id = Int -- Ean for item, userId for users...

type Database a = [(a,Id)]
```
As you can see our datatype for Database is polymorphic and therefore can hold both users or items as we wish!
So this means that the Database takes one polymorphic argument and returns a tuple containing this argument together with a ID.

The following functions is reachable if Database.hs is imported
```Haskell
empty :: Database a
deleteWithID :: Id -> Database a -> Database a
insert :: a -> Id -> Database a -> Database a
grabWithID :: Id -> Database a -> a
```
So in the structure of the function specifications for the functions above. You can see a bunch of types with an arrow pointing right. The last arrow indicates what the function returns and the other arrows is just separating arguments the function takes while being called!  

If you want examples, read the example for Item.hs... Same principle!

## Description of Cart

Cart.hs is our way of representing a shopping cart! This is our way of keeping track on the product our customer picks.  
Cart has only one datatype which is declared in the following way:
```Haskell  

type Cart = [Item]  

```
The datatype Cart is simply the linked list of items. This is to create a simple structure where we can easily loop through and calculate our prices.
This may not be the best in time Complexity however, we don't expect someone to add infinite amount of items in their cart.  

If you Cart.hs is imported you make the following functions reachable.  

```Haskell

empty :: Cart
addToCart :: Item -> Cart -> Cart
removeFromCart :: Item -> Cart -> Cart
removeFromCartAUX :: Item -> Cart -> Cart -> Cart
calculatePrice :: Cart -> Int
getFirst :: Cart -> (Item,Cart)

```
So in the structure of the function specifications for the functions above. You can see a bunch of types with an arrow pointing right. The last arrow indicates what the function returns and the other arrows is just separating arguments the function takes while being called!  

If you want examples, read the example for Item.hs... Same principle!

## Description of Interface
