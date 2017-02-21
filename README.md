# Cashier-System
A Haskell based Cashier-System
## Description of Item
Item is the way we chose to represent a product of in the Cashier-System.
Our Item.hs has its own datastructure which is defined in the following way.
```Haskell
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int

data Item = Item Name Ean Price Stock
```
This means that every item has the following Arguments...  
Name, simply the name in the form of a string.  
Ean, the barcode that is on the item itself.  
Price, the price we want to take for the item.  
Stock, the amount of items we have in storage.  

The following functions is reachable if Item.hs is imported.  
```Haskell
setName         :: Name -> Item -> Item
getName         :: Item -> Name
setEan          :: Ean -> Item -> Item
getEan          :: Item -> Ean
setPrice        :: Price -> Item -> Item
getPrice        :: Item -> Price
addToStock      :: Stock -> Item -> Item
removeFromStock :: Stock -> Item -> Item
getStock        :: Item -> Stock
```

## Description of User
## Description of Interface
## Description of Database
