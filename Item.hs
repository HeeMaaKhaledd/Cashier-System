module Item(Item,setName,setEan,setPrice,addToStock,removeFromStock,empty) where

-- Just to improve readability of the code.
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int

data Item = Item Name Ean Price Stock deriving Show

-- set the product name
setName :: Name -> Item -> Item
setName x (Item name ean price stock) = Item x ean price stock

-- set the product ean-code
setEan :: Ean -> Item -> Item
setEan x (Item name ean price stock) = Item name x price stock

--set the product price
setPrice :: Price -> Item -> Item
setPrice x (Item name ean price stock) = Item name ean x stock

--add more items to stock.
addToStock :: Stock -> Item -> Item
addToStock x (Item name ean price stock)
 | x > 0 = Item name ean price (stock + x)
 | otherwise = error "You can't add a negative number to the stock."

removeFromStock :: Stock -> Item -> Item
removeFromStock x (Item name ean price stock)
  | x > 0 && x < stock = Item name ean price (stock - x)
  | x > stock = error "You can't remove more items than we have in stock."
  | otherwise = error "You can't remove a negative number."

empty = Item "" 0 0 0