module Item(Item,setName,setEan,setPrice,addToStock,removeFromStock
            ,empty,createItem,getPrice,getName,getStock,getEan) where
import Test.HUnit

-- Just to improve readability of the code.
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int

data Item = Item Name Ean Price Stock deriving (Show, Eq)

-- only to help create item fast.
createItem a b c d = Item a b c d

-- set the product name
setName :: Name -> Item -> Item
setName x (Item name ean price stock) = Item x ean price stock

getName :: Item -> Name
getName (Item name ean price stock) = name

-- set the product ean-code
setEan :: Ean -> Item -> Item
setEan x (Item name ean price stock) = Item name x price stock

getEan :: Item -> Ean
getEan (Item name ean price stock) = ean

--set the product price
setPrice :: Price -> Item -> Item
setPrice x (Item name ean price stock) = Item name ean x stock

getPrice :: Item -> Price
getPrice (Item name ean price stock) = price

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

getStock :: Item -> Stock
getStock (Item name ean price stock) = stock

empty :: Item
empty = Item "" 0 0 0

---------TestCases---------
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test6a,test7]

test1 = TestCase $ assertEqual "setName test ((Item name ean price stock))" (createItem "test" 0 0 0) (setName "test" empty)

test2 = TestCase $ assertEqual "getName (Item name ean price stock)" ("") (getName empty)

test3 = TestCase $ assertEqual "setEan 1001001 ((Item name ean price stock))" (createItem "" 1001001 0 0) (setEan 1001001 empty)

test4 = TestCase $ assertEqual "setPrice 1337 ((Item name ean price stock))" (createItem "" 0 1337 0) (setPrice 1337 empty)

test5 = TestCase $ assertEqual "getPrice ((Item name ean price stock))" (0) (getPrice empty)

test6 = TestCase $ assertEqual "addToStock ((Item name ean price stock))" (createItem "" 0 0 1337) (addToStock 1337 empty )

test6a = TestCase $ assertEqual "removeFromStock ((Item name ean price stock))" (createItem "" 0 0 10) (removeFromStock 10 (createItem "" 0 0 20))

test7 = TestCase $ assertEqual "getStock ((Item name ean price stock))" (20) (getStock (createItem "" 0 0 20) )
