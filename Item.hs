module Item(Item,setName,setEan,setPrice,addToStock,removeFromStock
            ,empty,createItem,getPrice,getName,getStock,getEan,replaceStock) where
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
{- setName x
   PRE:  True
   POST: Changing the string(name) to variable x which contains a string
   EXAMPLES: setName "TestingTesting" (createItem "test" 1000101 35 100)
             = Item "TestingTesting" 1000101 35 100
-}
setName :: Name -> Item -> Item
setName x (Item name ean price stock) = Item x ean price stock

{- getName
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getName :: Item -> Name
getName (Item name ean price stock) = name

{- setEan x
   PRE:  True
   POST: Changes the Items ean code
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
-- set the product ean-code
setEan :: Ean -> Item -> Item
setEan x (Item name ean price stock) = Item name x price stock

{- getEan
   PRE: True
   POST: Returns the ean code from the item
   EXAMPLES:
-}
getEan :: Item -> Ean
getEan (Item name ean price stock) = ean

{- setPrice x
   PRE:  Price is a postive int
   POST: Sets the ean code to x, provided x is an integer
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
--set the product price
setPrice :: Price -> Item -> Item
setPrice x (Item name ean price stock) = Item name ean x stock

{-
   PRE: getPrice
   POST: retrieves the price of an item
   EXAMPLES:
-}
getPrice :: Item -> Price
getPrice (Item name ean price stock) = price

{- addToStock x
   PRE:  True
   POST: adding an int to the stock value
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
--add more items to stock.
addToStock :: Stock -> Item -> Item
addToStock x (Item name ean price stock)
 | x > 0 = Item name ean price (stock + x)
 | otherwise = error "You can't add a negative number to the stock."

 {- removeFromStock x
    PRE: True
    POST: subtracting the stack value with an int.
    EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
 -}
removeFromStock :: Stock -> Item -> Item
removeFromStock x (Item name ean price stock)
  | x > 0 && x < stock = Item name ean price (stock - x)
  | x > stock = error "You can't remove more items than we have in stock."
  | otherwise = error "You can't remove a negative number."

  {- replaceStock x
     PRE:  Poitive integer
     POST: Replacing the current stock value with another integer.
     EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
  -}
replaceStock :: Stock -> Item -> Item
replaceStock x (Item name ean price stock) = (Item name ean price x)

{- getStock
   PRE:  True
   POST: Retrieves the stock value
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
getStock :: Item -> Stock
getStock (Item name ean price stock) = stock

{- empty
   PRE:  True
   POST: Returns an Item empty string and 3 integers
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
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
