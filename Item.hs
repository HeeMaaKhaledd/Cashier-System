module Item(Item,setName,setEan,setPrice,addToStock,removeFromStock
            ,empty,createItem,getPrice,getName,getStock,getEan,replaceStock) where
import Test.HUnit

-- Just to improve readability of the code.
type Name  = String
type Ean   = Int
type Price = Int
type Stock = Int

{- REPRESENTATION CONVENTION: Item holds 4 arguments Name, Ean, Price, Stock
      Name represents the items name, Ean the items Ean, Price the price of the item,
      Stock the amount of items left in stock

   REPRESENTATION INVARIANT: Price & Stock can't be a negative number!
 -}
data Item = Item Name Ean Price Stock deriving (Show, Eq)

{- createItem name ean price stock
   PRE:  True
   POST: takes all the arguments name,ean,price,stock and creates a item with this information.
   EXAMPLES: createItem "test" 1000101 35 100 = Item "Test" 1000101 35 100
-}
createItem :: Name -> Ean -> Price -> Stock -> Item
createItem a b c d = Item a b c d

{- setName x i
   PRE:  True
   POST: Changing the string(name) to variable x which contains a string
   EXAMPLES: setName "TestingTesting" (createItem "test" 1000101 35 100)
             = Item "TestingTesting" 1000101 35 100
-}
setName :: Name -> Item -> Item
setName x (Item name ean price stock) = Item x ean price stock

{- getName i
   PRE:  True
   POST: Returns the argument name
   EXAMPLES: getName (createItem "test" 1000101 35 100)
            = "test"
-}
getName :: Item -> Name
getName (Item name ean price stock) = name

{- setEan x i
   PRE:  True
   POST: Changes the Items ean code
   EXAMPLES: setEan 123456 (createItem "test" 1000101 35 100)
             = Item "test" 123456 35 100
-}
setEan :: Ean -> Item -> Item
setEan x (Item name ean price stock) = Item name x price stock

{- getEan i
   PRE: True
   POST: Returns the ean code from the item i
   EXAMPLES:getEan (createItem "test" 1000101 35 100)
            = 1000101
-}
getEan :: Item -> Ean
getEan (Item name ean price stock) = ean

{- setPrice x i
   PRE:  True
   POST: Sets the ean code to x
   EXAMPLES: setPrice 10 (createItem "test" 1000101 35 100)
            = Item "test" 1000101 10 100
-}
setPrice :: Price -> Item -> Item
setPrice x (Item name ean price stock) = Item name ean x stock

{-
   PRE: getPrice i
   POST: retrieves the price of an item i
   EXAMPLES:getPrice (createItem "test" 1000101 35 100)
-}
getPrice :: Item -> Price
getPrice (Item name ean price stock) = price

{- addToStock x i
   PRE:  True
   POST: adding x to the stock value
   SIDE EFFECTS: Crashes if precondition is violated and shows a error message.
   EXAMPLES:addToStock 1237 (createItem "test" 1000101 35 100)
          = Item "test" 1000101 35 1337)
-}
addToStock :: Stock -> Item -> Item
addToStock x (Item name ean price stock)
 | x > 0 = Item name ean price (stock + x)
 | otherwise = error "You can't add a negative number to the stock."

 {- removeFromStock x i
    PRE: x can't be larger than items stockvalue.
    POST: subtracting the stock value with x.
    SIDE EFFECTS: Crashes if precondition is violated and shows a error message.
    EXAMPLES:removeFromStock 88 (createItem "test" 1000101 35 100)
            = Item "test" 1000101 35 12
 -}
removeFromStock :: Stock -> Item -> Item
removeFromStock x (Item name ean price stock)
  | x > 0 && x < stock = Item name ean price (stock - x)
  | x > stock = error "You can't remove more items than we have in stock."
  | otherwise = error "You can't remove a negative number."

  {- replaceStock x i
     PRE:  True
     POST: Replacing the current stock value with i.
     EXAMPLES: replaceStock 1337 (createItem "test" 1000101 35 100)
              = Item "test" 1000101 35 1337
  -}
replaceStock :: Stock -> Item -> Item
replaceStock x (Item name ean price stock) = (Item name ean price x)

{- getStock i
   PRE:  True
   POST: Retrieves the stock value of i
   EXAMPLES:getStock (createItem "test" 1000101 35 100)
            = 100
-}
getStock :: Item -> Stock
getStock (Item name ean price stock) = stock

{- empty
   PRE:  True
   POST: Returns an Item empty string and 3 zeroes
   EXAMPLES: empty
            = Item "" 0 0 0
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
