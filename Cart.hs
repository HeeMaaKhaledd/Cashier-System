module Cart(Cart,Cart.empty,addToCart,removeFromCart,getFirst) where
import Item
import Database
import Test.HUnit

type Cart = [Item]

{- empty
   PRE:  True
   POST: Returns the empty Cart
-}
empty :: Cart
empty = []

{- addToCart i c
   PRE:  True
   POST: returns a new cart that now also containing the item i
-}
addToCart :: Item -> Cart -> Cart
addToCart i c = [i] ++ c

{- removeFromCart i c
   PRE:  True
   POST: Returns the new cart but now without i in it.
   SIDE EFFECTS: error if item is not in the list. ?? doublecheck
-}
removeFromCart :: Item -> Cart -> Cart
removeFromCart i [] = error "non existing product"
removeFromCart i c = removeFromCartAUX i c Cart.empty

{- removeFromCartAUX i c u
   PRE:  True
   POST: Returns a new cart without i in it.
-}
removeFromCartAUX :: Item -> Cart -> Cart -> Cart
removeFromCartAUX i (c:cs) newC
  | i == c = newC ++ cs
  | i /= c = removeFromCartAUX i cs (c:newC)

{- getFirst
   PRE:  True
   POST: Returns a tuple with the first item and the remaining Cart
   SIDE EFFECTS: returns a error if cart is empty -- check how to write Side Effects
-}
getFirst :: Cart -> (Item,Cart)
getFirst [] = error "no items in Cart"
getFirst (c:cs) = (c,cs)

---------TestCases---------
runtests = runTestTT $ TestList [test1, test2, test3, test4] -- , , test5, test6, test6a,test7]

test1 = TestCase $ assertEqual "empty test ((Cart))" ([]) (Cart.empty)

test2 = TestCase $ assertEqual "addToCart Item Sebbe 101 10 1 ([Item name ean price stock])" ([createItem "Sebbe" 101 10 1]) (addToCart (createItem "Sebbe" 101 10 1) Cart.empty)

test3 = TestCase $ assertEqual "removeFromCart Item sebbe 101 10 1 ([Item cola 193 10 10), (Item sebbe 101 10 1)])" [(createItem "cola" 193 10 10)] (removeFromCart (createItem "Sebbe" 101 10 1) ([(createItem "cola" 193 10 10), (createItem "Sebbe" 101 10 1)]))

test4 = TestCase $ assertEqual "getFirst ([Item sebbe 101 10 1, Item cola 193 10 10]))" ((createItem "Sebbe" 101 10 1), [(createItem "cola" 193 10 10)]) (getFirst [(createItem "Sebbe" 101 10 1), (createItem "cola" 193 10 10)])
--
-- test5 = TestCase $ assertEqual "getPrice ((Item name ean price stock))" (0) (getPrice empty)
--
-- test6 = TestCase $ assertEqual "addToStock ((Item name ean price stock))" (createItem "" 0 0 1337) (addToStock 1337 empty )
--
-- test6a = TestCase $ assertEqual "removeFromStock ((Item name ean price stock))" (createItem "" 0 0 10) (removeFromStock 10 (createItem "" 0 0 20))
--
-- test7 = TestCase $ assertEqual "getStock ((Item name ean price stock))" (20) (getStock (createItem "" 0 0 20) )
