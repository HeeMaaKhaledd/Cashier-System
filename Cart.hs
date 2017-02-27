module Cart(Cart,Cart.empty,addToCart,removeFromCart,getFirst,calculatePrice,cartUpdate) where
import Item
import Test.HUnit

type Price = Int
type Cart = [Item]

{- empty
   PRE:  True
   POST: Returns the empty Cart
-}
empty :: Cart
empty = []

{- addToCart i c
   PRE:  True
   POST: returns a new cart that now also containing a new item
-}
addToCart :: Item -> Cart -> Cart
addToCart i c = [i] ++ c

{- removeFromCart i c
   PRE:  i exists in c
   POST: Returns the new cart but now without i in it.
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
   PRE:  Cart is not empty
   POST: Returns a tuple with the first item and the remaining Cart
-}
getFirst :: Cart -> (Item,Cart)
getFirst [] = error "no items in Cart"
getFirst (c:cs) = (c,cs)

{- calculatePrice
   PRE:  True
   POST: Returns sum of cart in integers, representing Price.
-}
calculatePrice :: Cart -> Price
calculatePrice c = sum (map Item.getPrice c)

{- cartUpdate i c
   PRE:  True
   POST: calls an updated cart of items that checks for doublets in the system.
-}
cartUpdate :: Item -> Cart -> Cart
cartUpdate i c = cartUpdateAUX i c Cart.empty

{- cartUpdateAUX i (c:cs) newC
   PRE:  True
   POST: calls a cart of items and if there are doublets of a product, it adds an item to an updated cart with reduced items stock.
   VARIANT: length of (c:cs)
-}
cartUpdateAUX :: Item -> Cart -> Cart -> Cart
cartUpdateAUX i [] newC     = newC
cartUpdateAUX i (c:cs) newC
  | Item.getEan i == Item.getEan c = cartUpdateAUX i cs (newC ++ [i])
  | otherwise = cartUpdateAUX i cs (newC ++ [c])

---------TestCases---------
runtests = runTestTT $ TestList [test1, test2, test3, test4]

test1 = TestCase $ assertEqual "empty test ((Cart))" ([]) (Cart.empty)

test2 = TestCase $ assertEqual "addToCart Item Sebbe 101 10 1 ([Item name ean price stock])" ([createItem "Sebbe" 101 10 1]) (addToCart (createItem "Sebbe" 101 10 1) Cart.empty)

test3 = TestCase $ assertEqual "removeFromCart Item sebbe 101 10 1 ([Item cola 193 10 10), (Item sebbe 101 10 1)])" [(createItem "cola" 193 10 10)] (removeFromCart (createItem "Sebbe" 101 10 1) ([(createItem "cola" 193 10 10), (createItem "Sebbe" 101 10 1)]))

test4 = TestCase $ assertEqual "getFirst ([Item sebbe 101 10 1, Item cola 193 10 10]))" ((createItem "Sebbe" 101 10 1), [(createItem "cola" 193 10 10)]) (getFirst [(createItem "Sebbe" 101 10 1), (createItem "cola" 193 10 10)])
