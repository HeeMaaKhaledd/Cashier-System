module Cart(Cart,Cart.empty,addToCart,removeFromCart,calculatePrice,getFirst) where
import Item
import Database

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
