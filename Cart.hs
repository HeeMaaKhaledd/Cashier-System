module Cart(Cart,Cart.empty,addToCart,removeFromCart,calculatePrice,getFirst) where
import Item
import Database

type Cart = [Item]

{- empty
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
empty :: Cart
empty = []

{- addToCart
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
addToCart :: Item -> Cart -> Cart
addToCart i c = [i] ++ c

{- removeFromCart
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
removeFromCart :: Item -> Cart -> Cart
removeFromCart i [] = error "non existing product"
removeFromCart i c = removeFromCartAUX i c Cart.empty

{- removeFromCartAUX
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
removeFromCartAUX :: Item -> Cart -> Cart -> Cart
removeFromCartAUX i (c:cs) newC
  | i == c = newC ++ cs
  | i /= c = removeFromCartAUX i cs (c:newC)

  {- calculatePrice
     PRE:  True
     POST: Returns the argument name
     SIDE EFFECTS: ... if any, including exceptions ...
  -}
calculatePrice :: Cart -> Int
calculatePrice [] = 0
calculatePrice (c:cs) = getPrice c + calculatePrice cs

{- getFirst
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getFirst :: Cart -> (Item,Cart)
getFirst (c:cs) = (c,cs)
