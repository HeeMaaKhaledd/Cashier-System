module Cart(Cart,Cart.empty,addToCart,removeFromCart,calculatePrice,getFirst) where
import Item
import Database

type Cart = [Item]

empty :: Cart
empty = []

addToCart :: Item -> Cart -> Cart
addToCart i c = [i] ++ c

removeFromCart :: Item -> Cart -> Cart
removeFromCart i [] = error "non existing product"
removeFromCart i c = removeFromCartAUX i c Cart.empty

removeFromCartAUX :: Item -> Cart -> Cart -> Cart
removeFromCartAUX i (c:cs) newC
  | i == c = newC ++ cs
  | i /= c = removeFromCartAUX i cs (c:newC)

calculatePrice :: Cart -> Int
calculatePrice [] = 0
calculatePrice (c:cs) = getPrice c + calculatePrice cs

getFirst (c:cs) = (c,cs)
