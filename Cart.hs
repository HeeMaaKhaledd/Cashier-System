module Cart(Cart,Cart.empty,addToCart,removeFromCart,calculatePrice) where
import Item

type Cart = [Item]

empty :: Cart
empty = []

addToCart :: Item -> Cart -> Cart
addToCart x c = x:c

removeFromCart :: Item -> Cart -> Cart
removeFromCart x c = undefined

calculatePrice :: Cart -> Int
calculatePrice (c:cs) = undefined
