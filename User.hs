-- improving readability of code.
import Cart

type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = User Name Id Wallet Spent IsAdmin Cart deriving Show

-- a set of functions that is suitable for a user to have.
-- Set the name of the user.
setName :: Name -> User -> User
setName x (User c i wallet spent a k) = User x i wallet spent a k

-- Set the id of the user
setId :: Id -> User -> User
setId x (User c i wallet spent a k) = User c x wallet spent a k

-- add to wallet
fillWallet :: Wallet -> User -> User
fillWallet x (User c i wallet spent a k)
  | x > 0 = User c i (wallet + x) spent a k
  | otherwise = error "You can't fill with negative currency"

-- removes from wallet.
removeWallet :: Wallet -> User -> User
removeWallet x (User c i wallet spent a k)
  | x > wallet = error " You can't remove more than you got! "
  | x > 0 = User c i (wallet - x) spent a k
  | otherwise = error "Invalid number"

--Clears the wallet
clearWallet :: User -> User
clearWallet (User c i wallet spent a k) = User c i 0 spent a k

-- Spent (adds to how much the user has spent.)
addSpent = undefined

-- Spent (removes to how much the user has spent.)
removeSpent = undefined

-- makes user a superuser.
makeAdmin = undefined

-- makes a superuser a normal noob user
removeAdmin = undefined
