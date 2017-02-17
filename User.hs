-- improving readability of code.
type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = User Name Id Wallet Spent IsAdmin deriving Show

-- a set of functions that is suitable for a user to have.
-- Set the name of the user.
setName :: Name -> User -> User
setName x (User c i wallet spent a ) = User x i wallet spent a

-- Set the id of the user
setId :: Id -> User -> User
setId x (User c i wallet spent a ) = User c x wallet spent a

-- add to wallet
fillWallet :: Wallet -> User -> User
fillWallet x (User c i wallet spent a)
  | x > 0 = User c i (wallet + x) spent a
  | otherwise = error "You can't fill with negative currency"
-- removes from wallet.
removeWallet :: Wallet -> User -> User
removeWallet x (User c i wallet spent a)
  | x > wallet = error " You can't remove more than you got! "
  | x > 0 = User c i (wallet - x) spent a
  | otherwise = error "Invalid number"

--Clears the wallet
clearWallet :: User -> User
clearWallet (User c i wallet spent a) = User c i 0 spent a
-- Spent (adds to how much the user has spent.)
addSpent = undefined

-- Spent (removes to how much the user has spent.)
removeSpent = undefined

-- makes user a superuser.
makeAdmin = undefined

-- makes a superuser a normal noob user
removeAdmin = undefined
