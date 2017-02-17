-- improving readability of code.
type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = User Name Id Wallet Spent IsAdmin deriving Show

-- a set of functions that is suitable for a user to have.
-- Set the name of the user.
setName = undefined

-- Set the id of the user
setId = undefined

-- add to wallet
fillWallet = undefined

-- removes from wallet.
removeWallet = undefined

-- Spent (adds to how much the user has spent.)
addSpent = undefined

-- Spent (removes to how much the user has spent.)
removeSpent = undefined

-- makes user a superuser.
makeAdmin = undefined

-- makes a superuser a normal noob user
removeAdmin = undefined
