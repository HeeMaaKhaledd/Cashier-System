-- improving readability of code.
module User(User,setName,setId,getId,fillWallet,
            removeWallet,clearWallet,getWallet,
            addSpent,removeSpent,makeAdmin,removeAdmin,newUser) where

type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = User Name Id Wallet Spent IsAdmin deriving (Show, Eq)

newUser :: Name -> Id -> Wallet -> Spent -> IsAdmin -> User
newUser a b c d e = User a b c d e

-- a set of functions that is suitable for a user to have.
-- Set the name of the user.
setName :: Name -> User -> User
setName x (User c i wallet spent a ) = User x i wallet spent a

getName :: User -> Name
getName (User c i wallet spent a ) = c

-- Set the id of the user
setId :: Id -> User -> User
setId x (User c i wallet spent a ) = User c x wallet spent a

getId :: User -> Id
getId (User c i wallet spent a ) = i

-- add to wallet
fillWallet :: Wallet -> User -> User
fillWallet x (User c i wallet spent a )
  | x > 0 = User c i (wallet + x) spent a
  | otherwise = error "You can't fill with negative currency"

-- removes from wallet.
removeWallet :: Wallet -> User -> User
removeWallet x (User c i wallet spent a )
  | x > wallet = error " You can't remove more than you got! "
  | x > 0 = User c i (wallet - x) spent a
  | otherwise = error "Invalid number"

--Clears the wallet
clearWallet :: User -> User
clearWallet (User c i wallet spent a ) = User c i 0 spent a

getWallet :: User -> Wallet
getWallet (User c i wallet spent a ) = wallet

-- Spent (adds to how much the user has spent.)
addSpent :: Spent -> User -> User
addSpent x (User c i wallet spent a)
  | x > 0 = User c i wallet (spent + x) a
  | x < 0 = error "The value must be greater than zero"

-- Spent (removes to how much the user has spent.)
removeSpent :: User -> User
removeSpent (User c i wallet spent a) = User c i wallet 0 a

-- makes user a superuser.
makeAdmin ::User -> User
makeAdmin (User c i wallet spent a) = User c i wallet spent True

-- makes a superuser a normal noob user
removeAdmin ::User -> User
removeAdmin (User c i wallet spent a) = User c i wallet spent False
