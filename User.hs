-- improving readability of code.
module User(User,setName,setId,getId,fillWallet,
            removeWallet,clearWallet,getWallet,
            addSpent,removeSpent,makeAdmin,removeAdmin,newUser, getAdminStatus) where

type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = Void | User Name Id Wallet Spent IsAdmin deriving (Show, Eq)

{- newUser a b c d e
   PRE:  True
   POST: returns a new User from the variables a b c d
-}
newUser :: Name -> Id -> Wallet -> Spent -> IsAdmin -> User
newUser a b c d e = User a b c d e

{- setName x u
   PRE:  True
   POST: returns a new user with the name x
-}
setName :: Name -> User -> User
setName x (User c i wallet spent a ) = User x i wallet spent a

{- getName u
   PRE:  True
   POST: Returns the name of user u
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getName :: User -> Name
getName (User c i wallet spent a ) = c

{- setId i u
   PRE:  True
   POST: Returns a user u with a uppdated Id to i
-}
setId :: Id -> User -> User
setId x (User c i wallet spent a ) = User c x wallet spent a

{- getId u
   PRE:  True
   POST: Returns the id of user u
-}
getId :: User -> Id
getId (User c i wallet spent a ) = i

{- fillWallet w u
   PRE:  True
   POST: Returns the user u with a wallet now containing w more currency
   SIDE EFFECTS: in case of a negative w we return an error.
-}
fillWallet :: Wallet -> User -> User
fillWallet x (User c i wallet spent a )
  | x > 0 = User c i (wallet + x) spent a
  | otherwise = error "You can't fill with negative currency"

{- removeWallet w u
   PRE:  True
   POST: Returns the user u with a wallet now containing w less currency
   SIDE EFFECTS: in case of w being larger that what user aldready has. or if w is negative.
-}
removeWallet :: Wallet -> User -> User
removeWallet x (User c i wallet spent a )
  | x > wallet = error " You can't remove more than you got! "
  | x > 0 = User c i (wallet - x) spent a
  | otherwise = error "Invalid number"

{- clearWallet u
   PRE:  True
   POST: returns the user u with a empty wallet!
-}
clearWallet :: User -> User
clearWallet (User c i wallet spent a ) = User c i 0 spent a

{- getWallet u
   PRE:  True
   POST: Return the wallet of user u
-}
getWallet :: User -> Wallet
getWallet (User c i wallet spent a ) = wallet

{- addSpent s u
   PRE:  True
   POST: returns the user u with its (orignal spent value + s)
-}
addSpent :: Spent -> User -> User
addSpent x (User c i wallet spent a)
  | x >= 0 = User c i wallet (spent + x) a
  | x < 0 = error "The value must be greater than zero"

{- reduceSpent s u
   PRE:  True
   POST: Returns the user u with its (original spent value - s)
   SIDE EFFECTS:
   if s is larger than what the user already spent you get an error.
   if s is lesser than 0 we also get an error.
-}
reduceSpent :: Spent -> User -> User
reduceSpent x (User c i wallet spent isAdmin)
  | x >= 0 && x <= spent = User c i wallet (spent - x) isAdmin
  | x < 0                = error "The value myst be greater than zero"
  | otherwise            = error "You can't reduce spent with more than spent already is"

{- removeSpent s u
   PRE:  True
   POST: Returns the user u with its 0 spent.
-}
removeSpent :: User -> User
removeSpent (User c i wallet spent a) = User c i wallet 0 a

{- makeAdmin u
   PRE:  True
   POST: Returns the user u with a updated adminstatus
-}
makeAdmin ::User -> User
makeAdmin (User c i wallet spent a) = User c i wallet spent True

{- removeAdmin user u
   PRE:  True
   POST: Returns the with a updated adminstatus
-}
removeAdmin ::User -> User
removeAdmin (User c i wallet spent a) = User c i wallet spent False

getAdminStatus :: User -> Bool
getAdminStatus (User c i wallet spent a) = a
