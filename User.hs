-- improving readability of code.
module User(User,setName,setId,getId,fillWallet,
            removeWallet,clearWallet,getWallet,
            addSpent,removeSpent,makeAdmin,removeAdmin,newUser) where

type Name = String
type Id = Int
type Wallet = Int
type Spent = Int
type IsAdmin = Bool

data User = Void | User Name Id Wallet Spent IsAdmin deriving (Show, Eq)

{- newUser
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
newUser :: Name -> Id -> Wallet -> Spent -> IsAdmin -> User
newUser a b c d e = User a b c d e

{- setName
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
setName :: Name -> User -> User
setName x (User c i wallet spent a ) = User x i wallet spent a

{- getName
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getName :: User -> Name
getName (User c i wallet spent a ) = c

{- setId
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
setId :: Id -> User -> User
setId x (User c i wallet spent a ) = User c x wallet spent a

{- getId
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getId :: User -> Id
getId (User c i wallet spent a ) = i

{- fillWallet
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
fillWallet :: Wallet -> User -> User
fillWallet x (User c i wallet spent a )
  | x > 0 = User c i (wallet + x) spent a
  | otherwise = error "You can't fill with negative currency"

{- removeWallet
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
removeWallet :: Wallet -> User -> User
removeWallet x (User c i wallet spent a )
  | x > wallet = error " You can't remove more than you got! "
  | x > 0 = User c i (wallet - x) spent a
  | otherwise = error "Invalid number"

{- clearWallet
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
clearWallet :: User -> User
clearWallet (User c i wallet spent a ) = User c i 0 spent a

{- getWallet
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
getWallet :: User -> Wallet
getWallet (User c i wallet spent a ) = wallet

{- addSpent
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
addSpent :: Spent -> User -> User
addSpent x (User c i wallet spent a)
  | x > 0 = User c i wallet (spent + x) a
  | x < 0 = error "The value must be greater than zero"

{- removeSpent
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
removeSpent :: User -> User
removeSpent (User c i wallet spent a) = User c i wallet 0 a

{- makeAdmin
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
makeAdmin ::User -> User
makeAdmin (User c i wallet spent a) = User c i wallet spent True

{- removeAdmin
   PRE:  True
   POST: Returns the argument name
   SIDE EFFECTS: ... if any, including exceptions ...
-}
removeAdmin ::User -> User
removeAdmin (User c i wallet spent a) = User c i wallet spent False
