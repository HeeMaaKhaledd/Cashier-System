module Check(checkIfOnlyInt,checkIfBool) where

checkIfOnlyInt :: String -> Bool
checkIfOnlyInt [] = True
checkIfOnlyInt (x:xs)
  | x == '0'  = checkIfOnlyInt xs
  | x == '1'  = checkIfOnlyInt xs
  | x == '2'  = checkIfOnlyInt xs
  | x == '3'  = checkIfOnlyInt xs
  | x == '4'  = checkIfOnlyInt xs
  | x == '5'  = checkIfOnlyInt xs
  | x == '6'  = checkIfOnlyInt xs
  | x == '7'  = checkIfOnlyInt xs
  | x == '8'  = checkIfOnlyInt xs
  | x == '9'  = checkIfOnlyInt xs
  | otherwise = False

checkIfBool :: String -> Bool
checkIfBool s
  | s == "True"  = True
  | s == "False" = True
  | otherwise    = False
