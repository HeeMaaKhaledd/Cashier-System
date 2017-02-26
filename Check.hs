module Check(checkIfOnlyInt,checkIfBool) where

checkIfOnlyInt :: String -> Bool
checkIfOnlyInt [] = False
checkIfOnlyInt s = checkIfOnlyIntAUX s

checkIfOnlyIntAUX :: String -> Bool
checkIfOnlyIntAUX [] = True
checkIfOnlyIntAUX (x:xs)
  | x == '0'  = checkIfOnlyIntAUX xs
  | x == '1'  = checkIfOnlyIntAUX xs
  | x == '2'  = checkIfOnlyIntAUX xs
  | x == '3'  = checkIfOnlyIntAUX xs
  | x == '4'  = checkIfOnlyIntAUX xs
  | x == '5'  = checkIfOnlyIntAUX xs
  | x == '6'  = checkIfOnlyIntAUX xs
  | x == '7'  = checkIfOnlyIntAUX xs
  | x == '8'  = checkIfOnlyIntAUX xs
  | x == '9'  = checkIfOnlyIntAUX xs
  | otherwise = False

checkIfBool :: String -> Bool
checkIfBool s
  | s == "True"  = True
  | s == "False" = True
  | otherwise    = False
