module Check(checkIfOnlyInt,checkIfBool) where
{- checkIfOnlyInt s
   PRE:  True
   POST: returns a boolean True/False wether the string contains something else than integers
   EXAMPLES: checkIfOnlyInt "1123" = True
             checkIfOnlyInt "a123" = False
             checkIfOnlyInt [] = False
  -}
checkIfOnlyInt :: String -> Bool
checkIfOnlyInt [] = False
checkIfOnlyInt s = checkIfOnlyIntAUX s
{- checkIfOnlyIntAUX x
   PRE:  True
   POST: determines if each x is an integer, else False
   VARIANT: length of x
   EXAMPLES: checkIfOnlyIntAUX "1123" = True
             checkIfOnlyIntAUX "a123" = False
             checkIfOnlyIntAUX [] = True
-}
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
{- checkIfBool
   PRE:  True
   POST: determines if each s is a Boolean. else False
   EXAMPLES: checkIfBool "True" = True
             checkIfBool "False" = True
             checkIfBool "asd" = False
-}
checkIfBool :: String -> Bool
checkIfBool s
  | s == "True"  = True
  | s == "False" = True
  | otherwise    = False
