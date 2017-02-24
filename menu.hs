main = do
    putStrLn "What do you want to do?"
    putStrLn "[1]Buy item"
    putStrLn "[2]Insert item"
    putStrLn "[3]Remove item"
    x <- getLine
    run (read x :: Int)

run :: Int -> IO ()
run choice
  | choice == 1 = do
        putStrLn "Good input"
  | choice == 2 = do
        putStrLn "Good input"
  | choice == 3 = do
        putStrLn "Good input"

  -- | otherwise = do
  --         putStrLn "Bad input, try again"
  --         run (read x :: Int)
