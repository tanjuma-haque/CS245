{- Author: <Vinty (Liwen) Guo, Umme Tanjuma Haque>
        File: Do.hs
        Practice exercises with do-notation
-}
module Main where

main :: IO ()
main = do
  {-
  putStrLn "Hello, world!"
  name <- prompt "What is your name?"
  greet <- pure "Hello, "
  putStrLn $ greet ++ name
  -}

  testing <- prompt2 "Enter two lines of text:"
  putStrLn testing

prompt :: String -> IO String
prompt query = do
  putStrLn query
  answer <- getLine
  pure answer

prompt2 :: String -> IO String
prompt2 query = do
  putStrLn query
  first_line <- getLine
  second_line <- getLine
  let final = first_line ++ second_line
  pure final
