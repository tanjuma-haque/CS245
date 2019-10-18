
module Main where
import System.Random

main :: IO()
main = do
  num <- (randomRIO (1,10) :: IO Integer)
  putStrLn "Plese enter the number that you guess"
  loop num

loop :: Integer -> IO ()
loop random_num = do
  number_entered <- getLine
  let random = show random_num
  case random == number_entered of
    True -> putStrLn ("Congratualation! The number " ++ show number_entered ++ " is corret!")
    False -> do
      putStrLn "Sorry, this is not the corret number. Please try again."
      loop random_num
