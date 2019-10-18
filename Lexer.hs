{- Name: <Tanjuma Haque>
 File: Lexer.hs
 Desc: A Java lexer
-}
module Main where
import Data.Char
import System.Environment
import System.Exit
import System.FilePath
main :: IO ()
main = do
 args <- getArgs
 filename <- checkArgs args
 input <- readFile filename
 let result = lexJava input
 writeFile (takeBaseName filename <.> "lex") (unlines result)
-- Check the command-line arguments. Returns the filename
-- to lex upon success.
checkArgs :: [String] -> IO FilePath
checkArgs [path] = pure path
checkArgs _other = do
 putStrLn "Usage: ./Lexer <filename>.java"
 putStrLn "Writes to <filename>.lex"
 exitFailure
-- Takes Java code as input and returns a list of Strings.
-- Each String in the output list is one Java token.
-- Comments and whitespace are discarded.

-- lexJava uses lexNoPrefix on findToken which is used on the input string to give a list of tokens
lexJava :: String -> [String]
lexJava "" = []
lexJava (x:xs) = lexNoPrefix(findToken (x:xs))

-- lex1 takes in a character and string and returns two strings, in which the first one is token type of the char and the 2nd is the remainder
lex1 :: Char -> String -> (String, String)
lex1 n "" = ( n:"", "")
lex1 n (x:xs)
  | n == '0' = (n : fst (ifFirstZero (x:xs)) , snd (ifFirstZero (x:xs)))
  | isLetter n = findLongestIdentfr (n:x:xs)
  | (isDigit n && n /= '0') = findDigit(n:x:xs)
  | isSymbol(n) = findOpandSep (n:x:xs)
  | otherwise = (n:"", xs) 

--uses Lex1 to lex the first token (first part of tuple) and recurs lexJava from the second part of the tuple 
lexNoPrefix :: String -> [String]
lexNoPrefix "" = []
lexNoPrefix (x:xs)  = (fst pair4):(lexJava (snd pair4))
    where pair4 = lex1 x xs

--finds longest identifier in char+string of Lex1 and returns (identifer, rest of str)
findLongestIdentfr :: String -> (String,String) 
findLongestIdentfr "" = ( "", "")
findLongestIdentfr (x:xs) 
  | (isLetter x || x == '_' || x == '$'|| isDigit x) = (x : fst pair, snd pair)
  | otherwise = ("", x:xs)
      where pair = findLongestIdentfr xs

--tokenizes digits that do not start with 0 and ends with L/l
findDigit :: String -> (String, String) 
findDigit "" = ("", "")
findDigit (x:xs)
  |(isDigit x || x == '_' )= (x:fst pair3, snd pair3)
  |((x == 'L'|| x == 'l') && xs == "") = (x:fst pair3, snd pair3)
  | otherwise = ("", x:xs)
      where pair3 = findDigit xs


--if first char is 0, this function deals with hexadecimal, binary, oct etc. // The function fails to add L/l to digits if it is in the middle of the string.
-- I tried adding another guard that checked for whitespaces instead of empty string but it did not work hence, I removed that part. 
ifFirstZero :: String -> (String, String)
ifFirstZero "" = ("", "")
ifFirstZero (x:xs) 
 | (x == 'x'|| x =='X') =  (x : fst pair6, snd pair6) 
 | isDigit x || x == '_' = ( (x : fst pair5), snd pair5)
 | (x == 'b' || x =='B') = ((x:fst pair7), snd pair7)
 | otherwise = ("0", x:xs)
        where pair5 = ifFirstZero (xs)
              pair6 = findHexDigit (xs)
              pair7 = findBinaryDigit (xs)


--it simply finds a hexdigit before allowing underscores
findHexDigit :: String -> (String, String) 
findHexDigit "" = ("", "")
findHexDigit (x:xs) 
 | isHexDigit x = findUnderscoreHex(x:xs)
 | otherwise = ("", x:xs)

--allows underscores as part of the token including L/l
findUnderscoreHex :: String -> (String, String) 
findUnderscoreHex "" = ("", "")
findUnderscoreHex (x:xs) 
 |(isHexDigit x || x == '_') = (x : fst pair1, snd pair1)
 |((x == 'L'|| x == 'l') && xs == "") = (x:fst pair1, snd pair1)
 | otherwise = ("", x:xs) 
       where pair1 = findUnderscoreHex(xs)

--finds a bit before allowing underscores
findBinaryDigit :: String -> (String, String)
findBinaryDigit "" = ("", "")
findBinaryDigit (x:xs) 
 |( x == '0' || x =='1') = findUnderscoreBin(x : xs)
 | otherwise = ("", x:xs)

-- allows underscores for Bin
findUnderscoreBin :: String -> (String, String)
findUnderscoreBin "" = ("", "")
findUnderscoreBin (x:xs) 
 | (x == '0' || x == '1'|| x == '_' ) = (x : fst pair2, snd pair2) 
 | ((x == 'L'|| x == 'l') && xs == "") = (x:fst pair2, snd pair2)
 | otherwise = ("", x:xs)
       where pair2 = findUnderscoreBin(xs)

-- I tried making a operator function but it was not working and I have it commented below:
{-}
findOperator :: String -> (String, String)
findOperator "" = ("", "")
findOperator(x:xs)
  | ([x] `elem` operators) = ((x : fst pair8), snd pair8)
  | otherwise = ("", x:xs)
findOperator(x:y:xs)
  | ([x:y] `elem` operators) = ((x:y : fst pair9), snd pair9)
  | otherwise = ("", x:y:xs)
findOperator(x:y:w:xs)
  | ([x:y:w] `elem` operators) = ((x:y:w : fst pair10), snd pair10)
  | otherwise = ("", x:y:w:xs)
findOperator(x:y:w:z:xs)
  | ([x:y:w:z] `elem` operators)= ((x:y:w:z : fst pair11), snd pair11)
  | otherwise = ("", x:y:w:z:xs)
 where operators = ["=", ">", "<", "!", "~", "?", ":", "->", "==", ">=", "<=", "!=", "&&", "||", "++", "--", "+", "-", "*", "/", "&", "|", "^", "%", "<<", ">>", ">>>", "+=", "-=","*=", "/=", "&=", "|=", "^=", "%=", "<<=", ">>=", ">>>="]
       pair8 = findOperator (y:w:z:xs)
       pair9 = findOperator (w:z:xs)
       pair10 = findOperator (z:xs)
       pair11 = findOperator (xs)
  -}     
-- alternative to operators and separators function
findOpandSep :: String -> (String, String)
findOpandSep "" = ("", "")
findOpandSep(x:xs)
   |isSymbol(x) = (x: (fst pair12) , snd pair12)
   |otherwise = ("", x:xs)
      where pair12 = findOpandSep xs

-- removes whitespaces and comments from the string
findToken :: String -> String
findToken "" = ""
findToken (z:y:xs) 
 | isSpace z = findToken (y:xs)
 | (z == '/' && y == '/') =  findSingleComment xs
 | (z == '/' && y == '*') = findMultiComment xs
 | otherwise = z:y:xs
findToken (x:xs) = (x:xs)

--this function is called by findToken if only there is // and this functin calls itself until it finds the end of the comment
-- where it recurs back to findToken to check for any whitespaces after it
findMultiComment :: String -> String
findMultiComment "" = ""
findMultiComment (z:y:xs)
 | (z == '*' && y == '/' ) = findToken xs
 | otherwise = findMultiComment (y:xs)
--this function is called by findToken if only there is /* and this functin calls itself until it finds the end of the comment
-- where it recurs back to findToken to check for any whitespaces after it
findSingleComment :: String -> String 
findSingleComment "" = ""
findSingleComment (x:xs)
 | x == '\n' = findToken xs
 | otherwise = findSingleComment xs








