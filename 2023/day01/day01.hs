import System.IO
import Data.Char (isDigit, digitToInt)
import Data.Time

findDigits :: String -> [Int]
findDigits "" = []
findDigits (f:suite) = if isDigit f then digitToInt f : findDigits suite else findDigits suite

findAllDigits :: [String] -> [[Int]]
findAllDigits [] = []
findAllDigits s = map findDigits s

constructValue :: [Int] -> Int
constructValue [] = 0
constructValue l = head l * 10 + last l

constructAllValues :: [[Int]] -> [Int]
constructAllValues = map constructValue 

main :: IO ()
main = do
    start <- getCurrentTime
    contents <- readFile "input.txt"
    -- putStrLn contents
    let allValues = constructAllValues (findAllDigits (lines contents))
    -- putStrLn (unlines (map show allValues))
    let result = sum allValues
    end <- getCurrentTime
    print result
    print $ diffUTCTime end start