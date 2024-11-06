import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import Text.Regex.TDFA.String ()
import System.IO ()
import Debug.Trace (trace)

getMaxes :: String -> [Int]
getMaxes "" = [0, 0, 0]
getMaxes str =
    let matchesRed = getAllTextMatches (str =~ "(\\d+) red" :: AllTextMatches [] String)
        numbersRed = map read matchesRed :: [Int]
        matchesGreen = getAllTextMatches (str =~ "(\\d+) green" :: AllTextMatches [] String)
        numbersGreen = map read matchesGreen :: [Int]
        matchesBlue = getAllTextMatches (str =~ "(\\d+) blue" :: AllTextMatches [] String)
        numbersBlue = map read matchesBlue :: [Int]
    in [safeMax numbersRed, safeMax numbersGreen, safeMax numbersBlue]

safeMax :: [Int] -> Int
safeMax [] = 0
safeMax list = maximum list

isValid :: [Int] -> [Int] -> Bool
isValid [] [a, b, c] = False
isValid picks [maxred, maxgreen, maxblue] | trace ("Picks : " ++ show picks) False = undefined
isValid picks [maxred, maxgreen, maxblue] = head picks <= maxred
                    && picks !! 1 <= maxgreen
                    && picks !! 2 <= maxblue

validNo :: String -> [Int] -> Int
validNo "" [] = 0
validNo game maxes =
    let gameNo = game =~ "Game (\\d+):" :: Int
    in if isValid (getMaxes game) maxes then gameNo else 0

validGames :: [String] -> [Int] -> [Int]
validGames [] [a, b, c] = []
validGames games maxes = validNo (head games) maxes : validGames (tail games) maxes

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let valid = validGames (lines contents) [12, 13, 14]
    print (sum valid)