-- towers.hs - A Towers of Hanoi game, implemented in Haskell

module Main where

import Data.Char
import Data.Maybe
import Data.Tuple
import Text.Regex.Posix

-- ## Data declarations

-- Even though the TowerList can be of arbitrary length, we always operate with three
-- towers.  The final integer describes how many total pieces there are.
data TowersofHanoi = TowerList [Tower] Int

-- Note that the beginning of a list will indicate the top of a tower, to make
-- popping an element off the beginning of the tower easier.
data Tower = Tower [Int]

instance Show TowersofHanoi where
    show (TowerList tlist _) = "Tower 1:" ++ show (head tlist) ++ "\nTower 2:"
        ++ show (tlist !! 1) ++ "\nTower 3:" ++ show (tlist !! 2)

-- Shows integers with constant spacing
showInt :: Int -> String
showInt n
    | n < 10 = ' ' : ' ' : show n
    | otherwise = ' ' : show n

getTowerList :: Tower -> [Int]
getTowerList (Tower lst) = lst

instance Show Tower where
    show = concatMap showInt . reverse . getTowerList

-- ## Function definitions

strToInt :: String -> Maybe Int
strToInt str
    | str =~ "^[0-9]+$" = Just (read str)
    | otherwise = Nothing

initializeTowers :: Int -> TowersofHanoi
initializeTowers n = TowerList [Tower [1..n], Tower [], Tower []] n

isValidMove :: TowersofHanoi -> Int -> Int -> Bool
isValidMove (TowerList tlist _) from to = valid where
-- We use Haskell's laziness to test these conditions in an order that won't throw
-- an error (e.g. if we call `!!` on an empty list)
    valid = validStack && stackNotEmpty && pieceNotLarger
    validStack = from `elem` [1,2,3] && to `elem` [1,2,3]
    stackNotEmpty = getTowerList fromStack /= []
    pieceNotLarger
        | null (getTowerList toStack) = True
        | otherwise = head (getTowerList fromStack) < head (getTowerList toStack)
    fromStack = tlist !! (from - 1)
    toStack = tlist !! (to - 1)

-- Assumes that the move is valid--use isValidMove to check this
moveTowers :: TowersofHanoi -> Int -> Int -> TowersofHanoi
moveTowers (TowerList tlist n) from to = TowerList tlist' n where
    tlist' = zipWith updateTower [1,2,3] tlist
-- updateTower takes a tower number and a tower and updates the tower
    updateTower a (Tower lst)
        | a == from = Tower (tail lst)
        | a == to = Tower (piece : lst)
        | otherwise = Tower lst
    piece = head $ getTowerList $ tlist !! (from - 1)

-- Since we start with everything on tower 1, only towers 2 and 3 count
gameOver :: TowersofHanoi -> Bool
gameOver (TowerList tlist n) = towerFinished (tlist !! 1) || towerFinished (tlist !! 2) where
    towerFinished t = getTowerList t == [1..n]

-- ## Main execution

main :: IO ()
main = do
    putStrLn "Welcome to Towers of Hanoi!\nHow many disks would you like to use?"
    lines <- getLine
    playGame $ initializeTowers (read lines :: Int)

playGame :: TowersofHanoi -> IO ()
playGame towers =
    if gameOver towers then endGame
    else do
        print towers
        putStrLn "Move piece from which tower?"
        fromStr <- getLine
        putStrLn "Move piece to which tower?"
        toStr <- getLine
        let from = strToInt fromStr
        let to = strToInt toStr
        if isJust from && isJust to then
            if isValidMove towers (fromJust from) (fromJust to) then
                playGame $ moveTowers towers (fromJust from) (fromJust to)
            else do
                putStrLn "Invalid move.  Please try again."
                playGame towers
        else do
            putStrLn "Invalid input.  Please try again."
            playGame towers

endGame :: IO ()
endGame = do
    putStrLn "Congratulations, you won!\nWould you like to play again? (y/n)"
    response <- getLine
    -- Handle an empty input string to be a no
    if response == "" then putStrLn "Thank you for playing!"
    else case toLower (head response) of
             'y' -> main
             _ -> putStrLn "Thank you for playing!"
