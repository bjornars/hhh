import Data.List

interspersePad :: a -> [a] -> [a]
interspersePad n xs = n : intersperse n xs ++ [n]

data Square = Empty | X | O
data Board = Board [[Square]]

toX :: Square -> Char
toX Empty = ' '
toX X = 'X'
toX O = 'O'

renderRow :: [Square] -> String
renderRow xs = interspersePad '|' $ map toX xs

renderBoard :: Board -> [String]
renderBoard (Board xss) = interspersePad (replicate 7 '-') $ map renderRow xss

startBoard :: Board
startBoard = Board $ replicate 3 $ replicate 3 Empty

main :: IO()
main = putStrLn . unlines $ renderBoard startBoard 
