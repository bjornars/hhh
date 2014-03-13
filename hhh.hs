import Data.List (intersperse, minimumBy, maximumBy, transpose)
import Data.Maybe (isJust)

data Square = Empty | X | O deriving (Eq, Show)
type Grid = [[Square]]
data Turn = TurnX | TurnO deriving (Eq, Show)
data Board = Board Turn Grid

getGrid :: Board -> Grid
getGrid (Board _ grid) = grid

numSquares :: Int
numSquares = 3

flipTurn :: Turn -> Turn
flipTurn TurnX = TurnO
flipTurn TurnO = TurnX

turn2sq :: Turn -> Square
turn2sq TurnX = X
turn2sq TurnO = O

-- rendering
toX :: Square -> Char
toX Empty = ' '
toX X = 'X'
toX O = 'O'

renderBoard :: Board -> IO ()
renderBoard (Board _ squares) = putStrLn $ unlines rows
  where rows = interspersePad (replicate (numSquares * 2 + 1) '-') $ map cols squares
        cols line = interspersePad '|' $ map toX line
        interspersePad n xs = n : intersperse n xs ++ [n]

-- generating and making moves
unconcat :: Int -> [a] -> [[a]]
unconcat _ [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)

perm :: Turn -> [Square] -> Grid
perm turn = perm' []
    where perm' :: [Square] -> [Square] -> Grid
          perm' h (Empty: []) = [h ++ [turn2sq turn]]
          perm' h (Empty: xs) = (h ++ turn2sq turn : xs) : perm' (h ++ [Empty]) xs
          perm' h (x:xs) = perm' (h ++ [x]) xs
          perm' _ _ = []

generateNewMoves :: Turn -> Grid -> [Grid]
generateNewMoves turn squares = map (unconcat numSquares) $ perm turn $ concat squares

makeMove :: Board -> (Int, Int) -> Board
makeMove (Board turn squares) (x, y) = Board (flipTurn turn) (makeMove' squares (turn2sq turn) x y)

makeMove' :: Grid -> Square -> Int -> Int -> Grid
makeMove' squares s dx dy = replace dx squares $ replace dy (squares !! dx) s
    where replace i xs x = take i xs ++ [x] ++ drop (i + 1) xs


-- checking for wins and AI (trivial minimax)
filled :: Grid -> Bool
filled squares = Empty `notElem` concat squares

matchingRow :: Grid -> Square -> Bool
matchingRow squares square = any (all (== square)) squares

matchingCol :: Grid -> Square -> Bool
matchingCol squares = matchingRow $ transpose squares

matchingDiag :: Grid -> Square -> Bool
matchingDiag squares square = matchNwSeDiag squares || matchNwSeDiag (map reverse squares)
        where matchNwSeDiag xss = all (== square) $ zipWith (!!) xss [0..]
              matchNwSeDiag :: Grid -> Bool

isWin :: Grid -> Maybe Turn
isWin squares
    | isWinFor O = Just TurnO
    | isWinFor X = Just TurnX
    | otherwise = Nothing
    where isWinFor square = matchingRow squares square
                         || matchingCol squares square
                         || matchingDiag squares square


score :: Board -> (Int, Grid)
score (Board turn squares)
    | Just TurnX <- win = (100, squares)
    | Just TurnO <- win = (-100, squares)
    | filled squares    = (0, squares)
    | turn == TurnX     = maximumBy (compareBy fst) $ zipScore nextMoves
    | otherwise         = minimumBy (compareBy fst) $ zipScore nextMoves

    where win = isWin squares
          nextMoves = generateNewMoves turn squares
          scoreSquares sqss = fst $ score (Board (flipTurn turn) sqss)
          zipScore moves = zip (map scoreSquares moves) moves
          compareBy f x y  = f x `compare` f y


-- IO stuff
parseInput :: String -> Maybe (Int, Int)
parseInput input
        | n' < 0 = Nothing
        | n' > 8 = Nothing
        | otherwise = Just (n' `div` numSquares, n' `mod` numSquares)
        where n' = case reads input of
                        [(n, "")] -> n - 1
                        _  -> -1

getValidMove :: Grid -> IO (Int, Int)
getValidMove grid = do
    -- recurse until we get a valid move input
    input <- getLine
    case parseInput input of
        Nothing -> do
            putStrLn "cannot parse"
            getValidMove grid
        Just (row, col) ->
            if grid !! row !! col /= Empty then do
                putStrLn "not empty"
                getValidMove grid
            else
                return (row, col)

playGame :: Board -> IO ()
playGame board@(Board TurnX _) = do
    putStrLn "You are X. Make a move, 1-9 (starting upper left)"
    move <- getValidMove $ getGrid board
    let board' = makeMove board move
    renderBoard board'
    if isJust $ isWin $ getGrid board' then
        putStrLn "You won!"
    else if filled $ getGrid board' then
        putStrLn "Draw"
    else
        playGame board'

playGame (Board TurnO grid) = do
    let best = score $ Board TurnO grid
    let board' = Board TurnX (snd best)
    renderBoard board'
    if isJust $ isWin $ getGrid board' then
        putStrLn "You lost!"
    else if filled $ getGrid board' then
        putStrLn "Draw"
    else
         playGame board'

startBoard :: Board
startBoard = Board TurnX $ replicate numSquares $ replicate numSquares Empty

main :: IO()
main = do
    renderBoard startBoard
    playGame startBoard
    return ()
