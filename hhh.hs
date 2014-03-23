import Data.List (intersperse, minimumBy, maximumBy, transpose)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Maybe (isJust)


data Square = Empty | X | O deriving (Eq, Show)
type Grid = S.Seq Square
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

coordsToIdx :: (Int, Int) -> Int
coordsToIdx (x, y) = x * 3 + y

renderBoard :: Board -> IO ()
renderBoard (Board _ grid) = putStrLn $ unlines rows
  where rows = interspersePad (replicate (numSquares * 2 + 1) '-') $ map cols grid'
        cols line = interspersePad '|' $ map toX line
        grid' = toMatrix numSquares grid
        interspersePad n xs = n : intersperse n xs ++ [n]

-- generating and making moves
toMatrix :: Int -> S.Seq a -> [[a]]
toMatrix n xs
    | S.null xs = []
    | otherwise = toList h : toMatrix n t
        where (h, t) = S.splitAt n xs

generateNewMoves :: Turn -> Grid -> [Grid]
generateNewMoves turn grid = map setTurn $ S.elemIndicesL Empty grid
    where setTurn idx = S.update idx (turn2sq turn) grid

makeMove :: Board -> (Int, Int) -> Board
makeMove (Board turn squares) (x, y) = Board (flipTurn turn) (makeMove' squares (turn2sq turn) x y)

makeMove' :: Grid -> Square -> Int -> Int -> Grid
makeMove' grid s dx dy = S.update idx s grid
    where idx = coordsToIdx (dx, dy)


-- checking for wins and AI (trivial minimax)
filled :: Grid -> Bool
filled grid = case S.elemIndexL Empty grid of
                Nothing -> True
                Just _ -> False

matchingRow :: Grid -> Square -> Bool
matchingRow grid square = any (all (== square)) rows
    where rows = toMatrix numSquares grid

matchingCol :: Grid -> Square -> Bool
matchingCol grid = matchingRow $ S.fromList . concat $ transpose $ toMatrix numSquares grid

matchingDiag :: Grid -> Square -> Bool
matchingDiag grid square = matchNwSeDiag rows || matchNwSeDiag (map reverse rows)  -- todo
        where rows = toMatrix numSquares grid
              matchNwSeDiag xss = all (== square) $ zipWith (!!) xss [0..]

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
            if grid `S.index` coordsToIdx (row, col) /= Empty then do
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
startBoard = Board TurnX $ S.replicate (numSquares `square` 2) Empty
    where square :: Int -> Int -> Int
          square x y = x ^ y

main :: IO()
main = do
    renderBoard startBoard
    playGame startBoard
    return ()
