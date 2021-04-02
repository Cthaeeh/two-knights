module MoveGen where
import ChessData
import Util

generateMoves :: GameState -> [GameState]
generateMoves gs = concatMap (movesFromSquare gs) (squaresOccupiedByColor (onMove gs) (board gs))

squaresOccupiedByColor :: Color -> Board -> [Location]
squaresOccupiedByColor color board = [x | x <- allSquares, hasPieceWithColor board color x]
  where allSquares = cartesianProduct [0 .. 7] [0 .. 7]

hasPieceWithColor :: Board -> Color -> Location -> Bool
hasPieceWithColor board color position =  pieceColorAt board position == Just color

movesFromSquare :: GameState -> Location -> [GameState]
movesFromSquare gameState position = case board gameState `at` position of
  Empty -> []
  Full p -> movesWithPieceFrom p position gameState

movesWithPieceFrom :: Piece -> Location -> GameState -> [GameState]
movesWithPieceFrom piece from gameState = map (transformGameState gameState) (filter (isLegalMove gameState) (genericMoves piece from))

next :: Color -> Color
next c = case c of
  White -> Black
  Black -> White

-- TODO castling, en passant
transformGameState :: GameState -> Move -> GameState
transformGameState state move = GameState (next (onMove state)) (makeMove (board state) move) defaultCastlingRights False

-- TODO promotion 
makeMove :: Board -> Move -> Board
makeMove board (from, to) = replace (replace board to (board `at` from)) from Empty

replace :: Board -> Location -> Square -> Board
replace board (x, y) content = changeNth x (changeNth y content (board !! x)) board

genericMoves :: Piece -> Location -> [Move]
genericMoves piece from = case piece of
  Piece White Pawn -> movesFrom (pawnMovesUp from)
  Piece Black Pawn -> movesFrom (pawnMovesDown from)
  Piece _ Knight -> movesFrom (knightMoves from)
  Piece _ Bishop -> movesFrom (bishopMoves from)
  Piece _ Rook -> movesFrom (rookMoves from)
  Piece _ Queen -> movesFrom (queenMoves from)
  Piece _ King -> movesFrom (kingMoves from)
  where movesFrom = zip (repeat from)

isLegalMove :: GameState -> Move -> Bool
isLegalMove state move = not (isFriendlyFire (board state) move || isCollision (board state) move || isSuicide state move || isPieceSpecificIllegal state move)

at :: Board -> Location -> Square
at b (x, y) = b !! x !! y

pieceAt :: Board -> Location -> Maybe Piece
pieceAt board pos = case board `at` pos of
  Empty -> Nothing
  Full p -> Just p

pieceColorAt :: Board -> Location -> Maybe Color
pieceColorAt board pos = case board `at` pos of
  Empty -> Nothing
  Full p -> Just (color p)


-- You can not take your own pieces. 
isFriendlyFire :: Board -> Move -> Bool
isFriendlyFire board (to, from) = pieceColorAt board from == pieceColorAt board to

x :: (a, b) -> a
x = fst
y :: (a, b) -> b
y = snd

isFull :: Square -> Bool
isFull x = case x of
  Empty -> False
  x -> True

-- TODO make this more readable / robust.
-- Right now we implicitly assume diagonal or horizontal or vertical paths.
-- You can not jump over your own pieces (except knights). 
isCollision :: Board -> Move -> Bool
isCollision board move =
    let p = map (board `at`) (path move)
    in any isFull (drop 1 (reverse (drop 1 p)))

-- TODO unreadable code
path :: Move -> [Location]
path(from, to) =
    let xVals
          | x from == x to = repeat (x from)
          | x from > x to = reverse [x to .. x from]
          | otherwise = [x from .. x to];
        yVals
          | y from == y to = repeat (y from)
          | y from > y to = reverse [y to .. y from]
          | otherwise = [y from .. y to];
    in zip xVals yVals

-- You can make not get your self checked. 
isSuicide :: GameState -> Move -> Bool
isSuicide state move = False --TODO

-- Pawns can only capture diagonally, Castling rules must be valid, En passant must be valid.
isPieceSpecificIllegal :: GameState -> Move -> Bool
isPieceSpecificIllegal state (from, to) = case b `pieceAt` from of
  Just (Piece _ Pawn) -> (isDiagonalMove (from, to) && b `at` to == Empty) || (isStraightMove (from, to) && isFull (b `at` to))
  _ -> False
  where b = board state


isDiagonalMove :: Move -> Bool
isDiagonalMove move = abs (xDiff move) == abs (yDiff move)

isStraightMove :: Move -> Bool
isStraightMove move = xDiff move == 0 || yDiff move == 0

xDiff :: Move -> Int
xDiff (from, to) = x to - x from

yDiff :: Move -> Int
yDiff (from, to) = y to - y from

--
outside :: Location -> Bool
outside (x, y) = (0 > x) || (x > 7) || (0 > y) || (y > 7)

-- Generally possible rook moves from a position.
rookMoves :: Location  -> [Location]
rookMoves (x, y) = zip (repeat x) [0 .. 7]  ++ zip [0 .. 7] (repeat y)

-- Generally possible bishop moves from a position.
bishopMoves :: Location  -> [Location]
bishopMoves (x, y) = zip [x .. 7][y .. 7] ++ zip (reverse [0 .. x]) (reverse [0 .. y]) ++ zip [x .. 7](reverse [0 .. y]) ++ zip (reverse [0 .. x])[y .. 7]

-- Generally possible knight moves from a position.
knightMoves (x, y) = [x | x <- [(x + 1, y + 2), (x + 1, y - 2), (x + 2, y + 1), (x + 2, y - 1), (x - 1, y + 2), (x - 1, y - 2), (x - 2, y + 1), (x - 2, y - 1)], not (outside x)]

-- Generally possible queen moves from a position.
queenMoves :: Location  -> [Location]
queenMoves (x, y) = rookMoves (x, y) ++ bishopMoves (x, y)

-- Generally possible king moves from a position.
kingMoves (a, b) = [x | x <- [(a -1, b -1), (a -1, b), (a -1, b + 1), (a, b -1), (a, b + 1), (a + 1, b -1), (a + 1, b), (a + 1, b + 1)], not (outside x)]

-- Generally possible Upwards Pawn moves from a position. (white pawns)
pawnMovesUp :: Location  -> [Location]
pawnMovesUp (x, y) = [x | x <- [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)], not (outside x)] ++ [(x, y + 2) | y == 1]


-- Generally possible Downwards Pawn moves from a position. (black pawns)
pawnMovesDown :: Location  -> [Location]
pawnMovesDown (x, y) = [x | x <- [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)], not (outside x)] ++ [(x, y - 2) | y == 6]