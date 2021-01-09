module MoveGen where
import ChessData
import Util

generateMoves :: GameState -> [GameState]
generateMoves gs = concatMap (movesFromSquare gs) (squaresOccupiedByColor (onMove gs) (board gs))

squaresOccupiedByColor :: Color -> Board -> [Sqr]
squaresOccupiedByColor color board = [x | x <- allSquares, hasPieceWithColor board color x] 
  where allSquares = cartesianProduct [0 .. 7] [0 .. 7]

hasPieceWithColor :: Board -> Color -> Sqr -> Bool
hasPieceWithColor board color position =  pieceColorAt board position == Just color

movesFromSquare :: GameState -> Sqr -> [GameState]
movesFromSquare gameState position = case board gameState `at` position of
  Empty -> []
  Full p -> movesWithPieceFrom p position gameState

movesWithPieceFrom :: Piece -> Sqr -> GameState -> [GameState]
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

replace :: Board -> Sqr -> Square -> Board
replace board (x, y) content = changeNth x (changeNth y content (board !! x)) board

genericMoves :: Piece -> Sqr -> [Move] 
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

at :: Board -> Sqr -> Square 
at b (x, y) = b !! x !! y

pieceAt :: Board -> Sqr -> Maybe Piece 
pieceAt board pos = case board `at` pos of
  Empty -> Nothing 
  Full p -> Just p

pieceColorAt :: Board -> Sqr -> Maybe Color 
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

isFull x = case x of
  Empty -> False
  x -> True

-- You can not jump over your own pieces (except knights). 
isCollision :: Board -> Move -> Bool
isCollision board (to, from) = 
    let path = zipWith (curry (at board)) [x from .. x to] [y from .. y to];
    in any isFull (drop 1 (reverse (drop 1 path)))
    
-- You can make not get your self checked. 
isSuicide :: GameState -> Move -> Bool
isSuicide state move = False

-- Pawns can only capture diagonally, Castling rules must be valid, En passant must be valid.
isPieceSpecificIllegal :: GameState -> Move -> Bool
isPieceSpecificIllegal state (from, to) = case b `pieceAt` from of 
  Just (Piece White Pawn) -> isDiagonalMove (from, to) && b `at` to == Empty 
  Just (Piece Black Pawn) -> isDiagonalMove (from, to) && b `at` to == Empty
  _ -> False
  where b = board state


isDiagonalMove :: Move -> Bool
isDiagonalMove move = abs (xDiff move) == abs (yDiff move)

xDiff (from, to) = x to - x from
yDiff (from, to) = y to - y from

--
outside :: Sqr -> Bool
outside (x, y) = (0 > x) || (x > 7) || (0 > y) || (y > 7)

-- Generally possible rook moves from a position.
rookMoves (x, y) = zip [0 .. 7] (repeat x) ++ zip (repeat y) [0 .. 7]

-- Generally possible bishop moves from a position.
bishopMoves (x, y) = zip [x .. 7][y .. 7] ++ zip [x .. 0][y .. 0] ++ zip [x .. 7][y .. 0] ++ zip [x .. 0][y .. 7]

-- Generally possible knight moves from a position.
knightMoves (x, y) = [x | x <- [(x + 1, y + 2), (x + 1, y - 2), (x + 2, y + 1), (x + 2, y - 1), (x - 1, y + 2), (x - 1, y - 2), (x - 2, y + 1), (x - 2, y - 1)], not (outside x)]

-- Generally possible queen moves from a position.
queenMoves (a, b) = rookMoves (a, b) ++ bishopMoves (a, b)

-- Generally possible king moves from a position.
kingMoves (a, b) = [x | x <- [(a -1, b -1), (a -1, b), (a -1, b + 1), (a, b -1), (a, b + 1), (a + 1, b -1), (a + 1, b), (a + 1, b + 1)], not (outside x)]

-- Generally possible Upwards Pawn moves from a position. (white pawns)
pawnMovesUp :: Sqr  -> [Sqr]
pawnMovesUp (x, y) = [x | x <- [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)], not (outside x)] ++ [(x, y + 2) | y == 1]


-- Generally possible Downwards Pawn moves from a position. (black pawns)
pawnMovesDown :: Sqr  -> [Sqr]
pawnMovesDown (x, y) = [x | x <- [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)], not (outside x)] ++ [(x, y - 2) | y == 6]