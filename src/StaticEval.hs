
module StaticEval where

import ChessData

-- TODO add piece square tables.
staticEval :: Board -> Double
staticEval b = sum (map squareValue (concat b))

squareValue :: Square -> Double
squareValue s = case s of
    Empty -> 0
    Full (Piece White p) -> pieceValue p
    Full (Piece Black p) -> negate (pieceValue p)

pieceValue :: PType -> Double
pieceValue p = case p of
        Pawn -> 1
        Knight -> 3
        Bishop -> 3.3
        Rook -> 5
        Queen -> 9
        King -> 1000
