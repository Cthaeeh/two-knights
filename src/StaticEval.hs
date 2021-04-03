
module StaticEval where

import ChessData

-- TODO add piece square tables.
staticEval :: Board -> Int
staticEval b = sum (map squareValue (concat b))

squareValue :: Square -> Int
squareValue s = case s of
    Empty -> 0
    Full (Piece White p) -> pieceValue p
    Full (Piece Black p) -> negate (pieceValue p)

pieceValue :: PType -> Int
pieceValue p = case p of
        Pawn -> 100
        Knight -> 300
        Bishop -> 330
        Rook -> 500
        Queen -> 900
        King -> 100000
