module PrettyPrint where
import ChessData
import Data.List

pprintSquare :: Square -> String
pprintSquare s = case s of
    Empty -> " "
    Full (Piece White King) -> "K"
    Full (Piece White Queen) -> "Q"
    Full (Piece White Rook) -> "R"
    Full (Piece White Knight) -> "N"
    Full (Piece White Bishop) -> "B"
    Full (Piece White Pawn) -> "P"
    Full (Piece Black King) -> "k"
    Full (Piece Black Queen) -> "q"
    Full (Piece Black Rook) -> "r"
    Full (Piece Black Knight) -> "n"
    Full (Piece Black Bishop) -> "b"
    Full (Piece Black Pawn) -> "p"

pprintLine :: [Square] -> String
pprintLine = foldl (\l r -> concat [pprintSquare r, " ", l]) ""

pprintBoard :: Board -> String
pprintBoard b = foldr (\l r -> concat [r, pprintLine l, "\n"]) "" (transpose (reverse b))

pprintBoards :: [Board] -> String
pprintBoards = foldr (\l r -> concat [r, pprintBoard l, "---------------\n"]) ""
