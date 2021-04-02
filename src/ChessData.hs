module ChessData where 

import Data.List ( transpose )

data Color = White | Black deriving (Show, Eq)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Piece = Piece { color     :: Color ,
                     pieceType :: PType
                   } deriving (Show, Eq)

data Square = Empty | Full Piece deriving (Show, Eq)
type Board = [[Square]]

type Location = (Int, Int)
type Move = (Location, Location) 

type EnPassantRights = Bool
data CastlingRights = CastlingRights {whiteOO  :: Bool,
                                      whiteOOO :: Bool,
                                      blackOO  :: Bool,
                                      blackOOO :: Bool} deriving (Show, Eq)

data GameState = GameState {onMove          :: Color,
                            board           :: Board,
                            castlingRights  :: CastlingRights,
                            enPassantRights :: EnPassantRights} deriving (Show, Eq)

eM = Empty

wK = Full (Piece White King)
wQ = Full (Piece White Queen)
wR = Full (Piece White Rook)
wB = Full (Piece White Bishop)
wN = Full (Piece White Knight)
wP = Full (Piece White Pawn)

bK = Full (Piece Black King)
bQ = Full (Piece Black Queen)
bR = Full (Piece Black Rook)
bB = Full (Piece Black Bishop)
bN = Full (Piece Black Knight)
bP = Full (Piece Black Pawn)

-- white king is on 4 0 (e1)
defaultBoard = transpose (reverse [[bR,bN,bB,bQ,bK,bB,bN,bR],
                                   [bP,bP,bP,bP,bP,bP,bP,bP],
                                   [eM,eM,eM,eM,eM,eM,eM,eM],
                                   [eM,eM,eM,eM,eM,eM,eM,eM],
                                   [eM,eM,eM,eM,eM,eM,eM,eM],
                                   [eM,eM,eM,eM,eM,eM,eM,eM],
                                   [wP,wP,wP,wP,wP,wP,wP,wP],
                                   [wR,wN,wB,wQ,wK,wB,wN,wR]])

defaultCastlingRights = CastlingRights False False False False

defaultGameState = GameState{onMove = White,
                             board = defaultBoard,
                             castlingRights = defaultCastlingRights,
                             enPassantRights = False}
