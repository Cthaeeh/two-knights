module Search where

import Data.Foldable
import Data.Tree
import ChessData
import MoveGen

import Data.Ord

makeNode state = (state, generateMoves state) 

gameTree = unfoldTree makeNode 

prune :: Int -> Tree a -> Tree a
prune 0 (Node x ts) = Node x []
prune depth (Node x ts) = Node x (map (prune (depth - 1)) ts)

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
        
maximise :: Tree Double -> Double 
maximise (Node n []) = n 
maximise (Node n subTree) = maximum (map minimise subTree)

minimise :: Tree Double -> Double 
minimise (Node n []) = n
minimise (Node n subTree) = minimum (map maximise subTree)

staticEvalTree = fmap (staticEval .board)

-- TODO also allow to play white. I think right now it will give bad moves for white.
eval x = minimise $ staticEvalTree $ prune 3 $ gameTree x

makeAIMove :: GameState -> GameState 
makeAIMove gameState = case onMove gameState of
    White -> maximumBy (comparing eval) (generateMoves gameState)
    Black -> minimumBy (comparing eval) (generateMoves gameState)