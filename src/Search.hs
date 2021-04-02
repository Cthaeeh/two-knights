module Search where

import Data.Foldable
import Data.Tree
import ChessData
import MoveGen
import StaticEval (staticEval)

import Data.Ord

eval :: GameState -> Double
eval s = minimum $ approxMinDecreasin $ staticEvalTree $ prune 6 $ gameTree s

makeAIMove :: GameState -> GameState
makeAIMove gameState = case onMove gameState of
    White -> maximumBy (comparing eval) (generateMoves gameState)
    Black -> minimumBy (comparing eval) (generateMoves gameState)


makeNode state = (state, generateMoves state)

gameTree = unfoldTree makeNode

prune :: Int -> Tree a -> Tree a
prune 0 (Node x ts) = Node x []
prune depth (Node x ts) = Node x (map (prune (depth - 1)) ts)

staticEvalTree = fmap (staticEval . board)

type ApproxUp = [Double]
type ApproxDown = [Double]

-- Alphabeta search
approxMaxImproving :: Tree Double -> [Double]
-- Maximum of leaf is just its static value
approxMaxImproving (Node n []) = [n]
-- Maximum of node is maximized value of minimized sub nodes. 
approxMaxImproving (Node n subTree) = approxMax (map approxMinDecreasin subTree)

approxMinDecreasin :: Tree Double -> [Double]
-- Minimum of leaf is just its static value
approxMinDecreasin (Node n []) = [n]
-- Minimum of node is minimized value of maximized sub nodes. 
approxMinDecreasin (Node n subTree) = approxMin (map approxMaxImproving subTree)

-- Efficient operator > for DownwardsApprox Value
optimizedGreater :: Double -> ApproxDown -> Bool
optimizedGreater val = foldr (\ first -> (||) (val > first)) False

betterThan :: Double -> [ApproxDown] -> ApproxUp
betterThan val [] = []
betterThan val (first : rest) =
    if optimizedGreater val first then minimum first : betterThan (minimum first) rest
                                  else betterThan val rest

worseThan :: Double -> [ApproxDown] -> ApproxUp
worseThan val (first : rest) = [val]
worseThan val [] = []

-- Approximate a maximum in a list of approximated values (decreasing lists, truer value in tail).
approxMax :: [ApproxDown] -> ApproxUp
approxMax (first : rest) = minimum first : betterThan (minimum first) rest

-- Approximate a minimum in a list of approximated values (increasing lists, truer value in tail).
approxMin :: [ApproxUp] -> ApproxDown
approxMin (first : rest) = maximum first : worseThan (maximum first) rest