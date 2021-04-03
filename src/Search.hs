module Search where

import Data.Foldable
import Data.Tree
import ChessData
import MoveGen
import StaticEval (staticEval)

import Data.Ord

-- I think we do some unecessary calculations in the first ply, think about it.
eval :: GameState -> Int
eval s = if onMove s == White
    then maximum $ approxMaxImproving treeStatic
    else minimum $ approxMinDecreasin treeStatic
    where treeStatic = staticEvalTree $ prune 3 $ gameTree s

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

-- Make this strong types ?
type ApproxUp = [Int]
type ApproxDown = [Int]

-- Alphabeta search
approxMaxImproving :: Tree Int -> [Int]
-- Maximum of leaf is just its static value
approxMaxImproving (Node n []) = [n]
-- Maximum of node is maximized value of minimized sub nodes. 
approxMaxImproving (Node n subTree) = approxMax (map approxMinDecreasin subTree)

approxMinDecreasin :: Tree Int -> [Int]
-- Minimum of leaf is just its static value
approxMinDecreasin (Node n []) = [n]
-- Minimum of node is minimized value of maximized sub nodes. 
approxMinDecreasin (Node n subTree) = approxMin (map approxMaxImproving subTree)

-- Efficient operator > for DownwardsApprox Value
optimizedGreater :: ApproxDown -> Int -> Bool
optimizedGreater [] val = False
optimizedGreater (first : rest) val = (first <= val) || optimizedGreater rest val

optimizedSmaller :: ApproxUp -> Int -> Bool
optimizedSmaller [] val = False
optimizedSmaller (first : rest) val = (first >= val) || optimizedSmaller rest val

betterThan :: Int -> [ApproxDown] -> ApproxUp
betterThan val [] = []
betterThan val (first : rest) =
    if first `optimizedGreater` val
        then firstApprox : betterThan firstApprox rest
        else betterThan val rest
    where firstApprox = minimum first

worseThan :: Int -> [ApproxUp] -> ApproxDown
worseThan val [] = []
worseThan val (first : rest) =
    if first `optimizedSmaller` val
        then firstApprox : worseThan firstApprox rest
        else worseThan val rest
    where  firstApprox = maximum first

-- Approximate a maximum in a list of approximated values (decreasing lists, truer value in tail).
approxMax :: [ApproxDown] -> ApproxUp
approxMax (first : rest) = firstApprox : betterThan firstApprox rest
    where firstApprox = minimum first

-- Approximate a minimum in a list of approximated values (increasing lists, truer value in tail).
approxMin :: [ApproxUp] -> ApproxDown
approxMin (first : rest) = firstApprox : worseThan firstApprox rest
    where firstApprox = maximum first