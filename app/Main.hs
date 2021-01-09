module Main (main) where
import MoveGen
import PrettyPrint
import ChessData
import Data.Tree
import Data.Ord
import Data.Foldable

-- Should give us a list with the 20 possible positions after first half-move.
testGenMoves = generateMoves defaultGameState

makeNode state = (state, generateMoves state) 

gameTree = unfoldTree makeNode 

boards = map board testGenMoves

prune :: Int -> Tree a -> Tree a
prune 0 (Node x ts) = Node x []
prune depth tree = prune (depth - 1) tree 

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

eval x = maximise $ staticEvalTree $ prune 5 $ gameTree x

-- TODO also allow to play black
makeAIMove :: GameState -> GameState 
makeAIMove gameState = case onMove gameState of
    White -> maximumBy (comparing eval) (generateMoves gameState)
    Black -> minimumBy (comparing eval) (generateMoves gameState)
    
parseSquare :: String -> Sqr
parseSquare s = (0, 0)

makeHumanMove :: GameState -> IO GameState
makeHumanMove state = do
    putStr (pprintBoard (board state))
    putStr "Enter start square, like e1:\n" 
    from <- getLine
    to <- getLine
    let move = (parseSquare from, parseSquare to)
    print move
    pure (transformGameState state move)

detectWin :: GameState -> Maybe Color
detectWin s = Nothing

playGame :: GameState -> IO Color
playGame state = do
    gameState <- if onMove state == Black
        then pure (makeAIMove state)
        else makeHumanMove state
    case detectWin gameState of
        Nothing -> playGame gameState
        Just color -> pure color

main = do 
    putStrLn "Let's play chesss, you get white!\n"
    winner <- playGame defaultGameState 
    putStr $ show winner ++ "wins !\n"