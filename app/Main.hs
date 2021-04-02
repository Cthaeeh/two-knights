module Main (main) where
import PrettyPrint ( pprintBoard, pprintBoards)
import Data.Char ( ord, digitToInt)
import Control.Monad ( liftM2 )
import ChessData
    ( defaultGameState,
      Color(Black),
      GameState(board, onMove),
      Move,
      Location )
import Search ( makeAIMove )
import Data.Maybe ( isNothing )
import MoveGen ( transformGameState, generateMoves )


parseFile :: Char -> Maybe Int
parseFile c
    | c `elem` ['a'..'h' ] = Just (ord c - ord 'a')
    | otherwise = Nothing

parseRank :: Char -> Maybe Int
parseRank c
    | c `elem` ['1'..'8' ] = Just (digitToInt c - 1)
    | otherwise = Nothing

parseSquare :: String -> Maybe Location
parseSquare s = case length s of
    2 -> liftM2 (,) (parseFile (head s)) (parseRank (s !! 1))
    _ -> Nothing

parseMove :: String -> Maybe Move
parseMove s = case length s of
    4 -> liftM2 (,) (parseSquare (take 2 s)) (parseSquare (drop 2 s))
    _ -> Nothing

getMove :: IO Move
getMove = do
    str <- getLine
    let sq = parseMove str
    maybe
      (do putStrLn "invalid notation, try again."
          getMove)
      pure sq

isLegalMove :: GameState -> Move -> Bool
isLegalMove state move = transformGameState state move `elem` generateMoves state


makeHumanMove :: GameState -> IO GameState
makeHumanMove state = do
    putStr (pprintBoard (board state))
    putStr "Enter move (in long algebraic notation, e.g. e2e4):\n"
    move <- getMove -- TODO check move legality
    if isLegalMove state move
        then pure (transformGameState state move)
        else do putStrLn "That is not a legal move, try again."
                makeHumanMove state

detectWin :: GameState -> Maybe Color
detectWin s = Nothing --TODO

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

--e4 :: Sqr
--e4 = (4,3)
--e2 :: Sqr
--e2 = (4,1)
--e7 :: Sqr
--e7 = (4,6)
--e5 :: Sqr
--e5 = (4,4)
--
--e4s = transformGameState defaultGameState (e2,e4)
--main = putStrLn (pprintBoards (map board (generateMoves e4s)))