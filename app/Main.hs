module Main (main) where
import PrettyPrint ( pprintBoard, pprintBoards)
import Data.Char ( ord, digitToInt)
import Control.Monad
import ChessData
import Search
import Data.Maybe
import MoveGen

    
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

getSquare :: IO Location
getSquare = do
    putStr "Enter square, like e1:\n" 
    str <- getLine
    let sq = parseSquare str
    maybe getSquare pure sq

makeHumanMove :: GameState -> IO GameState
makeHumanMove state = do
    putStr (pprintBoard (board state))
    putStr "Enter move:\n" 
    move <- liftM2 (,) getSquare getSquare
    print move -- TODO check move legality
    pure (transformGameState state move)

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