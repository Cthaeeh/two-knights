import Test.HUnit
import MoveGen

import ChessData

e4 :: Sqr
e4 = (4,3)
e2 :: Sqr
e2 = (4,1)
e7 :: Sqr
e7 = (4,6)
e5 :: Sqr
e5 = (4,4)

testOne :: Test
testOne = TestCase $ assertEqual "Some Message" (Just 1) (Just 1) 

moveGenDefaultPos :: Test
moveGenDefaultPos = 
    TestCase $ assertEqual "Moves from the default Position." 20 (length (generateMoves defaultGameState))

moveGenAfterE4E5 :: Test
moveGenAfterE4E5 = 
    TestCase $ assertEqual "Moves after 1. e4 (Black to move)" 29 (length (generateMoves e4e5))
    where e4e5 = transformGameState (transformGameState defaultGameState (e2,e4)) (e7, e5)

moveGenAfterE4 :: Test
moveGenAfterE4 = 
    TestCase $ assertEqual "Moves after 1. e4 e5 (White to move)" 20 (length (generateMoves e4e5))
    where e4e5 = transformGameState defaultGameState (e2,e4)


main :: IO Counts
main = runTestTT $ TestList [testOne, moveGenDefaultPos, moveGenAfterE4, moveGenAfterE4E5]
