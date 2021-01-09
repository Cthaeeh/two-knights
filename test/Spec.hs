import Test.HUnit


testOne :: Test
testOne = TestCase $ assertEqual "Some Message" (Just 1) (Just 1) 

testTwo :: Test
testTwo = TestCase $ assertEqual "Other Message" (Just 1) Nothing


main :: IO Counts
main = runTestTT $ TestList [testOne, testTwo]
