import Lab
import Test.HUnit
import Examples

σ = \_ -> 0

early_stopping = 100
vars = [[v] | v <- ['a'..'z']]

checkAllVars ω ω' = all (statesEq ω ω') vars

rollOut :: [Int] -> Ω -> Ω
rollOut []     ω = ω
rollOut (x:xs) ω = Out (x, rollOut xs ω)

statesEq :: Ω -> Ω -> Iden -> Bool
statesEq (Normal σ) (Normal σ')      v = σ v == σ' v
statesEq (Abort σ) (Abort σ')        v = σ v == σ' v
statesEq (Out (o, ω)) (Out (o', ω')) v = o == o' && statesEq ω ω' v
statesEq (In g) (In g')              v = all 
  (\n -> statesEq (g n) (g' n) v)
  [0..early_stopping]
statesEq _ _ _                         = False

testProperties = TestLabel "Properties" $ TestList [ ]
testSkip   = TestLabel "testSkip"   $ TestList [ ]
testFail   = TestLabel "testFail"   $ TestList [ ]
testIfElse = TestLabel "testIfElse" $ TestList [ testAssignWithIfElse ]
testOut    = TestLabel "testOut"    $ TestList [ testPrintUptoTen ]
testNewvar = TestLabel "testNewvar" $ TestList [ testAssignAndRestore ]

testAssignWithIfElse = TestCase $ do 
  assertBool "should run if clause" $ checkAllVars 
    (sem assignWithIfElse $ update σ "x" 10) (Normal $ update σ "x" 0)
  assertBool "should run else clause" $ checkAllVars 
    (sem assignWithIfElse $ update σ "x" 11) (Normal $ update σ "x" 1)

testPrintUptoTen = TestCase $ assertBool "should abort if outputs differs" $
  checkAllVars (sem printUptoTen σ) (rollOut [0..10] $ Normal $ update σ "x" 11)

testAssignAndRestore = TestCase $ assertBool "should assign and restore" $
  checkAllVars (sem assignAndRestore σ) (Out (10, Normal $ update σ "x" 3))

-- testFail
-- testAssign
-- testIfElse
-- testSeq
-- testCatch
-- testNewvar
-- testSOut
-- testSIn
-- testWhile

main = runTestTT $ TestList [ testIfElse, testOut, testNewvar ]