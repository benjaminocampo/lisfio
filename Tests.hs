import Lab
import Test.HUnit
import Examples

σ = \_ -> 0

early_stopping = 100
vars = [[v] | v <- ['a'..'z']]

checkStates ω ω' = all (statesEq ω ω') vars

rollOut :: [Int] -> Ω -> Ω
rollOut []     ω = ω
rollOut (n:ns) ω = Out (n, rollOut ns ω)

statesEq :: Ω -> Ω -> Iden -> Bool
statesEq (Normal σ) (Normal σ')      v = σ v == σ' v
statesEq (Abort σ) (Abort σ')        v = σ v == σ' v
statesEq (Out (o, ω)) (Out (o', ω')) v = o == o' && statesEq ω ω' v
statesEq (In g) (In g')              v = all 
  (\n -> statesEq (g n) (g' n) v)
  [0..early_stopping]
statesEq _ _ _                         = False

testProperties = TestLabel "Properties" $ TestList 
  [ testDivideByZero
  , testAssignAndRestore
  , testGiveControlAfterFail
  , testPrintUptoTen 
  , testGetAndRestore
  ]

testExercises  = TestLabel "Exercises" $ TestList
  [ testg4ex5a
  , testg4ex5b
  , testg5ex3a
  , testg5ex3b
  , testg5ex2d
  , testg5ex2e
  ]

testDivideByZero = TestCase $
  assertEqual "should return default value for divition"
    (sem divideByZero σ)
    0

testAssignWithIfElse = TestCase $ do 
  assertBool "should run if clause" $
    checkStates 
      (sem assignWithIfElse $ update σ "x" 10)
      (Normal $ update σ "x" 0)
  assertBool "should run else clause" $
    checkStates 
      (sem assignWithIfElse $ update σ "x" 11)
      (Normal $ update σ "x" 1)

testPrintUptoTen = TestCase $
  assertBool "should fail if outputs differs" $
    checkStates
      (sem printUptoTen σ)
      (rollOut [0..10] $ Normal $ update σ "x" 11)

testAssignAndRestore = TestCase $
  assertBool "should assign and restore" $
    checkStates
      (sem assignAndRestore σ)
      (Out (10, Normal $ update σ "x" 3))

testGiveControlAfterFail = TestCase $
  assertBool "should give control to assign" $
    checkStates
      (sem giveControlAfterFail σ)
      (Normal $ update σ "x" 2)

testGetAndRestore = TestCase $
  assertBool "should input before newvar and output values after restoration" $
    checkStates
      (sem getAndRestore σ)
      (In $ \n -> Out(10, Out(n, Normal $ update σ "x" n)))

testg4ex5a = TestCase $ do
  assertBool "should fail if states differs: g4ex5a" $
    checkStates
      (sem g4ex5a $ update σ "x" 1)
      (Normal $ update σ "x" 2)
  assertBool "should fail if states differs: g4ex5a" $
    checkStates
      (sem g4ex5a $ update σ "x" 3)
      (Normal $ update σ "x" 3)

testg4ex5b = TestCase $ do
  assertBool "should fail if states differs: g4ex5b" $
    checkStates
      (sem g4ex5b $ update σ "x" 1)
      (Normal $ update σ "x" 2)
  assertBool "should fail if states differs: g4ex5b" $
    checkStates
      (sem g4ex5b $ update σ "x" 3)
      (Normal $ update σ "x" 3)

testg5ex3a = TestCase $
  assertBool "should fail if states differs: g5ex3a" $
    checkStates
      (sem g5ex3a $ update (update σ "x" 2) "y" 1)
      (Normal $ update (update σ "x" 1) "y" 3)

testg5ex3b = TestCase $
  assertBool "should fail if states differs: g5ex3b" $
    checkStates
      (sem g5ex3b $ update (update σ "x" 2) "y" 1)
      (Normal $ update σ "y" 4)

testg5ex2d = TestCase $ do
  assertBool "should fail if commands aren't equivalent: g5ex2d" $
    checkStates
      (sem (g5ex2di STrue) σ)
      (sem (g5ex2dii STrue) σ)
  assertBool "should fail if commands aren't equivalent: g5ex2d" $
    checkStates
      (sem (g5ex2di SFalse) σ)
      (sem (g5ex2di SFalse) σ)

testg5ex2e = TestCase $
  assertBool "should fail if commands aren't equivalent: g5ex2e" $
    checkStates
      (sem g5ex2ei σ)
      (sem g5ex2eii σ)

main = runTestTT $ TestList [ testProperties, testExercises ]