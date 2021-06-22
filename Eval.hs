import Lab
import Examples

σ = \_ -> 0

evalAssignWithIfElse :: IO ()
evalAssignWithIfElse = eval assignWithIfElse σ

evalPrintUptoTen :: IO ()
evalPrintUptoTen = eval printUptoTen σ

evalGetUptoTen :: IO ()
evalGetUptoTen = eval getUptoTen σ

evalAssignAndRestore :: IO ()
evalAssignAndRestore = eval assignAndRestore σ

evalGetAndRestore :: IO ()
evalGetAndRestore = eval getAndRestore σ

evalGiveControlAfterFail :: IO ()
evalGiveControlAfterFail = eval giveControlAfterFail σ

evalWhileTruePrint :: IO ()
evalWhileTruePrint = eval whileTruePrint σ

evalDivideByZero :: IO ()
evalDivideByZero = eval divideByZero σ

evalg4ex5a :: IO ()
evalg4ex5a = eval g4ex5a σ

evalg4ex5b :: IO ()
evalg4ex5b = eval g4ex5b σ

evalg5ex3a :: IO ()
evalg5ex3a = eval g5ex3a σ

evalg5ex3b :: IO ()
evalg5ex3b = eval g5ex3b σ

evalg5ex2di :: IO ()
evalg5ex2di = eval (g5ex2di STrue) σ

evalg5ex2dii :: IO ()
evalg5ex2dii = eval (g5ex2dii STrue) σ

evalg5ex2ei :: IO ()
evalg5ex2ei = eval g5ex2ei σ

evalg5ex2eii :: IO ()
evalg5ex2eii = eval g5ex2eii σ
