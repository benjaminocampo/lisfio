module Examples where
import Lab

concatSeq :: [Expr Ω] -> Expr Ω
concatSeq = foldr Seq Skip

x    = Var "x"
y    = Var "y"
cero = Const 0
one  = Const 1
two  = Const 2
ten  = Const 10

{-
  if x == 10 then
    x := 0
  else
    x := 1
  od
-}
assignWithIfElse :: Expr Ω
assignWithIfElse = IfElse (Eq x ten)
  (Assign "x" $ Const 0)
  (Assign "x" $ Const 1)

{- 
  while x <= 10 do
    !x ;
    x := x + 1
  od
-}
printUptoTen :: Expr Ω
printUptoTen = While (Lte x ten) $ concatSeq
  [ (SOut $ x)
  , (Assign "x" $ Plus x one)
  ]

{-
  while y < 10 do
    ?x ;
    !x ;
    !y ;
    y := y + 1
  od
-}
getUptoTen :: Expr Ω
getUptoTen = While (Lt y ten) $ concatSeq 
  [ (SIn "x")
  , (SOut $ x)
  , (SOut $ y)
  , (Assign "y" $ Plus y one)
  ]

{-
  x := 3 ;
  newvar x := 10 in !x end ;
-}
assignAndRestore :: Expr Ω
assignAndRestore = concatSeq
  [ (Assign "x" $ Const 3)
  , (Newvar "x" ten $ SOut x)
  ]

{-
  ?x ;
  newvar x := 10 in !x end ;
  !x
-}
getAndRestore :: Expr Ω
getAndRestore = concatSeq
  [ (SIn "x")
  , (Newvar "x" ten $ SOut x)
  , (SOut $ x)
  ]
{-
  catch x := 1; Fail with x := 0
-}
giveControlToFail :: Expr Ω
giveControlToFail = Catch
  (Seq (Assign "x" one) Fail)
  (Assign "x" cero)

{-
  while True do !x
-}
whileTruePrint :: Expr Ω
whileTruePrint STrue $ SOut x

{-
  while x < 2 do
    if x < 0 then x := 0
    else x:= x + 1
  od
-}
g4ex5a :: Expr Ω
g4ex5a = While (Lt x two) $
  IfElse (Lt x cero)
    (Assign "x" cero)
    (Assign "x" $ Plus x one)

{-
  while x < 2 do
    if y = 0 then x := x + 1
    else skip
  od
-}
g4ex5b :: Expr Ω
g4ex5b = While (Lt x two) $
  IfElse (Eq y cero) $
    (Assign "x" $ Plus x one)
    Skip

{-
  y:= x + y;
  if y > 0 then x := x - 1
  else skip
-}
g5ex3a :: Expr Ω
g5ex3a = Seq
  (Assign "y" $ Plus x y)
  (IfElse (Gt y cero) (Assign "x" $ Minus x one) Skip)

{-
  while x > 0 do
    y:= x + y;
    if y > 0 then x := x - 1
    else skip
-}
g5ex3b :: Expr Ω
g5ex3b = While (Gt x 0) g5ex3a

{-
  newvar x:= 2 in
    while x > 0 do
      y:= x + y;
      if y > 0 then x := x -1
      else skip
-}
g5ex4 :: Expr Ω
g5ex4 = Newvar "x" two $
  While (Gt x cero) $ Seq
    (Assign "y" $ Plus x y)
    (IfElse (Gt y cero) (Assign "x" $ Minus x one) Skip)


{-
  x := 0;
  catch
    while x < 1 do fail od
  with
    x:= 0
-}
g5ex2ei :: Expr Ω
g5ex2ei = Seq
  (Assign "x" cero)
  (Catch (While (Lt x one) Fail) (Assign "x" cero))

{-
  x := 0;
  while x < 1 do
    catch fail with x := 1
-}
g5ex2ei :: Expr Ω
g5ex2ei = Seq
  (Assign "x" cero)
  (While (Lt x one) (Catch Fail $ Assign "x" one))
