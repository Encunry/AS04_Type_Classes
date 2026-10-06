{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Test.Hspec ( hspec, describe, it, shouldBe, shouldSatisfy, Spec )
import Lang.Imp
import qualified Map.TreeMap as M

evalSpec :: Spec
evalSpec =
  describe "eval" $ do
    it "eval (AConst 1) empty == 1" $
      eval (AConst 1) emptyState `shouldBe` (1 :: Integer)

    it "eval (AVar \"x\") [x=5] == 5" $
      eval (AVar "x") (M.fromList [("x", 5)]) `shouldBe` (5 :: Integer)

    it "eval (APlus 2 3) == 5" $
      eval (APlus (AConst 2) (AConst 3)) emptyState `shouldBe` (5 :: Integer)

    it "eval (AMinus 2 5) == -3" $
      eval (AMinus (AConst 2) (AConst 5)) emptyState `shouldBe` (-3 :: Integer)

    it "eval (AMul 4 6) == 24" $
      eval (AMul (AConst 4) (AConst 6)) emptyState `shouldBe` (24 :: Integer)

    it "eval (ADiv 7 2) == 3" $
      eval (ADiv (AConst 7) (AConst 2)) emptyState `shouldBe` (3 :: Integer)

    it "eval (AMod 7 2) == 1" $
      eval (AMod (AConst 7) (AConst 2)) emptyState `shouldBe` (1 :: Integer)

    it "verschachtelt: (x + 2) * 3 mit x=4 == 18" $
      eval (AMul (APlus (AVar "x") (AConst 2)) (AConst 3))
           (M.fromList [("x", 4)])
        `shouldBe` (18 :: Integer)

    it "eval (REq 3 3) == True" $
      eval (REq (AConst 3) (AConst 3)) emptyState `shouldBe` True

    it "eval (REq 3 4) == False" $
      eval (REq (AConst 3) (AConst 4)) emptyState `shouldBe` False

    it "eval (RLt 3 4) == True" $
      eval (RLt (AConst 3) (AConst 4)) emptyState `shouldBe` True

    it "eval (RLt 4 4) == False" $
      eval (RLt (AConst 4) (AConst 4)) emptyState `shouldBe` False

    it "eval (BNot (REq 1 2)) == True" $
      eval (BNot (REq (AConst 1) (AConst 2))) emptyState `shouldBe` True

    it "eval (BAnd True False) == False" $
      eval (BAnd (RLt (AConst 1) (AConst 2)) (REq (AConst 1) (AConst 2)))
           emptyState
        `shouldBe` False

    it "eval (BAnd True True) == True" $
      eval (BAnd (RLt (AConst 1) (AConst 2)) (BNot (REq (AConst 1) (AConst 2))))
           emptyState
        `shouldBe` True

execSpec :: Spec
execSpec =
  describe "exec" $ do
    it "exec CSkip emptyState == emptyState" $
      exec CSkip emptyState `shouldBe` emptyState

    it "M.toList (exec (CAssign \"a\" (AConst 2)) emptyState) == [(\"a\",2)]" $
      M.toList (exec (CAssign "a" (AConst 2)) emptyState) `shouldBe` [("a",2)]

    it "CAssign überschreibt eine bestehende Variable" $
      M.lookup "a" (exec (CAssign "a" (AConst 9)) (M.fromList [("a", 1)]))
        `shouldBe` Just 9

    it "CAssign liest den alten Zustand: x := x + 1" $
      M.lookup "x" (exec (CAssign "x" (APlus (AVar "x") (AConst 1)))
                         (M.fromList [("x", 4)]))
        `shouldBe` Just 5

    it "CSeq [] ändert nichts" $
      exec (CSeq []) (M.fromList [("a", 1)]) `shouldBe` M.fromList [("a", 1)]

    it "CSeq führt von links nach rechts aus: a := 1; b := a + 1" $
      let st = exec (CSeq [ CAssign "a" (AConst 1)
                          , CAssign "b" (APlus (AVar "a") (AConst 1)) ])
                    emptyState
      in (M.lookup "a" st, M.lookup "b" st) `shouldBe` (Just 1, Just 2)

    it "CSeq: spätere Zuweisung gewinnt: a := 1; a := 2" $
      M.lookup "a" (exec (CSeq [ CAssign "a" (AConst 1)
                               , CAssign "a" (AConst 2) ])
                         emptyState)
        `shouldBe` Just 2

    it "CIfThEl nimmt den then-Zweig bei wahrer Bedingung" $
      M.lookup "r" (exec (CIfThEl (RLt (AConst 1) (AConst 2))
                                  (CAssign "r" (AConst 10))
                                  (CAssign "r" (AConst 20)))
                         emptyState)
        `shouldBe` Just 10

    it "CIfThEl nimmt den else-Zweig bei falscher Bedingung" $
      M.lookup "r" (exec (CIfThEl (RLt (AConst 2) (AConst 1))
                                  (CAssign "r" (AConst 10))
                                  (CAssign "r" (AConst 20)))
                         emptyState)
        `shouldBe` Just 20

    it "CWhile führt den Rumpf nicht aus, wenn die Bedingung falsch ist" $
      exec (CWhile (RLt (AConst 2) (AConst 1)) (CAssign "a" (AConst 1)))
           emptyState
        `shouldBe` emptyState

    it "CWhile zählt hoch: while i < 5 do i := i + 1" $
      M.lookup "i" (exec (CWhile (RLt (AVar "i") (AConst 5))
                                 (CAssign "i" (APlus (AVar "i") (AConst 1))))
                         (M.fromList [("i", 0)]))
        `shouldBe` Just 5

    it "CWhile summiert 1..4: s == 10" $
      M.lookup "s" (exec (CSeq
                     [ CAssign "i" (AConst 1)
                     , CAssign "s" (AConst 0)
                     , CWhile (RLt (AVar "i") (AConst 5)) (CSeq
                         [ CAssign "s" (APlus (AVar "s") (AVar "i"))
                         , CAssign "i" (APlus (AVar "i") (AConst 1))
                         ])
                     ])
                     emptyState)
        `shouldBe` Just 10


{-
Factorial:

z := x
y := 1
while (! (z = 0))
  y := y * z
  z := z - 1
-}
fact :: Cmd
fact = 
  CSeq [
    CAssign "z" (AVar "x"),
    CAssign "y" (AConst 1),
    CWhile (BNot (REq (AVar "z") (AConst 0))) (CSeq [
      CAssign "y" (AMul (AVar "y") (AVar "z")),
      CAssign "z" (AMinus (AVar "z") (AConst 1))
    ])
  ]

factSpec :: Spec
factSpec = describe "fact" $ do 
    -- fact [x=5] ~> y=120
    it "M.lookup \"y\" (exec fact (M.fromList [(\"x\", 5)])) == Just 120" $
      M.lookup "y" (exec fact (M.fromList [("x", 5)])) `shouldBe` Just 120 
    

{-
Quotient remainder:

q := 0
r := m
while (n < 2)
  q := q + 1
  r := r - n
-}
intDiv :: Cmd
intDiv = 
  CSeq [
    CAssign "q" (AConst 0),
    CAssign "r" (AVar "m"),
    CWhile (RLt (AVar "n") (AVar "r")) (CSeq [
        CAssign "q" (APlus (AVar "q") (AConst 1)),
        CAssign "r" (AMinus (AVar "r") (AVar "n"))
    ])
  ]

intDivSpec :: Spec
intDivSpec = describe "intDiv" $ do 
    -- intDiv 5 2 == q_uotient=2 r_emainder=1
    it "exec intDiv (M.fromList [(\"m\", 5), (\"n\", 2)]) ~> \"q\" -> 2, \"r\" -> 1" $
      exec intDiv (M.fromList [("m", 5), ("n", 2)]) `shouldSatisfy` 
        (\s -> M.lookup "q" s == Just 2 && M.lookup "r" s == Just 1)


{-
function gcd(a, b)
    while b ≠ 0
        t := b
        b := a mod b
        a := t
    return a
-}
gcd' :: Cmd
gcd' = CSeq [
  CWhile (BNot (REq (AVar "b") (AConst 0))) (CSeq [
        CAssign "t" (AVar "b"),
        CAssign "b" (AMod (AVar "a") (AVar "b")),
        CAssign "a" (AVar "t")
    ])
  ]


gcdSpec :: Spec
gcdSpec = describe "intDiv" $ do 
    -- intDiv 5 2 == q_uotient=2 r_emainder=1
    it "M.lookup \"a\" (exec gcd' (M.fromList [(\"a\", 252), (\"b\", 105)])) == Just 21" $
      M.lookup "a" (exec gcd' (M.fromList [("a", 252), ("b", 105)])) `shouldBe` Just 21 
    




main :: IO ()
main = hspec $ do
  describe "1. Evaluation" $ do
    evalSpec
  describe "2. Execution" $ do
    execSpec
  describe "3. Example programs" $ do
    factSpec
    intDivSpec
    gcdSpec
   
    