module CPasses.C0PassesSpec (spec) where

import Test.Hspec
import CompilerPasses
import CPasses.C0Passes
import CPasses.C0 as C0
import qualified NiPasses.N1 as N1

spec :: Spec
spec = do
  describe "Explicate Control Tests:" $ do
    it "can explicate control of simple ints" $ do
      explicateControl (N1.Program (N1.Int 5)) `shouldBe`
        Right (ECPass (C0.Program [("start", Return $ Atm $ C0.Int 5)]) [])

    it "can explicate control of simple vars" $ do
      explicateControl (N1.Program (N1.Var "x")) `shouldBe`
        Right (ECPass (C0.Program [("start", Return $ Atm $ C0.Var "x")]) [])

    it "can explicate control of negate var" $ do
      explicateControl (N1.Program (N1.Negate (N1.Var "x"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (Sub $ C0.Var "x"))]) [])

    it "can explicate control of negate int" $ do
      explicateControl (N1.Program (N1.Negate (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (Sub $ C0.Int 6))]) [])

    it "can explicate control of add int int" $ do
      explicateControl (N1.Program (N1.Add (N1.Int 5) (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (C0.Add (C0.Int 5) (C0.Int 6)))]) [])

    it "can explicate control of add int var" $ do
      explicateControl (N1.Program (N1.Add (N1.Int 5) (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (C0.Add (C0.Int 5) (C0.Var "y")))]) [])

    it "can explicate control of add var int" $ do
      explicateControl (N1.Program (N1.Add (N1.Var "x") (N1.Int 6))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (C0.Add (C0.Var "x") (C0.Int 6)))]) [])

    it "can explicate control of add var var" $ do
      explicateControl (N1.Program (N1.Add (N1.Var "x") (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Return (C0.Add (C0.Var "x") (C0.Var "y")))]) [])

    it "can explicate control of a simple let assignment of int" $ do
      explicateControl (N1.Program (N1.Let "x" (N1.Int 5) (N1.Var "x"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x" (Atm (C0.Int 5))) (Return (Atm (C0.Var "x"))))]) ["x"])

    it "can explicate control of a simple let assignment with add as body" $ do
      explicateControl (N1.Program (N1.Let "x" (N1.Int 5) (N1.Add (N1.Var "x") (N1.Int 6)))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x" (Atm (C0.Int 5))) (Return (C0.Add (C0.Var "x") (C0.Int 6))))]) ["x"])

    it "can explicate control of a simple let assignment with sub as body" $ do
      explicateControl (N1.Program (N1.Let "x" (N1.Int 5) (N1.Negate (N1.Var "x")))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x" (Atm (C0.Int 5))) (Return (Sub (C0.Var "x"))))]) ["x"])

    it "can explicate control of a nested let assignment" $ do
      explicateControl (N1.Program (N1.Let "x" (N1.Int 5) (N1.Let "y" (N1.Int 6) (N1.Add (N1.Var "x") (N1.Var "y"))))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x" (Atm (C0.Int 5)))
                                               (Seq (Assign "y" (Atm (C0.Int 6)))
                                                  (Return (C0.Add (C0.Var "x") (C0.Var "y")))))]) ["x", "y"])

    it "can explicate control of a nested lets with add as body" $ do
      explicateControl (N1.Program (N1.Let "y"
                                     (N1.Let "x1" (N1.Int 20)
                                       (N1.Let "x2" (N1.Int 22)
                                         (N1.Add (N1.Var "x1") (N1.Var "x2")))) (N1.Var "y"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x1" (Atm (C0.Int 20)))
                                              (Seq (Assign "x2" (Atm (C0.Int 22)))
                                                (Seq (Assign "y" (C0.Add (C0.Var "x1") (C0.Var "x2")))
                                                  (Return (Atm (C0.Var "y"))))))]) ["x1","x2","y"])

    -- STUDENT TEST 1 (Normal case): Read expression in let binding
    it "can explicate control of a let with read as binding" $ do
      explicateControl (N1.Program (N1.Let "x" N1.Read (N1.Var "x"))) `shouldBe`
        Right (ECPass (C0.Program [("start", Seq (Assign "x" C0.Read) (Return (Atm (C0.Var "x"))))]) ["x"])

    -- STUDENT TEST 2 (Edge case): Simple Read at top level
    it "can explicate control of a simple read expression" $ do
      explicateControl (N1.Program N1.Read) `shouldBe`
        Right (ECPass (C0.Program [("start", Return C0.Read)]) [])
