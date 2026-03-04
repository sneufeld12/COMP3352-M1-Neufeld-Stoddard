module X86Passes.X86bPassesSpec (spec) where

import Test.Hspec
import X86Passes.X86bPasses
import CompilerPasses
import CPasses.C0 as C0
import X86Passes.X86b as X86b

spec :: Spec
spec = do
  describe "X86b Passes Tests:" $ do
    it "can translate C0 return on an int" $ do
      instructionSelection (C0.Program [("start", Return (Atm $ C0.Int 3))]) [] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (Imm 3) (Reg RAX),
             Jmp (Label "conclusion")])], []))

    it "can translate C0 return on a var" $ do
      instructionSelection (C0.Program [("start", Return (Atm $ C0.Var "x"))]) ["x"] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (X86b.Var "x") (Reg RAX),
             Jmp (Label "conclusion")])], ["x"]))

    it "can translate C0 return on a imm sub" $ do
      instructionSelection (C0.Program [("start", Return (Sub $ C0.Int 2))]) [] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (Imm 2) (Reg RAX),
             Negq (Reg RAX),
             Jmp (Label "conclusion")])], []))

    it "can translate C0 return on a imm add" $ do
      instructionSelection (C0.Program [("start", Return (Add (C0.Int 2) (C0.Int 3)))]) [] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (Imm 2) (Reg RAX),
             Addq (Imm 3) (Reg RAX),
             Jmp (Label "conclusion")])], []))

    it "can translate C0 an assign followed by return" $ do
      instructionSelection (C0.Program [("start", Seq (Assign "x" (Atm $ C0.Int 5)) (Return (Atm $ C0.Var "x")))]) ["x"] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (Imm 5) (X86b.Var "x"),
             Movq (X86b.Var "x") (Reg RAX),
             Jmp (Label "conclusion")])], ["x"]))

    it "can translate C0 multiple assigns followed by return" $ do
      instructionSelection (C0.Program [("start",
        Seq (Assign "x" (Atm $ C0.Int 5))
          (Seq (Assign "y" (Atm $ C0.Int 6))
            (Return (C0.Add (C0.Var "x") (C0.Var "y")))))]) ["x","y"] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (Imm 5) (X86b.Var "x"),
             Movq (Imm 6) (X86b.Var "y"),
             Movq (X86b.Var "x") (Reg RAX),
             Addq (X86b.Var "y") (Reg RAX),
             Jmp (Label "conclusion")])], ["x","y"]))

    -- STUDENT TEST 1 (Normal case): Negate a variable
    it "can translate C0 return on a var sub" $ do
      instructionSelection (C0.Program [("start", Return (Sub $ C0.Var "x"))]) ["x"] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Movq (X86b.Var "x") (Reg RAX),
             Negq (Reg RAX),
             Jmp (Label "conclusion")])], ["x"]))

    -- STUDENT TEST 2 (Edge case): Read expression assignment
    it "can translate C0 read assignment" $ do
      instructionSelection (C0.Program [("start", Seq (Assign "x" C0.Read) (Return (Atm $ C0.Var "x")))]) ["x"] `shouldBe`
        Right (SIPass (X86b.Program
          [(Label "start", Block
            [Callq (Label "read_int") 0,
             Movq (Reg RAX) (X86b.Var "x"),
             Movq (X86b.Var "x") (Reg RAX),
             Jmp (Label "conclusion")])], ["x"]))
