module X86Passes.X86b where

import Data.Int
import Data.Foldable
import Data.Hashable

data Reg = RSP
  | RBP | RAX | RBX | RCX | RDX
  | RSI | RDI | R8 | R9 | R10
  | R11 | R12 | R13 | R14 | R15 deriving Eq

instance Show Reg where
  show RSP = "%rsp"
  show RBP = "%rbp"
  show RAX = "%rax"
  show RBX = "%rbx"
  show RCX = "%rcx"
  show RDX = "%rdx"
  show RSI = "%rsi"
  show RDI = "%rdi"
  show R8 = "%r8"
  show R9 = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

regToOrd :: Reg -> Integer
regToOrd reg =
  case reg of
    RSP -> 0; RBP -> 1; RAX -> 2; RBX -> 3; RCX -> 4; RDX -> 5
    RSI -> 6; RDI -> 7; R8 -> 8; R9 -> 9; R10 -> 10; R11 -> 11
    R12 -> 12; R13 -> 13; R14 -> 14; R15 -> 15

instance Hashable Reg where
  hashWithSalt salt reg = salt + fromInteger (regToOrd reg)

instance Ord Reg where
  compare r1 r2 = compare (regToOrd r1) (regToOrd r2)

data Arg = Imm Int64 | Reg Reg | Mem Reg Int64 | Var String
    deriving Eq

instance Show Arg where
    show (Imm v) = "$" ++ show v
    show (Reg reg) = show reg
    show (Mem reg offset) = show offset ++ "(" ++ show reg ++ ")"
    show (Var str) = str

argToOrd :: Arg -> Integer
argToOrd arg =
  case arg of
    Imm v -> toInteger v
    Reg r -> regToOrd r
    Mem r v -> regToOrd r + toInteger v
    Var _ -> 0

instance Ord Arg where
  compare a1 a2 =
    case a1 of
      Imm _ -> case a2 of
          Var _ -> LT; Mem _ _ -> GT; Reg _ -> GT
          _ -> compare (argToOrd a1) (argToOrd a2)
      Reg _ -> case a2 of
          Var _ -> LT; Mem _ _ -> LT; Imm _ -> LT
          _ -> compare (argToOrd a1) (argToOrd a2)
      Mem reg1 offset1 -> case a2 of
          Var _ -> LT; Imm _ -> LT; Reg _ -> GT
          Mem reg2 offset2 -> case compare reg1 reg2 of
            GT -> GT; LT -> LT; EQ -> compare offset1 offset2
      Var x -> case a2 of
          Var y -> compare x y; _ -> GT

instance Hashable Arg where
  hashWithSalt salt (Reg reg) = hashWithSalt salt reg
  hashWithSalt salt (Mem reg offset) = hashWithSalt salt reg + fromIntegral offset + 16
  hashWithSalt salt (Imm v) = hashWithSalt salt v + 16
  hashWithSalt salt (Var s) = hashWithSalt salt s + 16

newtype Label = Label String deriving Eq

instance Show Label where
    show (Label lbl) = lbl

data Instr =
    Addq Arg Arg
  | Subq Arg Arg
  | Movq Arg Arg
  | Negq Arg
  | Callq Label Integer
  | Retq
  | Pushq Arg
  | Popq Arg
  | Jmp Label deriving Eq

instance Show Instr where
  show (Addq src dst) = "addq   " ++ show src ++ ", " ++ show dst
  show (Subq src dst) = "subq   " ++ show src ++ ", " ++ show dst
  show (Movq src dst) = "movq   " ++ show src ++ ", " ++ show dst
  show (Negq dst) = "negq   " ++ show dst
  show (Callq lbl _) = "callq  " ++ show lbl
  show Retq = "retq"
  show (Pushq src) = "pushq  " ++ show src
  show (Popq src) = "popq   " ++ show src
  show (Jmp lbl) = "jmp " ++ show lbl

newtype Block = Block [Instr] deriving (Eq)
instance Show Block where
  show (Block ins)=
    "        " ++ foldl' (\acc x -> acc ++ show x ++ "\n        ") "" ins

newtype X86b = Program [(Label, Block)] deriving (Eq)
instance Show X86b where
  show (Program []) = ".global _main\n\nretq"
  show (Program blocks) =
    ".global _main\n\n" ++
    foldl' (\acc (lbl, blk') ->
        acc ++ show lbl ++ ":\n" ++ show blk' ++ "\n") "" blocks
