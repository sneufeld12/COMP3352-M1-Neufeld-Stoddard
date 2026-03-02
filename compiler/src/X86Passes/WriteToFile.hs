-- Code generation for a tiny subset of x86-64 assembly
module X86Passes.WriteToFile where
import Data.Int (Int64)

-- ===== x86a Abstract Syntax =====

data Reg
  = RSP
  | RBP | RAX | RBX | RCX | RDX
  | RSI | RDI | R8  | R9  | R10
  | R11 | R12 | R13 | R14 | R15
  deriving (Eq)

data SrcArg
  = SrcImm Int64
  | SrcReg Reg
  | SrcMem Reg Int64
  deriving (Eq)

data DstArg
  = DstReg Reg
  | DstMem Reg Int64
  deriving (Eq)

newtype Label = Label String
  deriving (Eq)

data Instr
  = Addq SrcArg DstArg
  | Subq SrcArg DstArg
  | Movq SrcArg DstArg
  | Negq DstArg
  | Callq Label Integer -- arity included in AST
  | Retq
  | Pushq SrcArg
  | Popq SrcArg
  | Jmp Label
  deriving (Eq)

data Block a = Block
  { blockInfo :: a
  , instrs    :: [Instr]
  }
  deriving (Eq)

data X860 a = Program
  { progInfo :: a
  , cfg      :: [(Label, Block a)]
  }
  deriving (Eq)

--each constructor maps to one exact assembly form
instance Show Reg where
    show RAX = "%rax"
    -- one case per register
    show RBX = "%rbx"
    show RCX = "%rcx"
    show RSP = "%rsp"
    show RBP = "%rbp"
    show RDX = "%rdx"
    show RSI = "%rsi"
    show RDI = "%rdi"
    show R8  = "%r8"
    show R9  = "%r9"
    show R10 = "%r10"
    show R11 = "%r11"
    show R12 = "%r12"
    show R13 = "%r13"
    show R14 = "%r14"
    show R15 = "%r15"

--operands must follow AT&T rules
instance Show SrcArg where
    show (SrcImm n)      = "$" ++ show n
    show (SrcReg r)      = show r
    show (SrcMem r off)  =
        show off ++ "(" ++ show r ++ ")"

instance Show DstArg where
    show (DstReg r)      = show r
    show (DstMem r off)  =
        show off ++ "(" ++ show r ++ ")"

instance Show Label where
    show (Label s) = s

-- Encode Instruction pattern
instance Show Instr where
  show (Addq s d) = "addq " ++ show s ++ ", " ++ show d
  show (Subq s d) = "subq " ++ show s ++ ", " ++ show d
  show (Movq s d) = "movq " ++ show s ++ ", " ++ show d
  show (Negq d)   = "negq " ++ show d
  show (Callq (Label l) _) = "callq " ++ show l
  show Retq       = "retq"
  show (Pushq s)  = "pushq " ++ show s
  show (Popq s)   = "popq " ++ show s
  show (Jmp (Label l)) = "jmp " ++ show l

instance Show a => Show (Block a) where
  show (Block _ is) =
    unlines [ "\t" ++ show i | i <- is ]

instance Show a => Show (X860 a) where
  show (Program _ cfg) =
    unlines $
      [ ".text" ] ++
      concatMap showBlock cfg
    where
      showBlock (lbl, blk) =
        [ ".globl " ++ show lbl
        , show lbl ++ ":"
        , show blk
        ]


-- ===== Sample Program =====

sampleBlock :: Block ()
sampleBlock =
  Block ()
    [ Movq (SrcImm 5)  (DstReg RAX)
    , Addq (SrcImm 37) (DstReg RAX)
    , Retq
    ]

sampleProg :: X860 ()
sampleProg =
  Program () [(Label "main", sampleBlock)]

-- ===== Main =====

main :: IO ()
main = do
  print(sampleProg)  -- prints the assembly program
  writeFile "test2.s" (show sampleProg)


