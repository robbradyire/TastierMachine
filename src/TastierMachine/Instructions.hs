module TastierMachine.Instructions where
import Data.Int (Int8, Int16)
import Data.List (elem)

data Instruction = Add
                 | Sub
                 | Mul
                 | Div
                 | Equ
                 | Neq
                 | Lss
                 | Gtr
                 | Geq
                 | Leq
                 | Neg
                 | Load
                 | Sto
                 | Call
                 | LoadG
                 | StoG
                 | Const
                 | Enter
                 | Jmp
                 | FJmp
                 | Ret
                 | Leave
                 | Read
                 | Write
                 | WriteCh
                 | Halt
                 | Dup
                 | Nop
                 | LStack
                 | LStackG
                 | StoSt
                 | StoStG
                 | Pop
                 deriving (Eq, Ord, Show, Enum)

data InstructionWord = Nullary Instruction
                     | Unary Instruction Int16
                     | Binary Instruction Int16 Int16
                     deriving (Eq, Show)

arguments :: Instruction -> Int
arguments i =
  if i `elem` [Load, Sto, Call] then 2
  else if i `elem` [LoadG, StoG, Const, Enter, Jmp, FJmp, LStack, StoSt] then 1
  else 0
