module TastierMachine.Instructions where

data Instruction = Add | Sub | Mul | Div | Equ | Lss | Gtr |
                   Neg | Load | LoadG | Sto | StoG | Const |
                   Call | Ret | Enter | Leave | Jmp | FJmp |
                   Read | Write | Halt
                   deriving (Eq, Ord, Show, Enum)
