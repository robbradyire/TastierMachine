{-# LANGUAGE DoAndIfThenElse #-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import qualified Data.Int as I
import qualified Data.Bits as B
import Data.Array ((//), (!), Array)
import qualified Data.Word as W
import qualified Control.Monad.State as S

data Machine = Machine { rpc :: I.Int16,  -- mutable register containing the address of the next instruction to execute
                         rtp :: I.Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: I.Int16,  -- mutable register containing the address of the base of the stack
                         rcc :: I.Int16,  -- mutable register containing condition codes after operations

                         imem :: (Array I.Int16 Instructions.InstructionWord), -- instruction memory
                         dmem :: (Array I.Int16 I.Int16), -- data memory
                         smem :: (Array I.Int16 I.Int16)  -- stack memory
                       }
                       deriving (Show)

run :: S.State Machine ()
run = do
  machine@(Machine rpc rtp rbp rcc imem dmem smem) <- S.get
  let instructionWord = imem ! rpc

  case instructionWord of
    Instructions.Nullary i ->
      case i of
        Instructions.Halt ->
          return ()

        Instructions.Add -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = a + b
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Sub    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = a - b
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Mul    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = a * b
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Div    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = a `div` b
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Equ    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = fromIntegral $ fromEnum (a == b)
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Lss    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = fromIntegral $ fromEnum (a < b)
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Gtr    -> do
          let a = smem ! rtp
          let b = smem ! (rtp-1)
          let result = fromIntegral $ fromEnum (a > b)
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-1, result)]) }

        Instructions.Neg    -> do
          let a = smem ! rtp
          let result = B.complement a
          S.put $ machine { rpc = rpc + 1, smem = (smem // [(rtp, result)]) }

        Instructions.Ret    -> do
          S.put $ machine { rpc = (smem ! rtp-1), rtp = rtp - 1 }

        Instructions.Leave  -> do
          S.put $ machine { rtp = rbp+1, rbp = (smem ! rbp+3) }

        Instructions.Read   -> S.put machine
        Instructions.Write  -> S.put machine

    Instructions.Unary i a ->
      case i of
        Instructions.StoG   -> do
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, dmem = (dmem // [(a, (smem ! rtp-1))]) }

        Instructions.LoadG  -> do
          S.put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (dmem ! a))]) }

        Instructions.Const  -> do
          S.put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, a)]) }

        Instructions.Enter  -> do
          let lexicalLevelDelta = (smem ! rtp-1)
          if lexicalLevelDelta == 0 then -- calling a procedure at the same level as the current one
            S.put $ machine { rtp = rtp+a+2, rbp = rtp-2, smem = (smem // [(rtp, (smem ! rbp+2)), (rtp+1, rbp)]) }
          else do -- calling a procedure at a different level than the current one
            let calleeStaticLinkFieldAddr = followChain 1 lexicalLevelDelta rbp smem
            S.put $ machine { rtp = rtp+a+2, rbp = rtp-2, smem = (smem // [(rtp, calleeStaticLinkFieldAddr), (rtp+1, rbp)]) }

        Instructions.Jmp  -> do
          S.put $ machine { rpc = a }

        Instructions.FJmp -> do
          let jump = smem ! rtp
          if jump /= 0 then
            S.put $ machine { rtp = rtp - 1, rpc = a }
          else
            S.put $ machine { rtp = rtp - 1, rpc = rpc + 1 }

    Instructions.Binary i a b ->
      case i of
        Instructions.Load   -> do --Load gets a variable from a calling frame onto the top of the stack
          let loadAddr = (followChain 0 a rbp smem) + b + 4
          S.put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (smem ! loadAddr))]) }

        Instructions.Sto    -> do
          let storeAddr = (followChain 0 a rbp smem) + b + 4
          S.put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(storeAddr, (smem ! rtp-1))]) }

        Instructions.Call   -> do
          S.put $ machine { rpc = b, rtp = rtp + 2, smem = (smem // [(rtp, rpc+3), (rtp+1, a)]) }

--followChain follows the static link chain to find the address of the base of the stack frame i levels down

followChain :: I.Int16 -> I.Int16 -> I.Int16 -> (Array I.Int16 I.Int16) -> I.Int16
followChain limit n rbp smem =
  if n > limit then
    followChain limit (n-1) (smem ! rbp) smem
  else rbp
