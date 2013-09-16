{-# LANGUAGE DoAndIfThenElse #-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import qualified Data.Int as I
import qualified Data.Bits as B
import Data.Array ((//), (!), Array)
import qualified Data.Word as W
import qualified Control.Monad.State as S
--import System.IO
--import System.IO.Unsafe

data Machine = Machine { rpc :: I.Int16,  -- mutable register containing the address of the next instruction to execute
                         rtp :: I.Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: I.Int16,  -- mutable register containing the address of the base of the stack
                         rcc :: I.Int16,  -- mutable register containing condition codes after operations

                         imem :: (Array I.Int16 W.Word8), -- instruction memory
                         dmem :: (Array I.Int16 I.Int16), -- data memory
                         smem :: (Array I.Int16 I.Int16)  -- stack memory
                       }
                       deriving (Show)

type MachineState = S.State Machine ()
type Program = Array I.Int16 W.Word8

bitwiseOr a b = a B..|. b
bitwiseAnd a b = a B..&. b

loadProgram :: Program -> Machine -> MachineState
loadProgram p m = do S.put $ m { imem = p }

run :: MachineState
run = do
  machine@(Machine rpc rtp rbp rcc imem dmem smem) <- S.get
  let instruction = imem ! rpc

  case toEnum $ fromIntegral instruction of
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

    Instructions.Load   -> return ()
    Instructions.LoadG  -> return ()
    Instructions.Sto    -> return ()
    Instructions.StoG   -> return ()
    Instructions.Const  -> return ()
    Instructions.Call   -> return ()
    Instructions.Ret    -> return ()
    Instructions.Enter  -> return ()
    Instructions.Leave  -> return ()
    Instructions.Jmp    -> return ()
    Instructions.FJmp   -> return ()
    Instructions.Read   -> return ()
    Instructions.Write  -> return ()
