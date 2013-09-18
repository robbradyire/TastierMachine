{-# LANGUAGE DoAndIfThenElse #-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import Data.Int (Int8, Int16)
import Data.Bits (complement)
import Data.Array ((//), (!), Array, elems)
import Control.Monad.RWS.Lazy (RWS, put, get, ask, tell, local)

data Machine = Machine { rpc :: Int16,  -- mutable register containing the address of the next instruction to execute
                         rtp :: Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: Int16,  -- mutable register containing the address of the base of the stack
                         rcc :: Int16,  -- mutable register containing condition codes after operations

                         imem :: (Array Int16 Instructions.InstructionWord), -- instruction memory
                         dmem :: (Array Int16 Int16), -- data memory
                         smem :: (Array Int16 Int16)  -- stack memory
                       }
                       deriving (Show)

run :: RWS [Int16] [Int16] Machine ()
run = do
  machine@(Machine rpc rtp rbp rcc imem dmem smem) <- get
  let instructionWord = imem ! rpc

  case instructionWord of
    Instructions.Nullary i ->
      case i of
        Instructions.Halt -> do
          put $ machine { rpc = rpc + 1 }
          return ()

        Instructions.Dup -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, smem ! (rtp-1))]) }
          run

        Instructions.Add -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = a + b
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Sub    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = a - b
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Mul    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = a * b
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Div    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = a `div` b
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Equ    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (a == b)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Lss    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (a < b)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Gtr    -> do
          let a = smem ! (rtp-1)
          let b = smem ! (rtp-2)
          let result = fromIntegral $ fromEnum (a > b)
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(rtp-2, result)]) }
          run

        Instructions.Neg    -> do
          let a = smem ! (rtp-1)
          let result = complement a
          put $ machine { rpc = rpc + 1, smem = (smem // [(rtp-1, result)]) }
          run

        Instructions.Ret    -> do
          put $ machine { rpc = (smem ! (rtp-1)), rtp = rtp - 1 }
          run

        Instructions.Leave  -> do
          put $ machine { rtp = rbp+1, rbp = (smem ! (rbp+3)) }
          run

        Instructions.Read   -> do
          (i:rest) <- ask
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, i)]) }
          local tail run

        Instructions.Write  -> do
          tell $ [smem ! (rtp-1)]
          put $ machine { rpc = rpc + 1, rtp = rtp - 1 }
          run

    Instructions.Unary i a ->
      case i of
        Instructions.StoG   -> do
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, dmem = (dmem // [(a, (smem ! (rtp-1)))]) }
          run

        Instructions.LoadG  -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (dmem ! a))]) }
          run

        Instructions.Const  -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, a)]) }
          run

        Instructions.Enter  -> do
          let lexicalLevelDelta = (smem ! (rtp-1))
          if lexicalLevelDelta == 0 then do -- calling a procedure at the same level as the current one
            put $ machine { rtp = rtp+a+2, rbp = rtp-2, smem = (smem // [(rtp, (smem ! (rbp+2))), (rtp+1, rbp)]) }
            run
          else do -- calling a procedure at a different level than the current one
            let calleeStaticLinkFieldAddr = followChain 1 lexicalLevelDelta rbp smem
            put $ machine { rtp = rtp+a+2, rbp = rtp-2, smem = (smem // [(rtp, calleeStaticLinkFieldAddr), (rtp+1, rbp)]) }
            run

        Instructions.Jmp  -> do
          put $ machine { rpc = a }
          run

        Instructions.FJmp -> do
          let jump = smem ! (rtp-1)
          if jump == 0 then do
            put $ machine { rtp = rtp - 1, rpc = a }
            run
          else do
            put $ machine { rtp = rtp - 1, rpc = rpc + 1 }
            run

    Instructions.Binary i a b ->
      case i of
        Instructions.Load   -> do --Load gets a variable from a calling frame onto the top of the stack
          let loadAddr = (followChain 0 a rbp smem) + b + 4
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (smem ! loadAddr))]) }
          run

        Instructions.Sto    -> do
          let storeAddr = (followChain 0 a rbp smem) + b + 4
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(storeAddr, (smem ! (rtp-1)))]) }
          run

        Instructions.Call   -> do
          put $ machine { rpc = b, rtp = rtp + 2, smem = (smem // [(rtp, rpc+3), (rtp+1, a)]) }
          run

--followChain follows the static link chain to find the address of the base of the stack frame i levels down

followChain :: Int16 -> Int16 -> Int16 -> (Array Int16 Int16) -> Int16
followChain limit n rbp smem =
  if n > limit then
    followChain limit (n-1) (smem ! rbp) smem
  else rbp
