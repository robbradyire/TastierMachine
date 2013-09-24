{-# LANGUAGE DoAndIfThenElse #-}
{- |
  This module contains the implementation of the virtual machine which
  executes the instruction set from "TastierMachine.Instructions".

  The virtual processor has 4096 words of random-access data memory,
  and 4096 words of stack memory.

  The size of a machine word is 16 bits, and the storage format is
  big-endian (the first byte in the word is the most significant),
  or in other words, the two bytes in the word are 16 continuous
  bits in memory, going from most significant (bit 15) down to least
  significant (bit 0).

  The machine has four state registers which can be loaded onto the
  stack for manipulation, and stored back again. One of the registers,
  rcc has a special purpose: it's the condition code register where
  exceptions are notified to the program. For example, when an arithmetic
  operation causes an overflow, a particular bit in rcc will be set.

  The bit layout of rcc is as follows:

@
  15: Unused
  14: Unused
  13: Unused
  12: Unused
  11: Unused
  10: Unused
  9: Unused
  8: Unused
  7: Unused
  6: Unused
  5: Unused
  4: Unused
  3: Unused
  2: stack address error - this bit is set when an operation accesses an out of bounds stack memory address
  1: data address error - this bit is set when an operation accesses an out of bounds data memory address
  0: overflow bit - this bit is set when an operation overflows 16 bits and cleared at the start of the next cycle
@

-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import Data.Int (Int8, Int16)
import Data.Bits (complement)
import Data.Array ((//), (!), Array, elems)
import Control.Monad.RWS.Lazy (RWS, put, get, ask, tell, local)

data Machine = Machine { rpc :: Int16,  -- ^ register containing the address of the next instruction to execute
                         rtp :: Int16,  -- ^ register containing the address of the top of the stack
                         rbp :: Int16,  -- ^ register containing the address of the base of the stack
                         rcc :: Int16,  -- ^ register containing condition codes after operations

                         imem :: (Array Int16 Instructions.InstructionWord), -- ^ instruction memory
                         dmem :: (Array Int16 Int16), -- ^ data memory
                         smem :: (Array Int16 Int16)  -- ^ stack memory
                       }
                       deriving (Show)

{- |
  This function implements the internal state machine executing the instructions.
-}

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

        Instructions.Nop -> do
          put $ machine { rpc = rpc + 1 }
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
          {-
            The return address is on top of stack, set the pc to that address
          -}
          put $ machine { rpc = (smem ! (rtp-1)), rtp = rtp - 1 }
          run

        Instructions.Leave  -> do
          {-
            When we're leaving a procedure, we have to reset rbp and rtp to
            the values they had in the calling context. Our calling
            convention is that we store the return address at the top of the
            caller's stack frame, and rbp at the bottom of the callee's
            stack frame.

            ENTER stored the old rbp on top of stack, so we reset it from
            there. The new rtp is whatever the base of the current stack
            frame (the one we're about to leave) is. Since we're returning
            from the procedure, we don't need to keep all the local
            variables around, so we can just shrink the stack by setting the
            new rtp to be the current rbp.

            The RET instruction (which comes after LEAVE) will see the
            return address on top of stack, and jump there.
          -}
          put $ machine { rtp = rbp, rbp = (smem ! (rtp-1)) }
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
        Instructions.StoG   -> do -- memory mapped control and status registers implemented here
          case a of
            0 -> put $ machine { rpc = (smem ! (rtp-1)), rtp = rtp - 1 }
            1 -> put $ machine { rpc = rpc + 1, rtp = (smem ! (rtp-1)) }
            2 -> put $ machine { rpc = rpc + 1, rtp = rtp - 1, rbp = (smem ! (rtp-1)) }
            3 -> put $ machine { rpc = rpc + 1, rtp = rtp - 1, rcc = (smem ! (rtp-1)) }
            _ -> put $ machine { rpc = rpc + 1, rtp = rtp - 1, dmem = (dmem // [(a-4, (smem ! (rtp-1)))]) }
          run

        Instructions.LoadG  -> do -- memory mapped control and status registers implemented here
          case a of
            0 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, rpc)]) }
            1 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, rtp)]) }
            2 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, rbp)]) }
            3 -> put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, rcc)]) }
            _ -> put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (dmem ! (a-4)))]) }
          run

        Instructions.Const  -> do
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, a)]) }
          run

        Instructions.Enter  -> do
          {-
            ENTER has to save the base pointer then set the new base pointer
            to the current top of stack so that the called procedure can
            store local variables without overwriting other data. ENTER gets
            passed the offset of the new top of stack as an argument (this
            is how much stack space to pre-allocate).

            ENTER sees the return address and the lexical level delta on the
            top of the stack. However, RET expects the return address on top
            of stack, so we have to pop the delta and push rbp for leave. We
            can just store rbp in the delta's stack slot (rtp - 1).
          -}
          if (smem ! (rtp-1)) == 0 then do --calling a procedure at the same level, so we can share the stack frame
            put $ machine { rtp = rtp+a, rbp = rtp, smem = (smem // [(rtp, rbp+1), (rtp+1, rbp)]) }
            run
          else do
            let staticLink = followChain 1 (smem ! (rtp-1)) rbp smem
            put $ machine { rtp = rtp+a, rbp = rtp, smem = (smem // [(rtp, staticLink), (rtp+1, rbp)]) }
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
        Instructions.Load   -> do
          {-
            Load gets a variable from a calling frame onto the top of the
            stack. We follow the chain of links to find the stack frame the
            variable is in, add b (the address of the variable in that
            frame) and add two, because each frame has the caller's rbp,
            lexical level delta, static link, and dynamic link stored on the
            bottom of the stack.
          -}
          let loadAddr = (followChain 0 a rbp smem) + b + 4
          put $ machine { rpc = rpc + 1, rtp = rtp + 1, smem = (smem // [(rtp, (smem ! loadAddr))]) }
          run

        Instructions.Sto    -> do --Store updates a variable in a calling frame
          let storeAddr = (followChain 0 a rbp smem) + b + 4
          put $ machine { rpc = rpc + 1, rtp = rtp - 1, smem = (smem // [(storeAddr, (smem ! (rtp-1)))]) }
          run

        Instructions.Call   -> do
          {-
            CALL gets passed the lexical level delta in slot a, and the
            address of the procedure in slot b. CALL pushes the return
            address onto the stack, then the lexical level delta, so when
            the called procedure does ENTER, the stack contains the lexical
            level delta at (rtp - 1) and the return address at (rtp - 2).
          -}
          put $ machine { rpc = b, rtp = rtp + 2, smem = (smem // [(rtp, rpc+1), (rtp+1, a)]) }
          run

--followChain follows the static link chain to find the address of the base of the stack frame i levels down

followChain :: Int16 -> Int16 -> Int16 -> (Array Int16 Int16) -> Int16
followChain limit n rbp smem =
  if n > limit then
    followChain limit (n-1) (smem ! (rbp+1)) smem
  else rbp
