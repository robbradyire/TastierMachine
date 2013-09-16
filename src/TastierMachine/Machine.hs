{-# LANGUAGE DoAndIfThenElse #-}
module TastierMachine.Machine where
import qualified TastierMachine.Instructions as Instructions
import qualified Data.Int as I
import qualified Data.Bits as B
import qualified Data.Array as A
import qualified Data.Word as W
import qualified Control.Monad.State as S
import System.IO
import System.IO.Unsafe

data Machine = Machine { rpc :: I.Int16,  -- mutable register containing the address of the next instruction to execute
                         rtp :: I.Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: I.Int16,  -- mutable register containing the address of the base of the stack
                         rcc :: I.Int16,  -- mutable register containing condition codes after operations

                         imem :: (A.Array I.Int16 W.Word8), -- instruction memory
                         dmem :: (A.Array I.Int32 I.Int16),   -- data memory
                         smem :: (A.Array I.Int32 I.Int16)    -- stack memory
                       }
                       deriving (Show)

type MachineState = S.State Machine ()
type Program = A.Array I.Int16 W.Word8

bitwiseOr a b = a B..|. b
bitwiseAnd a b = a B..&. b

loadProgram :: Program -> Machine -> MachineState
loadProgram p m = do S.put $ m { imem = p }

debug a = unsafePerformIO $ do { (putStrLn $ show a); hFlush stdout; return a}

{-
run :: MachineState
run = do
  machine@(Machine rpc rtp rbp rcc imem dmem smem) <- S.get
--  let imem' = debug imem
  let instruction = imem A.! (debug rpc)
  S.put $ machine { rpc = rpc + 1 }

  case toEnum $ fromIntegral instruction of
    Instructions.Halt -> return ()
    _ -> return ()
-}
