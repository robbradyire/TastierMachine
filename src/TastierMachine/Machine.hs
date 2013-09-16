module TastierMachine.Machine where
import qualified TastierMachine.Instructions as I
import Data.Int
import Data.Bits
import qualified Control.Monad.State as State

data Machine = Machine { rsa :: Int16,  -- immutable register containing the address in the code at which to start executing
                         rpc :: Int16,  -- mutable register containing the address of the next instruction to execute
                         rss :: Int16,  -- immutable register containing the maximum stack size
                         rtp :: Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: Int16,  -- mutable register containing the address of the base of the stack
                         rcc :: Int16,  -- mutable register containing condition codes after operations

                         imem :: [I.Instruction],   -- instruction memory
                         dmem :: [Int16],           -- data memory
                         smem :: [Int16]            -- stack memory
                       } deriving (Show)

type MachineState = State.State Machine

run :: MachineState ()
run = do
    machine <- State.get
    if testBit (rcc machine) 0 then -- bit zero of the condition code register is set by the halt instruction so we should stop now
        return ()
    else do
        let instruction = (imem machine) !! (fromIntegral $ rpc machine)
        case instruction of
            I.Halt ->
                let oldRCC = rcc machine
                    newRCC = oldRCC .|. (bit 0)
                in do State.put $ machine { rcc = newRCC }

            _ -> do return ()

