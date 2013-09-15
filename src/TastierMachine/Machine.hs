module TastierMachine.Machine where
import qualified TastierMachine.Instructions as I
import Data.Int (Int16)


data Machine = Machine { rsa :: Int16,  -- immutable register containing the address in the code at which to start executing
                         rpc :: Int16,  -- mutable register containing the address of the next instruction to execute
                         rss :: Int16,  -- immutable register containing the maximum stack size
                         rtp :: Int16,  -- mutable register containing the address of the top of the stack
                         rbp :: Int16,  -- mutable register containing the address of the base of the stack

                         imem :: [I.Instruction],   -- instruction memory
                         gmem :: [Int16],           -- small memory containing global variables
                         smem :: [Int16]            -- stack memory containing
                       }
