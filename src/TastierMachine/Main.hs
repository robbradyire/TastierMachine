{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as Instructions
import qualified TastierMachine.Bytecode as Bytecode
import qualified Control.Monad.State as S
import qualified Data.Array as A
import qualified Data.Word as W
import qualified Data.Int as I
import System.IO
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

main = do
  args <- getArgs
  if length args == 1 then do
    bytecodeFile <- B.readFile (args !! 0)
    let instructions = G.runGet Bytecode.load bytecodeFile
    let program::(A.Array I.Int16 Instructions.InstructionWord) = (A.listArray (0, fromIntegral $ (length instructions)-1) instructions)
    putStrLn $ show program
    let machine = (Machine.Machine 0 0 0 0 program
                    (A.listArray (0,4095) (take 4096 $ cycle [0]))
                    (A.listArray (0,4095) (take 4096 $ cycle [0])))
    putStrLn $ show $ Machine.smem $ S.execState Machine.run machine
  else
    error $ "Usage: TastierMachine <bytecode file>"
