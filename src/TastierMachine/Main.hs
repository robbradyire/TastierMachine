{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as Instructions
import qualified TastierMachine.Bytecode as Bytecode
import qualified Control.Monad.State as S
import qualified Data.Array as A
import qualified Data.Word as W
import qualified Data.Int as I
import System.IO

main = do
  let instructions::[W.Word8] = map (fromInteger . fromIntegral . fromEnum) [Instructions.Halt]
  let program::(A.Array I.Int16 W.Word8) = (A.listArray (0,0) instructions)
  let machine = (Machine.Machine 0 0 0 0 program (A.listArray (0,0) []) (A.listArray (0,0) []))
  putStrLn $ show machine
  --hFlush stdout
  --putStrLn $ show $ S.execState Machine.run machine
