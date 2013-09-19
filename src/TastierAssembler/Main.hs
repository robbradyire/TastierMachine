{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as Instructions
import qualified TastierMachine.Bytecode as Bytecode
import Data.Array (Array, listArray)
import Data.Int (Int16)
import Data.Char (isSpace)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.RWS.Lazy (execRWS)
import System.Environment (getArgs)
import Data.Maybe (fromJust)

commentOpenString = ";;"

parse :: [(Int, B.ByteString)] -> [Instructions.InstructionWord]
parse [] = []
parse ((lineNumber, text):rest) =
  if B.isPrefixOf commentOpenString text then --the line is a comment
    parse rest
  else
  (case B.words text of
    ["Add"]         -> Instructions.Nullary Instructions.Add
    ["Sub"]         -> Instructions.Nullary Instructions.Sub
    ["Mul"]         -> Instructions.Nullary Instructions.Mul
    ["Div"]         -> Instructions.Nullary Instructions.Div
    ["Equ"]         -> Instructions.Nullary Instructions.Equ
    ["Lss"]         -> Instructions.Nullary Instructions.Lss
    ["Gtr"]         -> Instructions.Nullary Instructions.Gtr
    ["Neg"]         -> Instructions.Nullary Instructions.Neg
    ["Load", a, b]  -> Instructions.Binary Instructions.Load (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
    ["Sto", a, b]   -> Instructions.Binary Instructions.Sto (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
    ["Call", a, b]  -> Instructions.Binary Instructions.Call (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
    ["LoadG", a]    -> Instructions.Unary Instructions.LoadG (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["StoG", a]     -> Instructions.Unary Instructions.StoG (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["Const", a]    -> Instructions.Unary Instructions.Const (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["Enter", a]    -> Instructions.Unary Instructions.Enter (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["Jmp", a]      -> Instructions.Unary Instructions.Jmp (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["FJmp", a]     -> Instructions.Unary Instructions.FJmp (fromIntegral $ fst $ fromJust $ B.readInteger a)
    ["Ret"]         -> Instructions.Nullary Instructions.Ret
    ["Leave"]       -> Instructions.Nullary Instructions.Leave
    ["Read"]        -> Instructions.Nullary Instructions.Read
    ["Write"]       -> Instructions.Nullary Instructions.Write
    ["Halt"]        -> Instructions.Nullary Instructions.Halt
    ["Dup"]         -> Instructions.Nullary Instructions.Dup
    _               -> error $ "Unknown instruction on line " ++ show lineNumber ++ ": " ++ show text
  )
  : (parse rest)

main = do
  args <- getArgs
  if length args == 2 then do
    assemblerFile <- B.readFile (args !! 0)
    let chunks = map (B.dropWhile isSpace) $ B.lines assemblerFile
    let instructions = parse $ zip [1..(length chunks)] chunks
    B.writeFile (args !! 1) $ P.runPut $ Bytecode.save instructions
  else
    error $ "Usage: tasm <input assembler file> <output bytecode file>"
