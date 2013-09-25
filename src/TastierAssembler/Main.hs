{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as Instructions
import qualified TastierMachine.Bytecode as Bytecode
import Data.Array (Array, listArray)
import Data.Int (Int16)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.RWS.Lazy (execRWS)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Control.Monad.RWS.Lazy as RWS

commentOpenString = ";;"
eformatMarker = "."

parseInstruction :: Int -> B.ByteString -> (Either [B.ByteString] Instructions.InstructionWord)
parseInstruction lineNumber text =
      case B.words text of
        ["Add"]         -> Right $ Instructions.Nullary Instructions.Add
        ["Sub"]         -> Right $ Instructions.Nullary Instructions.Sub
        ["Mul"]         -> Right $ Instructions.Nullary Instructions.Mul
        ["Div"]         -> Right $ Instructions.Nullary Instructions.Div
        ["Equ"]         -> Right $ Instructions.Nullary Instructions.Equ
        ["Lss"]         -> Right $ Instructions.Nullary Instructions.Lss
        ["Gtr"]         -> Right $ Instructions.Nullary Instructions.Gtr
        ["Neg"]         -> Right $ Instructions.Nullary Instructions.Neg
        ["Load", a, b]  -> Right $ Instructions.Binary Instructions.Load (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
        ["Sto", a, b]   -> Right $ Instructions.Binary Instructions.Sto (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
        ["LoadG", a]    -> Right $ Instructions.Unary Instructions.LoadG (fromIntegral $ fst $ fromJust $ B.readInteger a)
        ["StoG", a]     -> Right $ Instructions.Unary Instructions.StoG (fromIntegral $ fst $ fromJust $ B.readInteger a)
        ["Const", a]    -> Right $ Instructions.Unary Instructions.Const (fromIntegral $ fst $ fromJust $ B.readInteger a)
        ["Enter", a]    -> Right $ Instructions.Unary Instructions.Enter (fromIntegral $ fst $ fromJust $ B.readInteger a)

        ["Jmp", a]      ->  case B.readInteger a of
                              Just i -> Right $ Instructions.Unary Instructions.Jmp (fromIntegral $ fst i)
                              _ -> Left $ ["Jmp", a]

        ["FJmp", a]     ->  case B.readInteger a of
                              Just i -> Right $ Instructions.Unary Instructions.FJmp (fromIntegral $ fst i)
                              _ -> Left $ ["FJmp", a]

        ["Call", a, b]  ->  case B.readInteger b of
                              Just i -> Right $ Instructions.Binary Instructions.Call (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ fst $ fromJust $ B.readInteger b)
                              _ -> Left $ ["Call", a, b]

        ["Ret"]         -> Right $ Instructions.Nullary Instructions.Ret
        ["Leave"]       -> Right $ Instructions.Nullary Instructions.Leave
        ["Read"]        -> Right $ Instructions.Nullary Instructions.Read
        ["Write"]       -> Right $ Instructions.Nullary Instructions.Write
        ["Halt"]        -> Right $ Instructions.Nullary Instructions.Halt
        ["Dup"]         -> Right $ Instructions.Nullary Instructions.Dup
        ["Nop"]         -> Right $ Instructions.Nullary Instructions.Nop
        _               -> error $ "Unknown instruction on line " ++ show lineNumber ++ ": " ++ show text

parse :: RWS.RWS [B.ByteString] [(Either [B.ByteString] Instructions.InstructionWord)] (Int, Int, M.Map B.ByteString Int) ()
parse = do
  (lineNumber, instNumber, symbolTable) <- RWS.get
  sourceCode <- RWS.ask
  if lineNumber > length sourceCode then return ()
  else do
    let currentLine = sourceCode !! (lineNumber-1)
    let mightBeLabelText = B.takeWhile isAlphaNumOrDollar currentLine
    let restOfLine = B.drop (B.length mightBeLabelText) currentLine

    if ((B.length mightBeLabelText) > 0) then --could be a label
      if B.null restOfLine then do --can only be an instruction
        RWS.put (lineNumber+1, instNumber+1, symbolTable)
        RWS.tell $ [parseInstruction instNumber currentLine]
        parse
      else if (B.head restOfLine) == ':' then --definitely a label
        if M.member mightBeLabelText symbolTable then
          error $ "Multiple definitions of the label " ++ (show mightBeLabelText) ++ " (line " ++ (show $ symbolTable M.! mightBeLabelText) ++ ", line " ++ (show lineNumber) ++ ")"
        else do
          RWS.put (lineNumber+1, instNumber+1, M.insert mightBeLabelText instNumber symbolTable)
          RWS.tell $ [parseInstruction instNumber $ B.tail restOfLine]
          parse
      else do
        RWS.put (lineNumber+1, instNumber+1, symbolTable)
        RWS.tell $ [parseInstruction instNumber currentLine]
        parse
    else do
      RWS.put (lineNumber+1, instNumber+1, symbolTable)
      RWS.tell $ [parseInstruction instNumber currentLine]
      parse
  where
    isAlphaNumOrDollar a = (a == '$' || isAlphaNum a)

patchLabelAddresses symtab instructions =
  map (patchLabel symtab) instructions
  where
    patchLabel symtab (lineNumber, (Right x)) = x
    patchLabel symtab (lineNumber, (Left x)) =
      case x of
        ["Jmp", a]  -> if M.member a symtab then Instructions.Unary Instructions.Jmp (fromIntegral $ symtab M.! a) else badLabel lineNumber a
        ["FJmp", a] -> if M.member a symtab then Instructions.Unary Instructions.FJmp (fromIntegral $ symtab M.! a) else badLabel lineNumber a
        ["Call", a, b] -> if M.member b symtab then Instructions.Binary Instructions.Call (fromIntegral $ fst $ fromJust $ B.readInteger a) (fromIntegral $ symtab M.! b) else badLabel lineNumber b

    badLabel lineNumber labelText = error $ "Reference to undefined label " ++ (show labelText) ++ " on line " ++ (show lineNumber)

ignoreLinePredicate l = (not $ B.null l) && (not $ B.isPrefixOf commentOpenString l) && (not $ B.isPrefixOf eformatMarker l)

main = do
  args <- getArgs
  if length args == 2 then do
    assemblerFile' <- B.readFile (args !! 0)
    let assemblerFile = "Call 0 Main\nJmp $END\n" `B.append` assemblerFile' `B.append` "\n$END: Halt\n"
    let chunks = filter ignoreLinePredicate $ map (B.dropWhile isSpace) $ B.lines assemblerFile
    let ((lines, insts, symtab), instructions) = RWS.execRWS parse chunks (1, 0, M.empty)
    let instructions' = patchLabelAddresses symtab (zip [1..length instructions] instructions)
    B.writeFile (args !! 1) $ P.runPut $ Bytecode.save instructions'
  else
    error $ "Usage: tasm <input assembler file> <output bytecode file>"
