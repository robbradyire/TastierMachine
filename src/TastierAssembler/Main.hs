{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, OverloadedStrings #-}
module Main where
import qualified TastierMachine.Machine as Machine
import qualified TastierMachine.Instructions as I
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

myReadInteger a =
  case B.readInteger a of
    Just x -> x
    Nothing -> error $ "Unresolved external variable: " ++ (show a)

parseInstruction :: Int -> B.ByteString ->
                    (Either [B.ByteString] I.InstructionWord)
parseInstruction lineNumber text =
  case B.words text of
    ["Add"]         -> Right $ I.Nullary I.Add
    ["Sub"]         -> Right $ I.Nullary I.Sub
    ["Mul"]         -> Right $ I.Nullary I.Mul
    ["Div"]         -> Right $ I.Nullary I.Div
    ["Equ"]         -> Right $ I.Nullary I.Equ
    ["Lss"]         -> Right $ I.Nullary I.Lss
    ["Gtr"]         -> Right $ I.Nullary I.Gtr
    ["Neg"]         -> Right $ I.Nullary I.Neg
    ["Load", a, b]  -> Right $ I.Binary I.Load
                               (fromIntegral $ fst $ myReadInteger a)
                               (fromIntegral $ fst $ myReadInteger b)
    ["Sto", a, b]   -> Right $ I.Binary I.Sto
                               (fromIntegral $ fst $ myReadInteger a)
                               (fromIntegral $ fst $ myReadInteger b)
    ["LoadG", a]    -> Right $ I.Unary I.LoadG
                               (fromIntegral $ fst $ myReadInteger a)
    ["StoG", a]     -> Right $ I.Unary I.StoG
                               (fromIntegral $ fst $ myReadInteger a)
    ["Const", a]    -> Right $ I.Unary I.Const
                               (fromIntegral $ fst $ myReadInteger a)
    ["Enter", a]    -> Right $ I.Unary I.Enter
                               (fromIntegral $ fst $ myReadInteger a)

    ["Jmp", a]      ->  case B.readInteger a of
                          Just i -> Right $ I.Unary
                                            I.Jmp
                                            (fromIntegral $ fst i)
                          _ -> Left $ ["Jmp", a]

    ["FJmp", a]     ->  case B.readInteger a of
                          Just i -> Right $ I.Unary
                                            I.FJmp
                                            (fromIntegral $ fst i)
                          _ -> Left $ ["FJmp", a]

    ["Call", a, b]  ->  case B.readInteger b of
                          Just i -> Right $
                                    I.Binary
                                    I.Call
                                    (convert $ B.readInteger a)
                                    (convert $ B.readInteger b)
                          _ -> Left $ ["Call", a, b]

    ["Ret"]         -> Right $ I.Nullary I.Ret
    ["Leave"]       -> Right $ I.Nullary I.Leave
    ["Read"]        -> Right $ I.Nullary I.Read
    ["Write"]       -> Right $ I.Nullary I.Write
    ["Halt"]        -> Right $ I.Nullary I.Halt
    ["Dup"]         -> Right $ I.Nullary I.Dup
    ["Nop"]         -> Right $ I.Nullary I.Nop
    _               -> error $ "Unknown instruction on line " ++
                               show lineNumber ++ ": " ++ show text
  where
    convert = fromIntegral . fst . fromJust

parse :: RWS.RWS [B.ByteString]
                 [(Either [B.ByteString] I.InstructionWord)]
                 (Int, Int, M.Map B.ByteString Int)
                 ()
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
          error $ "Multiple definitions of the label " ++
                  (show mightBeLabelText) ++
                  " (line " ++ (show $ symbolTable M.! mightBeLabelText) ++
                  ", line " ++ (show lineNumber) ++ ")"
        else do
          RWS.put (lineNumber+1, instNumber+1,
                   M.insert mightBeLabelText instNumber symbolTable)
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
        ["Jmp", a]  ->
          if M.member a symtab then
            I.Unary I.Jmp (fromIntegral $ symtab M.! a)
          else badLabel lineNumber a

        ["FJmp", a] ->
          if M.member a symtab then
            I.Unary I.FJmp (fromIntegral $ symtab M.! a)
          else badLabel lineNumber a

        ["Call", a, b] ->
          if M.member b symtab then
            I.Binary I.Call (fromIntegral $ fst $ fromJust $ B.readInteger a)
                            (fromIntegral $ symtab M.! b)
          else badLabel lineNumber b

    badLabel lineNumber labelText =
      error $ "Reference to undefined label " ++ (show labelText) ++
              " on line " ++ (show lineNumber)

ignoreLinePredicate l = (not $ B.null l) &&
                        (not $ B.isPrefixOf commentOpenString l) &&
                        (not $ B.isPrefixOf eformatMarker l)

main = do
  args <- getArgs
  if length args == 2 then do
    assemblerFile' <- B.readFile (args !! 0)

    let assemblerFile = "Call 0 Main\nJmp $END\n" `B.append` assemblerFile'
                        `B.append` "\n$END: Halt\n"

    let chunks = filter ignoreLinePredicate $
                 map (B.dropWhile isSpace) $
                 B.lines assemblerFile

    let ((lines, insts, symtab), instructions) = RWS.execRWS
                                                 parse
                                                 chunks
                                                 (1, 0, M.empty)

    let instructions' = patchLabelAddresses symtab
                        (zip [1..length instructions] instructions)

    B.writeFile (args !! 1) $ P.runPut $ Bytecode.save instructions'
  else
    error $ "Usage: tasm <input assembler file> <output bytecode file>"
