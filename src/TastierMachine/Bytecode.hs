{-# LANGUAGE ScopedTypeVariables #-}
module TastierMachine.Bytecode where
import qualified TastierMachine.Instructions as Instructions
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

load :: FilePath -> IO [W.Word8]
load filename = do
    fileContents <- B.readFile filename
    return $ B.unpack fileContents
    --map (toEnum . fromIntegral) $

save :: FilePath -> [Instructions.Instruction] -> IO ()
save filename program = do
    let bytes::[W.Word8] = map (fromInteger . fromIntegral . fromEnum) program
    B.writeFile filename $ B.pack bytes
