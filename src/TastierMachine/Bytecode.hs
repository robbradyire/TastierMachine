{-# LANGUAGE ScopedTypeVariables #-}
module TastierMachine.Bytecode where
import qualified TastierMachine.Instructions as I
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

load :: FilePath -> IO [I.Instruction]
load filename = do
    fileContents <- B.readFile filename
    return $ map (toEnum . fromIntegral) $ B.unpack fileContents

save :: FilePath -> [I.Instruction] -> IO ()
save filename program = do
    let bytes::[W.Word8] = map (fromInteger . fromIntegral . fromEnum) program
    B.writeFile filename $ B.pack bytes
