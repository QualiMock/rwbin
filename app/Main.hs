module Main where

import System.Environment
import Data.Char (toLower)
import GHC.Word (Word8)
import Data.Bits
import Data.List (foldl')
import qualified Data.ByteString as BS

toBinary :: Word8 -> [Bool]
toBinary x = reverse [testBit x i | i <- [0 .. finiteBitSize x - 1]]

fromBinary :: [Bool] -> Word8
fromBinary = foldl' (\acc b -> (acc `shiftL` 1) .|. fromBool b) 0
  where
    fromBool True = 1
    fromBool False = 0

toBinaryString :: [Bool] -> String
toBinaryString [] = ""
toBinaryString (x:xs)
  | x && null xs = "1"
  | not x && null xs = "0"
  | x = '1' : toBinaryString xs
  | not x = '0' : toBinaryString xs
  | otherwise = []

fromBinaryString :: String -> [Bool]
fromBinaryString [] = []
fromBinaryString (x:xs)
  | x == '1' && null xs = [True]
  | x == '0' && null xs = [False]
  | x == '1' = True : fromBinaryString xs
  | x == '0' = False : fromBinaryString xs
  | otherwise = []

joinList :: [[a]] -> [a]
joinList [] = []
joinList [x] = x
joinList (x:xs) = x ++ joinList xs

splitToBytes :: [Bool] -> [[Bool]]
splitToBytes [] = []
splitToBytes list
  | length list < 8 = []
  | length list `mod` 8 /= 0 = []
  | otherwise = take 8 list : splitToBytes (drop 8 list)

readBinary :: String -> IO ()
readBinary file = do
  input <- BS.readFile file
  putStrLn . joinList $ map (toBinaryString . toBinary) .  BS.unpack $ input

writeBinary :: String -> String -> IO ()
writeBinary file binaryString =
  BS.writeFile file (BS.pack . map fromBinary . splitToBytes . fromBinaryString $ binaryString)

main :: IO ()
main = do
  args <- getArgs
  if length args > 3 || length args < 2
    then do putStrLn "Insufficient arguments"
    else do let file = args !! 1
            case (toLower . head . head $ args) of
              'w' -> do inputFile <- readFile $ args !! 2
                        writeBinary file inputFile
              'r' -> readBinary file
              _ -> putStrLn ""
