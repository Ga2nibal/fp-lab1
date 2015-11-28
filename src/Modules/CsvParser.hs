module Modules.CsvParser (
  parseCsv,
  ParserOptions(..),
) where



import Data.Csv
import Data.Char
import Data.Vector as V
import Data.ByteString.Internal as BSI
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC

data ParserOptions = ParserOptions { separator :: Char, f_line,f_column,l_column :: Bool}
                                               
parseCsv :: String -> ParserOptions -> IO(Either String [[Double]])
parseCsv from_file csvPrms = do
    let options = DecodeOptions { decDelimiter = fromIntegral $ ord $ separator csvPrms }
        csv_data = BSL.pack $ BSI.unpackBytes $ BSC.pack from_file
        parsed = if (f_line csvPrms) 
          then decodeWith options NoHeader csv_data :: Either String (Vector (Vector String)) 
          else decodeWith options HasHeader  csv_data :: Either String (Vector (Vector String))
    case parsed of Left error -> return $ Left error
                   Right parsed_data -> do
                        let checked = convertToDoubleList(removeColumns (l_column csvPrms) (f_column csvPrms) parsed_data)
                        return $ Right $ checked

removeColumns :: Bool -> Bool -> Vector (Vector a) -> Vector (Vector a)
removeColumns True True m = removeHeads (removeLasts m)
removeColumns True False m = removeHeads m
removeColumns False True m = removeLasts m
removeColumns False False m = m

removeHeads :: Vector (Vector a) -> Vector (Vector a)
removeHeads arr = V.map V.init arr

removeLasts :: Vector (Vector a)-> Vector (Vector a)
removeLasts arr = V.map V.tail arr

convertToDoubleList :: Vector (Vector String) -> [[Double]]
convertToDoubleList v 
    | V.null v = []
    | otherwise = Prelude.map read (toList (V.head v)) : convertToDoubleList (V.tail v)