import Control.Applicative
import Options
import Modules.FCM
import Modules.CsvParser
import Modules.Distance
import System.IO.Error
import Control.Exception

data MainOptions = MainOptions
   { inputFile :: String,
    outputFile :: String,
    numOfClaster :: Int,
    accuracy :: Double,
    metric :: Metric,
    isRandCenters :: Bool,
    separator :: String,
    isIgnoreHeader :: Bool,
    isIgnoreFirstColumn :: Bool
   ,isIgnoreLastColumn :: Bool }

instance Options MainOptions where
    defineOptions = pure MainOptions
     	<*> simpleOption "inputFile" ""
            "Input filepath"
        <*> simpleOption "outputFile" ""
            "Output filepath"
        <*> simpleOption "numOfClaster" 0
            "number of clusters"
        <*> simpleOption "accuracy" 0.0001
            "accuracy"
        <*> defineOption (optionType_enum "metric") (\o -> o 
        	{ optionLongFlags = ["metric"] 
        	 , optionDefault = Euclid 
        	})
        <*> simpleOption "isRandCenters" False
            "Method of initialize: false==RandomMatrix or true==RandomCenters"
        <*> simpleOption "separator"  ","
            "column separator(will select the first character of the string)"
        <*> simpleOption "isIgnoreHeader" False
            "Ignore header row"

        <*> simpleOption "isIgnoreFirst" False
            "Ignore first column"
        <*> simpleOption "isIgnoreLast" False
            "Ignore last column"

main::IO()
main = runCommand $ \opts args -> do
    redirectResultToOutput opts `catch` catcherror

catcherror :: IOError -> IO()
catcherror ex = ioError ex

redirectResultToOutput :: MainOptions -> IO()
redirectResultToOutput param 
    | "" == (outputFile param) = do
        cl <- solve param
        putStrLn (show $ matrixWithLineSeparator cl)
    | otherwise = do
        cl <- solve param
        writeFile (outputFile param) (show $ matrixWithLineSeparator cl)

matrixWithLineSeparator :: [[Double]] -> String
matrixWithLineSeparator [] =  ""
matrixWithLineSeparator m = show (head m) ++ "\\r\\n" ++ matrixWithLineSeparator (tail m)

solve :: MainOptions -> IO [[Double]]
solve param = do
        let fcm = FcmParams { clustersCount = (numOfClaster param), dvtn = (accuracy param), distance_func = (metric param), init_method = (isRandCenters param) }
        parsed <- parse_from_csv param
        cluserized <- solveFcm parsed fcm
        return cluserized


parse_from_csv :: MainOptions -> IO [[Double]]
parse_from_csv param = do
    let parseOpts = ParserOptions { Modules.CsvParser.separator = ((head (Main.separator param)) :: Char), f_line = (isIgnoreHeader param), f_column = (isIgnoreFirstColumn param), l_column = (isIgnoreLastColumn param) }
    source <- readFile (inputFile param)
    csvResult <- parseCsv source parseOpts
    case csvResult of Left errorMessage -> error errorMessage
                      Right objects -> return objects