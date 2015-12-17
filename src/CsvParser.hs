module CsvParser
where

import System.IO
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.List.Split.Internals
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Text.Printf
import qualified Data.ByteString.Char8 as BS

import Args

parseCsv :: ProgramOptions -> IO [([Double], String)]
parseCsv opts = source (inputPath opts) $$ conduit opts =$ CL.consume


showResult :: ProgramOptions -> [Int] -> [(String, [(Double, Double)])] -> IO ()
showResult opts indexes results = if (outPath opts) == ""
    then showOnConsole results indexes
    else runResourceT $ (CL.sourceList results) $$ CL.map (\x -> BS.pack $ resultToString x ++ "\n" ++ (indexesToString indexes)) =$ (CB.sinkFile (outPath opts))


source :: String -> Source IO String
source filePath = do
    handle <- liftIO $ openFile filePath ReadMode
    readLineByLine handle 
    where 
        readLineByLine handle = do
            eof <- liftIO $ hIsEOF handle
            if eof
                then return ()
                    else do 
                    line <- liftIO $ hGetLine handle
                    yield line
                    readLineByLine handle    


conduit :: ProgramOptions -> Conduit String IO ([Double], String)
conduit opts = await >>= \str -> 
    case str of 
        Nothing -> return (); 
        Just j -> do
            let splittedStr = (splitOn (csvSeparator opts) j)
            let filtered = filterFirstColumn opts splittedStr
            let result = (map (\x -> read x :: Double) $ init filtered, last filtered)
            yield result
            conduit opts


filterFirstColumn :: ProgramOptions -> [String] -> [String]
filterFirstColumn opts splittedStr = if (ignoreFirstCol opts)
    then tail splittedStr
    else splittedStr


showOnConsole :: [(String, [(Double, Double)])] -> [Int] -> IO ()
showOnConsole results indexes = do
    mapM_ (\c -> putStrLn (resultToString $ c)) results 
    putStrLn $ indexesToString indexes


resultToString :: (String, [(Double, Double)]) -> String
resultToString c = (fst c) ++ (foldl (++) "" $ zipWith (\values index -> printf " - %d(%.2f;%.2f)" (index :: Int) (fst values) (snd values)) (snd c) [1,2..])


indexesToString :: [Int] -> String
indexesToString indexes = foldl (++) "" $ map (\index -> printf "%d, " index) indexes