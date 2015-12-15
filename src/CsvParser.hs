module CsvParser
where

import System.IO
import Data.Conduit
import Data.List.Split.Internals
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

import Args

parseCsv :: ProgramOptions -> IO [([Double], String)]
parseCsv opts = source (inputPath opts) $$ conduit opts =$ CL.consume

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
            let result = (map (\x -> read x :: Double) $ init splittedStr, last splittedStr)
            yield result
            conduit opts