module Main
where

--import System.IO
import Options
--import Data.Conduit
--import qualified Data.Conduit.List as CL
--import Data.Conduit.Binary as CB
--import Data.ByteString
--import Control.Monad.Trans.Resource

import Args
import CsvParser

main :: IO()
main = runCommand $ \opts _ -> do
    print $ inputPath opts
    results <- parseCsv opts 
    print results 
    --print (inputPath opts)
    --    $$ decode Ct.utf8
    --    =$ CB.lines
    --    =$ numberLine
    --    =$ Ct.encode Ct.utf8
    --    $$ sinkFile "test.txt"
        --where process = CL.

    --runResourceT $ source $$ copyConduit =$ sink
    --where source = sourceFile "butterfly.txt"
    --      sink = sinkFile "butterfly.copy.txt" 
    --      copyConduit = await >>= \x -> 
    --        case x of 
    --            Nothing -> return();
    --            Just j -> yield j;

    --print (inputPath opts)
    --print (separation opts)
    --print (attempts opts)