module Main
where

import Options
import Control.Monad.Reader

import CsvParser
import RandomUtil
import Bayes

main :: IO()
main = runCommand $ \opts _ -> do
    patterns <- runReader parseCsv opts 
    resWithIndexes <- runReaderT (splitPatterns patterns) opts
    let indexes = map (snd) $ fst resWithIndexes
    let res = map (fst) $ fst resWithIndexes
    runReader (showResult indexes $ getClassifiers res) opts