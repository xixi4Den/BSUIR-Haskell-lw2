module Main
where

import Options

import Args
import CsvParser
import RandomUtil
import Bayes

main :: IO()
main = runCommand $ \opts _ -> do
    patterns <- parseCsv opts 
    resWithIndexes <- splitPatterns (separation opts) patterns
    let indexes = map (snd) $ fst resWithIndexes
    let res = map (fst) $ fst resWithIndexes
    showResult opts indexes $ getClassificator $ groupByCluster res