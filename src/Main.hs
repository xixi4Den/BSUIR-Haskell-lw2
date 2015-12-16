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
    res <- splitPatterns (separation opts) patterns
    showResult opts $ getClassificator $ groupByCluster $ fst res