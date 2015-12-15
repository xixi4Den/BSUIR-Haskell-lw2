module Args
where

import Options

data ProgramOptions = ProgramOptions
    { 
        inputPath :: String,
        separation :: Double,
        attempts :: Int,
        csvSeparator :: String,
        ignoreFirstCol :: Bool,
        ignoreFirstLine :: Bool,
        outPath :: String
    }


instance Options ProgramOptions where
    defineOptions = pure ProgramOptions
        <*> simpleOption "inputPath" ""
            "A path to input file."
        <*> simpleOption "separation" 0.8
            "A percent for separating input data into training and testing parts."
        <*> simpleOption "attempts" 1
            "A number of attempts for finding the best classificator."
        <*> simpleOption "csvSeparator" ","
            "A separator of the csv file"
        <*> simpleOption "ignoreFirstCol" False
            "The csv parser should ignore the first column."
        <*> simpleOption "ignoreFirstLine" False
            "The csv parser should ignore the first line."
        <*> simpleOption "outPath" ""
            "A path to output file. Writes to the console if is empty."