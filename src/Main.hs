module Main
where

import Options

import Args

main :: IO()
main = runCommand $ \opts _ -> do
    print (inputPath opts)
    print (separation opts)
    print (attempts opts)