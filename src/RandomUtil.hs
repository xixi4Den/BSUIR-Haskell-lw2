module RandomUtil
where

import Data.Random
import Data.Random.Extras as RE

splitPatterns :: Double -> [([Double], String)] -> IO ([([Double], String)], [([Double], String)])
splitPatterns percent patterns = do
    shuffled <- runRVar (RE.shuffle patterns) StdRandom
    let trainingSetLength = round $ (fromIntegral (length patterns)) * percent
    return (take trainingSetLength shuffled, drop trainingSetLength shuffled)