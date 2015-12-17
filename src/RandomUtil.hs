module RandomUtil
where

import Data.Random
import Data.Random.Extras as RE

splitPatterns :: Double -> [([Double], String)] -> IO ([(([Double], String), Int)], [(([Double], String),Int)])
splitPatterns percent patterns = do
    shuffled <- runRVar (RE.shuffle $ zip patterns [1,2..]) StdRandom
    let trainingSetLength = round $ (fromIntegral (length patterns)) * percent
    return (take trainingSetLength shuffled, drop trainingSetLength shuffled)

