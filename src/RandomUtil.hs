module RandomUtil
where

import Data.Random
import Data.Random.Extras as RE
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Args

splitPatterns :: [([Double], String)] -> ReaderT ProgramOptions IO ([(([Double], String), Int)], [(([Double], String),Int)])
splitPatterns patterns = do
    opts <- ask
    shuffled <- liftIO $ runRVar (RE.shuffle $ zip patterns [1,2..]) StdRandom
    let trainingSetLength = round $ (fromIntegral (length patterns)) * (separation opts)
    liftIO $ return (take trainingSetLength shuffled, drop trainingSetLength shuffled)