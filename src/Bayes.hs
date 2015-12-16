module Bayes
where

import Data.List


groupByCluster :: [([Double], String)] -> [[([Double], String)]]
groupByCluster trainingSet = groupBy (\a b -> snd a == snd b) $ sortBy (\a b -> compare (snd a) (snd b)) trainingSet


clusterName :: [([Double], String)] -> String
clusterName group = snd $ head group


average :: [Double] -> Double
average list = sum list / (fromIntegral (length list))


getExpectedVectors :: [[([Double], String)]] -> [([Double], String)]
getExpectedVectors groups = do    
    map (\group -> getExpectedVectorsByGroup group) groups


getExpectedVectorsByGroup :: [([Double], String)] -> ([Double], String)
getExpectedVectorsByGroup group = do
    (map (\column -> average column) transposed, clusterName group)
        where 
            transposed = transpose $ map (fst) group


expectedVectorForCluster :: String -> [([Double], String)] -> [Double]
expectedVectorForCluster clusterName expectedVectors = fst $ head $ filter (\x -> snd x == clusterName) $ expectedVectors


getVariances :: [[([Double], String)]] -> [([Double], String)] -> [([Double], String)]
getVariances groups expectedVectors = map (\x -> getVariancesByGroup x expectedVectors) $ groups


getVariancesByGroup :: [([Double], String)] -> [([Double], String)] -> ([Double], String)
getVariancesByGroup group expectedVectors = do
    (zipWith (\attr expected -> koef * (sum $ map (\attrI -> (attrI - expected)**2) attr)) transposed expectedVector, clusterName group)
        where
            transposed = transpose $ map (\row -> fst row) group
            expectedVector = expectedVectorForCluster (clusterName group) expectedVectors
            koef = 1 / fromIntegral (length group - 1)

varianceForCluster :: String -> [([Double], String)] -> [Double]
varianceForCluster clusterName variances = fst $ head $ filter (\x -> snd x == clusterName) $ variances


getClassificator :: [[([Double], String)]] -> [(String, [(Double, Double)])]
getClassificator groups =
    map (\group -> ((clusterName group), foldl (++) [] $ zipWith (\a b -> [(a, b)]) (expectedVector group) (variance group))) groups
    where
        expectedVectors = getExpectedVectors groups
        expectedVector group = expectedVectorForCluster (clusterName group) expectedVectors
        variances = getVariances groups expectedVectors
        variance group = varianceForCluster (clusterName group) variances