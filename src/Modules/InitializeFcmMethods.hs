module Modules.InitializeFcmMethods
(
    generateRandomMatrix,
    generateRandomCenters,
    InitMethod(RandomMatrix, RandomCenters)
) where

import System.Random
import Data.List.Split

data InitMethod = RandomMatrix | RandomCenters deriving(Bounded, Show, Enum)


generateRandomCenters :: Int -> [[Double]] -> [[Double]]
generateRandomCenters clustersCount testObjects = take clustersCount testObjects

generateRandomMatrix :: Int -> Int -> IO [[Double]]
generateRandomMatrix testObjectsCount clustersCount = do
    rand <- generateRandomIoDouble testObjectsCount clustersCount
    return $ normalizeMatrix (chunksOf clustersCount rand)


normalizeMatrix :: [[Double]] -> [[Double]]
normalizeMatrix a = map (\ row -> map (/ (sum row)) row) a


generateRandomIoDouble :: Int -> Int -> IO [Double]
generateRandomIoDouble testObjectsCount clustersCount = do
    stdGen <- newStdGen
    return $ take (testObjectsCount * clustersCount) (randoms stdGen :: [Double])