module  Modules.FCM
(
solveFcm,
FcmParams(..),
TrainingSet,
CenterClusters,
DistanceFunc,
calculateMatrixDiff,
generateInitialMatrix
) where

import Data.List.Split
import Modules.Distance
import Modules.InitializeFcmMethods

data FcmParams = FcmParams { clustersCount :: Int, dvtn :: Double, distance_func :: Metric, init_method :: Bool }
type MatrixAccessories = [[Double]]
type TrainingSet = [[Double]]
type CenterClusters = [[Double]]
type DistanceFunc = ([Double] -> [Double] -> Double)

exponentialWeight :: Int
exponentialWeight = 2

solveFcm :: TrainingSet -> FcmParams -> IO MatrixAccessories
solveFcm testObjects fcmParams = do
    matrix <- generateInitialMatrix fcmParams testObjects
    return $ solveWhileAccuracy testObjects matrix (dvtn fcmParams) (choseDistanceFunc fcmParams)


generateInitialMatrix :: FcmParams -> TrainingSet -> IO MatrixAccessories
generateInitialMatrix fcmParams testObjects =
    if (init_method fcmParams)
        then generateRandomMatrix (length testObjects) (clustersCount fcmParams)
        else return $ calculateMatrixAccessories testObjects centerClusters distFunc
            where centerClusters = generateRandomCenters (clustersCount fcmParams) testObjects
                  distFunc = choseDistanceFunc fcmParams


choseDistanceFunc :: FcmParams -> DistanceFunc
choseDistanceFunc fcmParams = 
    case distance_func fcmParams of 
        Hemming -> hammingDistance
        Euclid -> euclideanDistance



solveWhileAccuracy :: TrainingSet -> MatrixAccessories -> Double -> DistanceFunc -> MatrixAccessories
solveWhileAccuracy testObjects matrixAccessories e f = 
    if (calculateMatrixDiff matrixAccessories newAccesories < e )
        then newAccesories
        else solveWhileAccuracy testObjects newAccesories e f
             where newAccesories = calculateMatrixAccessories testObjects (calculateClusterCenters testObjects matrixAccessories) f


getColumn :: [[Double]] -> Int -> [Double]
getColumn [] _ = []
getColumn (x:xs) col = (last $ take col x) : getColumn xs col

getRow :: [[Double]] -> Int -> [Double]
getRow matr index = last $ take index matr

calculateClusterCenters :: TrainingSet-> MatrixAccessories-> CenterClusters
calculateClusterCenters trainSet mAccess = map (\index -> calculateCenter index) [1..centersCount]
    where     featureCount = length $ head trainSet
              centersCount = length $ head mAccess
              sumLists list = sum $ map(\el -> el^2) list
              sumListsWithPow acc train = sum $ zipWith(\a b -> (a^exponentialWeight)*b) acc train
              calculateCenterFeature centerIndex featureIndex = (sumListsWithPow (getColumn mAccess centerIndex) (getColumn trainSet featureIndex)) / (sumLists (getColumn mAccess centerIndex))
              calculateCenter centerIndex = map(\i -> calculateCenterFeature centerIndex i) [1..featureCount]
            
calculateMatrixAccessories :: TrainingSet -> CenterClusters -> DistanceFunc -> MatrixAccessories
calculateMatrixAccessories trainSet centerClusters f = map(\trainIndex -> calculateObjAccessor trainIndex) [1..objectsCount]
    where   centersCount = length centerClusters
            objectsCount = length trainSet
            calculateObjAccessor trainIndex= map(\centerIndex -> calcAccessor trainIndex centerIndex) [1..centersCount]
            calcAccessor trainIndex centerIndex = replace_NaN $ 1.0/(sum(subSumIterator trainIndex centerIndex))
            subSumIterator trainIndex centerIndex = map(\centIter -> calcSubSum trainIndex centerIndex centIter) [1..centersCount]
            calcSubSum trainIndex centerIndex centIter = ((f (getRow trainSet trainIndex) (getRow centerClusters centerIndex))/(f (getRow trainSet trainIndex) (getRow centerClusters centIter))) ^ (2)


calculateMatrixDiff :: MatrixAccessories -> MatrixAccessories -> Double
calculateMatrixDiff x y = maximum rowMax
        where   diffs = zipWith(\xRow yRow -> rem xRow yRow) x y
                rem xRow yRow = zipWith(\xEl yEl -> abs $ xEl - yEl) xRow yRow
                rowMax = map(\row -> getMax row) diffs


getMax :: [Double] -> Double
getMax [] = 0
getMax arr = maximum arr

replace_NaN :: Double -> Double
replace_NaN value
    | isNaN value = 1
    | otherwise = value