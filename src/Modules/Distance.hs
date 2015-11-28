module  Modules.Distance
(
  hammingDistance,
  euclideanDistance,
  Metric(Euclid,Hemming)
) where

data Metric = Euclid | Hemming deriving(Bounded, Show, Enum)

hammingDistance :: (Floating a) => [a] -> [a] -> a
hammingDistance x y = sum . map (abs) $ zipWith (-) x y

euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance x y = sqrt . sum . map (^2) $ zipWith (-) x y