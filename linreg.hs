import GHC.Float (int2Double)

data Feature a = Feature
  { name :: String,
    value :: a
  }

data Label a = Label
  { labelName :: String,
    labelValue :: a
  }

type Features a = [Feature a]

data Sample a = Sample (Features a) (Label a)

type TrainingSet a = [Sample a]

type Weights a = [a]

type Bias a = a

square :: Double -> Double
square n = n * n

halve :: Double -> Double
halve n = n / 2

dot :: [Double] -> [Double] -> Double
dot w x = sum $ zipWith (*) w x

squaredErrorLoss :: Double -> Label Double -> Double
squaredErrorLoss prediction label = halve $ square (prediction - labelValue label)

loss :: Weights Double -> Bias Double -> Sample Double -> Double
loss weights bias (Sample features label) = squaredErrorLoss (dot weights (map value features) + bias) label

loss' :: Weights Double -> Bias Double -> TrainingSet Double -> Double
loss' ws b tset = sum (map (loss ws b) tset) / int2Double (length tset)

-- trainingSet :: [Sample]
-- Goal is to choose the weights such that the affine transformation of
-- dotProduct (transpose weights) features approximates label