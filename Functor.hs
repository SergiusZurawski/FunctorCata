module EarthquakeKata  where
import Data.List

data Earthquake =  Earthquake {
    magnitude :: Integer
} deriving (Show)

data Observatory = Observatory {
    name :: String,
    country ::  String,
    year ::  Integer,
    area ::  Integer,
    quakes :: [Earthquake]
} deriving (Show)

largestMagnitudeEarthquake :: Observatory -> Earthquake
largestMagnitudeEarthquake a = foldr (\x b -> if (magnitude x) > (magnitude b) then x else b) (Earthquake 0) (quakes a)

avgMagnitudeEarthquake :: Observatory -> Double
avgMagnitudeEarthquake a = realToFrac(sum (getListOfEartQuakes a)) / realToFrac((foldl (\x b -> x + 1) 0 (getListOfEartQuakes a)))

getListOfEartQuakes :: Observatory -> [Integer]
getListOfEartQuakes a = map (\x -> (magnitude x)) (quakes a)

greaterThenMagnitute :: Observatory -> Integer -> [Earthquake]
greaterThenMagnitute a b = filter(\x -> (magnitude x) > b) (quakes a)
