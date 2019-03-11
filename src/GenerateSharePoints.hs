module GenerateSharePoints where

import Protolude
import qualified Protolude.Base as P ( show )
import System.Random
import Types

generateSharePointsForSecret :: (RandomGen g, Num a) => Threshold -> a -> Reader g ([PolyPoint a], PolyC a)
generateSharePointsForSecret t secret = do
    poly   <- generatePoly t secret
    points <- getSharePoints t poly
    return (points, poly)

generatePoly :: (RandomGen g, Num a) => Threshold -> a -> Reader g (PolyC a)
generatePoly (Threshold m _) secret = PolyC . (secret :) <$> genRandomNumbers (m - 1)

getSharePoints :: (RandomGen g, Num a) => Threshold -> PolyC a -> Reader g [PolyPoint a]
getSharePoints (Threshold _ n) poly = genRandomNumbers n <&> fmap (pointAt poly)

genNext :: (RandomGen g, Num a) => (a, g) -> (a, g)
genNext (_, g) = first fromIntegral . next $ g

genRandomNumbers :: (RandomGen g, Num a) => Integer -> Reader g [a]
genRandomNumbers count = do
    generator <- ask
    return . drop 1 . take (fromIntegral count + 1) . fmap fst $ iterate genNext (0, generator)
