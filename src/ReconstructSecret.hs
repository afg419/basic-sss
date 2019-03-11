module ReconstructSecret where

import Protolude
import qualified Protolude.Base as P ( show )
import System.Random
import Types
import Data.List

lagrangeBasisPoly :: Fractional a => [PolyPoint a] -> Integer -> PolyC a
lagrangeBasisPoly points j = (1 / denominator) *. numerator
  where
    indices = take (length points) [0 ..]
    indicesWithout j' = [ x | x <- indices, x /= j' ]

    x_ i = xCoord $ points !! (fromIntegral i)
    d_ i = x_ j - x_ i
    p_ i = PolyC [-1 * (x_ i), 1]

    numerator   = foldr (.*.) (PolyC [1]) (fmap p_ $ indicesWithout j)
    denominator = foldr (*) 1 (fmap d_ $ indicesWithout j)

reconstructSecret :: Fractional a => [PolyPoint a] -> (a, PolyC a)
reconstructSecret points = (evalPoly reconstructedPoly 0, reconstructedPoly)
  where
    indices = take (length points) [0 ..]
    x_ i = xCoord $ points !! (fromIntegral i)
    y_ i = yCoord $ points !! (fromIntegral i)
    coefficientPolys  = fmap (\i -> y_ i *. lagrangeBasisPoly points i) indices
    reconstructedPoly = foldr (.+.) (PolyC [0]) coefficientPolys
