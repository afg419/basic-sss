module Main where

import Lib
import Protolude
import Data.Modular
import System.Random
import Data.Numbers.Primes
import GenerateSharePoints
import ReconstructSecret
import Types
import Data.Modular
import Data.Numbers.Primes
import Data.List

getPrime :: Int -> IO ()
getPrime i = print $ primes !! i

main :: IO ()
main = do
    generator <- getStdGen
    let secret         = 2130523 :: Integer / 13834127
    let (points, poly) = flip runReader generator $ generateSharePointsForSecret (sThreshold 4 6) secret
    print points
    print poly
    let (secret', poly') = reconstructSecret (take 4 points)
    print secret'
    print poly'
    return ()

instance KnownNat n => Fractional (Integer / n) where
    recip = inv
    fromRational (a1 :% a2) = (toMod' a1) * (inv (toMod' a2))
