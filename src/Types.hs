module Types where

import Protolude
import qualified Protolude.Base as P ( show )


data PolyC a where
    PolyC :: Num a => [a] -> PolyC a

instance Eq a => Eq (PolyC a) where
    pa@(PolyC as) == pb@(PolyC bs) = take maxDeg as' == take maxDeg bs'
        where
            maxDeg = fromIntegral $ max (degree pa) (degree pb)
            as' = as ++ zeros
            bs' = bs ++ zeros

instance Show a => Show (PolyC a) where
    show poly = "p(x) = " <> renderPolyC poly

data Threshold = Threshold Integer Integer
sThreshold :: Integer -> Integer -> Threshold
sThreshold a b = Threshold m n
  where
    a' = abs a
    b' = abs b
    m  = min a' b'
    n  = max a' b'

data PolyPoint a where
    PolyPoint :: Num a => (a, a) -> PolyPoint a
deriving instance Show a => (Show (PolyPoint a))

xCoord :: PolyPoint a -> a
xCoord (PolyPoint (a, _)) = a

yCoord :: PolyPoint a -> a
yCoord (PolyPoint (_, b)) = b

renderPolyC :: Show a => PolyC a -> [Char]
renderPolyC (PolyC [a0]) = show a0
renderPolyC (PolyC []  ) = renderPolyC (PolyC [0])
renderPolyC (PolyC (a0 : as)) =
    show a0
        <> " + "
        <> ( intercalate " + "
           $ map (\(power, coefficient) -> (show coefficient) <> "x^" <> show power) ([1 ..] `zip` as)
           )

evalPoly :: PolyC a -> a -> a
evalPoly (PolyC []       ) _ = 0
evalPoly (PolyC [a0     ]) _ = a0
evalPoly (PolyC (a0 : as)) x = a0 + x * evalPoly (PolyC as) x

pointAt :: Num a => PolyC a -> a -> PolyPoint a
pointAt poly x = PolyPoint (x, evalPoly poly x)

(.+.) :: PolyC a -> PolyC a -> PolyC a
PolyC as .+. PolyC bs = PolyC . fmap (uncurry (+)) . take l $ zip as' bs'
  where
    as' = as ++ zeros
    bs' = bs ++ zeros
    l   = max (length as) (length bs)

(*.) :: a -> PolyC a -> PolyC a
scalar *. (PolyC as) = PolyC . fmap (* scalar) $ as

(.*.) :: Num a => PolyC a -> PolyC a -> PolyC a
pa .*. pb = PolyC coefficients
  where
    newDegree    = degree pa + degree pb
    coefficients = fmap (multiplyInDegree pa pb) $ take (fromIntegral newDegree + 1) [0 ..]

zeros :: Num a => [a]
zeros = fromInteger <$> [0, 0 ..]

degree :: PolyC a -> Integer
degree (PolyC as) = (\x -> x - 1) . fromIntegral . length $ as

multiplyInDegree :: PolyC a -> PolyC a -> Integer -> a
multiplyInDegree (PolyC as) (PolyC bs) k = sum . fmap (uncurry (*)) $ as' `zip` (reverse bs')
  where
    as' = take (fromIntegral k + 1) $ as ++ zeros
    bs' = take (fromIntegral k + 1) $ bs ++ zeros

