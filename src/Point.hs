{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Point where

import GHC.TypeLits
import Data.FiniteField.PrimeField
import Prelude hiding (toInteger)
-- import System.IO.Unsafe (unsafePerformIO)
-- import System.Random



type Field = PrimeField 115792089237316195423570985008687907853269984665640564039457584007908834671663

p = 115792089237316195423570985008687907853269984665640564039457584007908834671663
zero = 115792089237316195423570985008687907853269984665640564039457584007908834671663 :: Field

-- mid = 57896044618658097711785492504343953926634992332820282019728792003954417335831 :: Field
-- max = zero - 1

-- root :: Field -> Field 
-- root f = let r = f ^ ((p +1) `div` 4)
--          in if r > mid then -r else r

data Point
    = C Field Field
    | Inf
    deriving Eq

mkPoint :: Field -> Field -> Maybe Point
mkPoint x y = if y ^ 2 == x ^ 3 + a * x + b then Just $ C x y else Nothing
    where a = 0 :: Field
          b = 7 :: Field

instance Show Point where
    show (C n m) = "(" ++ show n ++ ", " ++ show m ++ ")"
    show Inf     = "(inf, inf)"



(+++) :: Point -> Point -> Point
(+++) Inf y = y
(+++) x Inf = x
(+++) (C x1 y1) (C x2 y2)
    | x1 /= x2 = mkP s1
    | x1 == x2 && y1 /= y2 = Inf
    | x1 == x2 && y1 == y2 && y1 == zero = Inf
    | x1 == x2 && y1 == y2 && y1 /= zero = mkP s2
    where
        s1 = (y2- y1) / (x2 - x1)
        s2 = (3 * (x1 ^ 2)) / (2 * y1)
        mkP s = let x = (s^2 - x1 - x2)
                    y = (s * (x1 - x) - y1)
                in C x y

mulI :: Point -> Integer -> Point
mulI p 0 = Inf
mulI p 1 = p
mulI p n | even n    =  mulI (p +++ p) (div n 2)
         | otherwise = p +++ mulI p (n - 1)


(***) :: Field -> Point -> Point
n *** p = mulI p (toInteger n)










