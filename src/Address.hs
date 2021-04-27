--{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Address where

import Field
import Point
import Sign
import Data.ByteString (ByteString)
import Data.ByteString as B
import Crypto.Util (i2bs_unsized, i2bs, bs2i)
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Data.Proxy ( Proxy(..) )





toBS = i2bs 256
fromBS = bs2i
pre = i2bs_unsized

sec :: (KnownNat a, KnownNat b, KnownNat p) => Point p a b -> ByteString
sec (P (F x) (F y)) = append (pre 4) (append (toBS x) (toBS y))

secComp :: (KnownNat a, KnownNat b, KnownNat p) => Point p a b -> ByteString
secComp (P (F x) (F y))
    | even y    = append (pre 2) (toBS x)
    | otherwise = append (pre 3) (toBS x)


parseSec :: forall a b p. (KnownNat a, KnownNat b, KnownNat p) => 
             ByteString -> Maybe (Point p a b)
parseSec bs
    | prefix == 2 = Just $ P x (rootEven y2)     
    | prefix == 3 = Just $ P x (rootOdd  y2)
    | prefix == 4 = Just $ P x y
    | otherwise  = Nothing
    where prefix = B.head bs
          b_ = B.drop 1 bs
          x = F . fromBS $ B.take 32 b_
          y = F . fromBS $ B.drop 32 b_
          a = F $ natVal (Proxy :: Proxy a)
          b = F $ natVal (Proxy :: Proxy b) 
          y2 = (x /^/ 3) /+/ (a /*/ x) /+/ b

test p = Just p == parseSec (secComp p)




