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


import Data.Maybe

import Data.Binary (Binary(..))
import Data.Binary (encode, decode)
import Data.Binary.Put (putByteString)
import Data.Binary.Get (Get(..), getByteString, getBytes)
import Control.Monad (replicateM)





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

instance forall a b p. (KnownNat a, KnownNat b, KnownNat p) => Binary (Point p a b) where
    put = putByteString . secComp
    get = do
        bs <- getByteString 33
        return . fromJust $ parseSec bs

instance Binary Pubkey where
    put (Pub p) = put p
    get = do
        p <- get :: Get G
        return $ Pub p













(+++) = append


der :: Sig -> ByteString
der (Sig (F r) (F s)) = prefix +++ len +++ marker +++ rlen +++ rbin +++ marker +++ slen +++ sbin
    where prefix = pre 4
          marker = pre 2
          rbin = mkBin $ toBS r
          sbin = mkBin $ toBS s
          mkBin bs = if B.head bs < 128 then bs else  pre 0 +++ bs
          rlen = pre . toInteger $ B.length rbin
          slen = pre . toInteger $ B.length sbin
          len = pre (fromBS rlen + fromBS slen)

check :: ByteString -> Maybe ByteString
check bs =
    if B.head bs == 4
        then return $ B.tail bs
        else Nothing

getLen :: ByteString -> Maybe (Int, ByteString)
getLen bs = do
    let len = fromEnum $ B.head bs
    let sig = B.tail bs
    return (len, sig)

isMarked :: ByteString -> Maybe ByteString
isMarked bs = if B.head bs == 2 then return $ B.tail bs else Nothing

getSig :: ByteString -> Maybe (Int, Fp, ByteString)
getSig bs = do
    body <- isMarked bs
    let len = fromEnum . B.head $ body
        r = F . fromBS . B.take len . B.tail $ body
        rest = B.drop (len + 1) body
    return (len, r,rest)

parseDer :: ByteString -> Maybe Sig
parseDer bs = do
    checked <- check bs
    (len,sig) <- getLen checked
    (rlen,r,rest) <- getSig sig
    (slen,s,_) <- getSig rest
    if len == slen + rlen then return $ Sig r s else Nothing


instance Binary Sig where
    put = putByteString . der
    get = do
        c <- getByteString  1 
        lenb <- getByteString 1
        let len = fromEnum $ fromBS lenb
        body <- getByteString (len + 4)
        let sig = B.append c (B.append lenb body)
        return .fromJust $ parseDer sig


bin = der <$> sigIO
--test = parseDer <$> bin

test = do
    sig <- sigIO
    let r = decode (encode sig)
    print $ sig == r

--    print (sig == (decode $ encode sig))





