{-# LANGUAGE BangPatterns #-}

module Sign where

import Point
import Data.FiniteField.PrimeField
import Prelude hiding (toInteger)

n_ = 115792089237316195423570985008687907852837564279074904382605163141518161494337 :: Integer
n = 115792089237316195423570985008687907852837564279074904382605163141518161494337 :: Field
gx = 55066263022277343669578718895168534326250603453777594175500187360389116729240 :: Field
gy = 32670510020758816978083085130507043184471273380659243275938904335757337482424 :: Field
g = C gx gy

-- using BangPatterns
-- from finite-field package
exgcd :: Integral c => c -> c -> (c, c, c)
exgcd f1 f2 = f $ go f1 f2 1 0 0 1
  where
    go !r0 !r1 !s0 !s1 !t0 !t1
      | r1 == 0   = (r0, s0, t0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        (q, r2) = r0 `divMod` r1
        s2 = s0 - q*s1
        t2 = t0 - q*t1
    f (g,u,v)
      | g < 0 = (-g, -u, -v)
      | otherwise = (g,u,v)


type OP  = Field -> Field -> Field
mkOP op f g = fromInteger $ mod (op f' g') n_
    where f' = mod (toInteger f) n_
          g' = mod (toInteger g) n_
(|+|) = mkOP (+) :: OP 
(|*|) = mkOP (*) :: OP 
(|-|) = mkOP (-) :: OP
inv f = fromInteger (mod m n_) :: Field
    where (_,m,_) = exgcd (toInteger f) n_
f |/| h = f |*| inv h




data Seckey = Sec Field deriving Eq
data Pubkey = Pub Point deriving Eq
data Sig = Sig Field Field
data Msg = Msg Field

instance Show Seckey where
    show (Sec k) = show k

instance Show Pubkey where
    show (Pub k) = show k

instance Show Sig where
    show (Sig r s) = "{r : " ++ show r ++ ", s :" ++ show s ++ "}"    

instance Show Msg where
    show (Msg m) = show m    

mkPubkey :: Seckey -> Pubkey
mkPubkey (Sec n) = Pub $  n *** g 

verify :: Pubkey -> Msg -> Sig -> Bool
verify (Pub pk) (Msg m) (Sig r s) =
    case (m |/| s) *** g +++ ((r |/| s) *** pk) of
        Inf -> False
        C x y -> x == r

sign :: Seckey -> Msg -> Sig
sign (Sec sk) (Msg m) =
    let k =  12345 -- change random number
        (C r _) = k *** g
        s = (m |+| (r |*| sk)) |/| k
    in Sig r s
    

-- test
-- sk = Sec $ 448009486097687648
-- pk = mkPubkey sk
-- msg = Msg 9843759487659384673498
-- sig = sign sk msg
-- result = verify pk msg sig  


