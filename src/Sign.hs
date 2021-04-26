{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Sign where

import Point ( Point(..), (|+|), (|*|) ) 
import Field ( Field(..), mkF ) 
-- import Data.FiniteField.PrimeField
-- import Prelude hiding (toInteger)
import System.Random ( Random(randomIO) )
import Utils (exgcd)
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Data.Proxy ( Proxy(..) )



type Fp = Field (115792089237316195423570985008687907853269984665640564039457584007908834671663 :: Nat)
type G = Point (115792089237316195423570985008687907853269984665640564039457584007908834671663 :: Nat) (0 :: Nat) (7 :: Nat)

p = 115792089237316195423570985008687907853269984665640564039457584007908834671663 :: Integer
n_ = 115792089237316195423570985008687907852837564279074904382605163141518161494337 :: Integer
n = F 115792089237316195423570985008687907852837564279074904382605163141518161494337 :: Fp
gx = F 55066263022277343669578718895168534326250603453777594175500187360389116729240 :: Fp
gy = F 32670510020758816978083085130507043184471273380659243275938904335757337482424 :: Fp
g = P gx gy :: G




mkOP :: (Integer -> Integer -> Integer) -> Fp -> Fp -> Fp
mkOP op (F m) (F n) =
    let m' = mod m n_
        n' = mod n n_
    in F $ mod (op m' n') n_
(\+\) = mkOP (+) 
(\*\) = mkOP (*) 
(\-\) = mkOP (-) 
inv :: Fp -> Fp
inv (F n) = F (mod m n_) :: Fp
    where (_,m,_) = exgcd n n_
f \\\ h = f \*\ inv h




data Seckey = Sec Fp deriving Eq
data Pubkey = Pub G deriving Eq
data Sig = Sig Fp Fp
data Msg = Msg Fp

instance Show Seckey where
    show (Sec k) = show k

instance Show Pubkey where
    show (Pub k) = show k

instance Show Sig where
    show (Sig r s) = "{r : " ++ show r ++ ", s :" ++ show s ++ "}"    

instance Show Msg where
    show (Msg m) = show m    

mkPubkey :: Seckey -> Pubkey
mkPubkey (Sec n) = Pub $  n |*| g 

verify :: Pubkey -> Msg -> Sig -> Bool
verify (Pub pk) (Msg m) (Sig r s) =
    let u = m \\\ s
        v = r \\\ s
        t = (u |*| g) |+| (v |*| pk)
    in case t of
        Inf -> False
        P x y -> x == r

sign :: Seckey -> Msg -> IO Sig
sign (Sec sk) (Msg m) = do
    k <- randomK
    let (P r _) = k |*| g
    let s = (m \+\ (r \*\ sk)) \\\ k
    return $ Sig r s


randomF :: IO Fp
randomF = randomIO >>= return . mkF 

randomK :: IO Fp
randomK = do
    k <- randomF
    if k == n || k == F 0 then randomK else return k


    

-- test
sk = Sec $ F 448009486097687648
pk = mkPubkey sk
msg = Msg $ F 9843759487659384673498
sigIO = sign sk msg
--result :: IO Bool
result = verify pk msg <$> sigIO







