{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Field (
    Field(..),
    mkF,
    (/+/),
    (/*/),
    (/-/),
    (///),
    (/^/),
    rootEven,
    rootOdd
)where

import GHC.TypeLits ( KnownNat, Nat, natVal )
import Data.Proxy ( Proxy(..) )
import Utils (exgcd)

data Field (p :: Nat) = F Integer deriving Eq

instance Show (Field p) where
    show (F n) = show n

mkF :: forall p. (KnownNat p) => Integer -> Field p
mkF n =
    let p = natVal (Proxy :: Proxy p)
    in F (mod n p)


mkOP :: forall p. (KnownNat p) =>
        (Integer -> Integer -> Integer) ->
        Field p -> Field p -> Field p
mkOP op (F n) (F m) = F $ mod (op (mod n p) (mod m p)) p
    where p = natVal (Proxy :: Proxy p)


(/+/) :: (KnownNat p) => Field p -> Field p -> Field p
(/+/) = mkOP (+)

(/*/) :: (KnownNat p) => Field p -> Field p -> Field p
(/*/) = mkOP (*)

(/-/) :: (KnownNat p) => Field p -> Field p -> Field p
(/-/) = mkOP (-)

invF :: forall p. (KnownNat p) => Field p -> Field p
invF (F n) = F $ mod m p
    where p = natVal (Proxy :: Proxy p)
          (_,m,_) = exgcd n p

(///) :: (KnownNat p) => Field p -> Field p -> Field p
x /// y = x /*/ invF y

(/^/) :: forall p. (KnownNat p) => Field p -> Integer -> Field p
f /^/ 0 = let p = natVal (Proxy :: Proxy p) in F p
f /^/ 1 = f
f /^/ n | even n = (f /*/ f) /^/ div n 2
        | otherwise  = f /*/ (f /^/ (n - 1))

root :: forall p. (KnownNat p) => Field p -> Field p
root f = f /^/ div (p + 1) 4
    where p = natVal (Proxy :: Proxy p)

rootEven :: forall p. (KnownNat p) => Field p -> Field p
rootEven f = case root f of
    F n -> if even n then F n else F $ p - n
    where p = natVal (Proxy :: Proxy p)

rootOdd :: forall p. (KnownNat p) => Field p -> Field p
rootOdd f = case root f of
    F n -> if odd n then F n else F $ p - n
    where p = natVal (Proxy :: Proxy p)













