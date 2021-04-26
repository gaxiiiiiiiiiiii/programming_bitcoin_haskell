{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Field (
    Field(..),
    mkF,
    (/+/),
    (/*/),
    (/-/),
    (///)
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













