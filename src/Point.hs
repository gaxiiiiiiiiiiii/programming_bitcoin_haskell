{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Point where

import Field
import GHC.TypeLits ( KnownNat, Nat, natVal )
import Data.Proxy ( Proxy(..) )
import Utils (exgcd)


data Point (p :: Nat) (a :: Nat) (b :: Nat) =  P (Field p) (Field p) | Inf

instance Eq (Point p a b) where
    (P x1 y1) == (P x2 y2) = x1 == x2 && y1 == y2

instance Show (Point p a b) where
    show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show Inf = "(Inf, Inf)"


mkP :: forall a b p. (KnownNat a, KnownNat b, KnownNat p) =>
       Field p -> Field p -> Maybe (Point p a b)
mkP x y  =
    let a = mkF $ natVal (Proxy :: Proxy a)
        b = mkF $ natVal (Proxy :: Proxy a)
    in if y /*/ y  == (x /*/ x /*/ x) /+/ (a /*/ x) /+/ b
        then Just $ P x y else Nothing



(|+|) :: forall a b p. (KnownNat a, KnownNat b, KnownNat p) =>
          Point p a b -> Point p a b -> Point p a b
Inf |+| y = y
x |+| Inf = x
(P x1 y1) |+| (P x2 y2)
    | x1 /= x2 = mkP s1
    | x1 == x2 && y1 /= y2 = Inf
    | x1 == x2 && y1 == y2 && y1 == zero = Inf
    | x1 == x2 && y1 == y2 && y1 /= zero = mkP s2
    where
        zero = F 0
        s1 = (y2 /-/ y1) /// (x2 /-/ x1)
        s2 = (F 3 /*/ (x1 /*/ x1)) /// (F 2 /*/ y1)
        mkP s = let x = ((s /*/ s) /-/ x1 /-/ x2)
                    y = (s /*/ (x1 /-/ x) /-/ y1)
                in P x y

(|*|) :: forall a b p. (KnownNat a, KnownNat b, KnownNat p) =>
         Field p -> Point p a b -> Point p a b
(F n) |*| p
    | n == 0 = Inf
    | n == 1 = p
    | even n =  F (div n 2) |*|(p |+| p)
    | otherwise = p |+|  ( F (n - 1) |*| p)





