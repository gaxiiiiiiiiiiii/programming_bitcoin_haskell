{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module VarList where

import Prelude hiding (length)
import qualified Prelude as P

import Control.Monad (replicateM)
import GHC.Exts (IsList(..))

import Data.Binary (Binary(..))

import VarInt (VarInt)

data VarList a = VarList
    { length :: VarInt
    , elems :: [a]
    } deriving Show

instance IsList (VarList a) where
    type Item (VarList a) = a
    fromList xs = VarList (fromIntegral $ P.length xs) xs
    toList = elems

    


