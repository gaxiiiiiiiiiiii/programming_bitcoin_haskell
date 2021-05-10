{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Tx where


import Prelude hiding (sequence)
-- import HGC.Generics (Generics)

import Data.Binary (Binary(..), encode)
import Data.Binary.Get (Get)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Hash (hash256)
import Types (Chars, toChars, Word32le, Int32le, Int64le)
import VarStr (VarStr, fromByteString)
import qualified VarStr
import VarList (VarList)
import qualified VarList

import Sign
import Field
import Address
import Crypto.Util (bs2i)



type TxId = Chars 32

data OutPoint = OutPoint
    { hash :: TxId
    , index :: Word32le
    } deriving (Show, Eq)

instance Binary OutPoint where
    put op = do
        put $ hash op
        put $ index op
    get = do
        h <- get :: Get TxId
        i <- get :: Get Word32le
        return $ OutPoint h i

data TxIn = TxIn
    { previousOutput  :: OutPoint
    , signatureScript :: VarStr
    , sequence        :: Word32le
    } deriving (Show)

instance Binary TxIn where
    put tx = do
        put $ previousOutput tx
        put $ signatureScript tx
        put $ sequence tx
    get = do
        out <- get :: Get OutPoint
        sig <- get :: Get VarStr
        seq <- get :: Get Word32le
        return $ TxIn out sig seq

data TxOut = TxOut
    { value :: Int64le
    , pkScript :: VarStr
    } deriving (Show)

instance Binary TxOut where
    put tx = do
        put $ value tx
        put $ pkScript tx
    get = do
        v <- get :: Get Int64le
        pk <- get :: Get VarStr
        return $ TxOut v pk

data Tx = Tx
    { version :: Int32le
    , txIn :: VarList TxIn
    , txOut :: VarList TxOut
    , lockTime :: Word32le
    } deriving (Show)

instance Binary Tx where
    put tx = do
        put $ version tx
        put $ txIn tx
        put $ txOut tx
        put $ lockTime tx
    get = do
        ver <- get :: Get Int32le
        txin <- get :: Get (VarList TxIn)
        txout <- get :: Get (VarList TxOut)
        lock <- get :: Get Word32le
        return $ Tx ver txin txout lock

txId :: Tx -> TxId
txId = toChars . hash256 . BL.toStrict . encode

class Txs txs where
    find :: txs -> TxId -> Tx

type Txl = [Tx]

instance Txs Txl where
    find txs txid = head $ filter ((==) txid . txId) txs


utxo :: Txl
utxo = undefined

alltx :: Txl
alltx = undefined

signTx :: Seckey -> TxIn -> IO TxIn
signTx sk txin =
    let outpoint = previousOutput txin
        outs = txOut $ find alltx (hash outpoint)
        pk = pkScript $ VarList.elems outs !! fromIntegral (index outpoint)
        dummy = txin {signatureScript = pk}
        h = hash256 $ BL.toStrict $ encode dummy
        msg = Msg (F (bs2i h))
    in do
        sig <- BL.toStrict . encode <$> sign sk msg
        return $ txin {signatureScript = fromByteString sig }




















