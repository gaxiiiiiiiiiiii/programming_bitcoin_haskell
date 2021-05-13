{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Script where

import Control.Monad.Free ( Free(..) )
import Control.Monad.Trans.State (StateT, put, get)
import Control.Monad.Trans.Class (lift)
import Hash (hash160)
--import Data.ByteString 
import Crypto.Util (i2bs_unsized)
import Sign ( verify, Msg, Sig, Pubkey )
import Field () 
import Address ()
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)

type Elm = ByteString


data Cmd next
    = PUT Elm next
    -- | OP_PUSHDATA1 next
    -- | OP_PUSHDATA2 next
    -- | OP_PUSHDATA4 next
    -- | OP_1 next
    -- | OP_2 next
    -- | OP_3 next
    -- | OP_4 next
    -- | OP_5 next
    -- | OP_6 next
    -- | OP_7 next
    -- | OP_8 next
    -- | OP_9 next
    -- | OP_10 next
    -- | OP_11 next
    -- | OP_12 next
    -- | OP_13 next
    -- | OP_14 next
    -- | OP_15 next
    -- | OP_16 next
    | OP_CHECKSIG Msg next
    | OP_DUP next
    | OP_HASH160 next
    | OP_EQUAL next
    | OP_VERIFY next
    | OP_EQUALVERIFY next
    deriving Functor

type SL = Free Cmd 

op_put v = Free $ PUT v (Pure ())
op_checksig msg = Free . OP_CHECKSIG msg $ Pure()
op_dup          = Free . OP_DUP $ Pure()
op_hash160      = Free . OP_HASH160 $ Pure()
op_equal        = Free . OP_EQUAL $ Pure()
op_verify       = Free . OP_VERIFY $ Pure()
op_equalverify  = Free . OP_EQUALVERIFY $ Pure() 

op_putpk :: Pubkey -> SL ()
op_putpk = op_put . encode

op_putsig :: Sig -> SL ()
op_putsig = op_put . encode

p2pk :: Sig -> Msg -> SL ()
p2pk sig msg = op_putsig sig >> op_checksig msg


class Monad m => StackMachine m where
    onPut :: Elm -> m ()
    onPop :: m Elm
    onVerify :: m ()

true = fromStrict . i2bs_unsized $ 1
false = fromStrict . i2bs_unsized $ 0

interpret :: (Monad m, StackMachine m) => SL a -> m a
interpret (Pure a) = return a
interpret (Free cmd) = case cmd of
    PUT v next -> onPut v >> interpret next
    OP_CHECKSIG msg next-> do
        sigb <- onPop
        pkb <- onPop
        let sig = decode sigb :: Sig
        let pk = decode pkb :: Pubkey
        if verify pk msg sig
            then onPut true  
            else onPut false
        interpret next

    OP_DUP next -> do
        x <- onPop
        onPut x
        onPut x
        interpret next
    OP_HASH160 next -> do 
        x <- onPop
        onPut . fromStrict . hash160 . toStrict $ x  
        interpret next      
    OP_EQUAL next -> do
        x <- onPop
        y <- onPop
        if x == y
            then onPut true  
            else onPut false
        interpret next
    OP_VERIFY next -> onVerify >> interpret next
    OP_EQUALVERIFY next -> do
        interpret op_equal      
        onVerify
        interpret next


type Script = StateT [Elm] Maybe

instance StackMachine Script where
    onPut v = do
        vs <- get
        put (v:vs)
    onPop = do
        vs <- get
        case vs of
            [] -> lift Nothing 
            (v:vs) -> put vs >> return v
    onVerify = do
        v <- get
        case v of
            [] -> lift Nothing 
            (v:rest) -> if v == true
                            then put rest
                            else lift Nothing



    
        
