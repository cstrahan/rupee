{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Rupee.Types where

#include "ruby.h"
#include "shim.h"

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Data.Data
import Data.Maybe
import Data.Word
import Data.Typeable

data RubyException = RubyException { rbExceptionObject :: RValue } deriving (Show, Typeable)
instance Exception RubyException


newtype RValue = RValue (Ptr RValue) deriving (Eq, Ord, Show, Typeable, Data, Storable)
newtype RID    = RID (Ptr RID) deriving (Eq, Ord, Show, Typeable, Data, Storable)

newtype RubyT m a = RubyT { runRubyT :: ReaderT RValue m a }
                    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)
type Ruby a = RubyT IO a

instance MonadBase b m => MonadBase b (RubyT m) where
    liftBase = lift . liftBase

instance MonadTransControl RubyT where
    newtype StT RubyT a = StRuby {unStRuby :: StT (ReaderT RValue) a}
    liftWith = defaultLiftWith RubyT runRubyT StRuby
    restoreT = defaultRestoreT RubyT unStRuby
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (RubyT m) where
    newtype StM (RubyT m) a = StMRuby {unStMRuby :: ComposeSt RubyT m a}
    liftBaseWith = defaultLiftBaseWith StMRuby
    restoreM     = defaultRestoreM unStMRuby
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

data Dispatch = Dispatch RValue
                         RID
                         CInt
                         (Ptr RValue)
                         RValue

instance Storable Dispatch where
    sizeOf _ = (#size struct s_dispatch)
    alignment = sizeOf
    peek ptr = do self <- peek ((#ptr struct s_dispatch, self) ptr)
                  mid  <- peek ((#ptr struct s_dispatch, mid) ptr)
                  argc <- peek ((#ptr struct s_dispatch, argc) ptr)
                  argv <- peek ((#ptr struct s_dispatch, argv) ptr)
                  blk  <- peek ((#ptr struct s_dispatch, blk) ptr)
                  return (Dispatch self mid argc argv blk)
    poke ptr (Dispatch self mid argc argv blk) = do
        (#poke struct s_dispatch, self) ptr self
        (#poke struct s_dispatch, mid) ptr mid
        (#poke struct s_dispatch, argc) ptr argc
        (#poke struct s_dispatch, argv) ptr argv
        (#poke struct s_dispatch, blk) ptr blk

-- | Ruby native types
data RType = RNone
           | RObject
           | RClass
           | RModule
           | RFloat
           | RString
           | RRegexp
           | RArray
           | RHash
           | RStruct
           | RBignum
           | RFile
           | RData
           | RMatch
           | RComplex
           | RRational
           | RNil
           | RTrue
           | RFalse
           | RSymbol
           | RFixnum
           | RUndef
           | RNode
           | RIClass
           | RZombie
           deriving (Eq, Show, Read)

type HSCallback = CInt        -- argc
               -> Ptr RValue  -- argv
               -> RValue      -- self
               -> Ptr RValue  -- exc ptr, for indicating failure
               -> RValue      -- Array for registering Ruby objects
               -> RValue      -- Proc (Qnil if not given)
               -> IO RValue   -- result
