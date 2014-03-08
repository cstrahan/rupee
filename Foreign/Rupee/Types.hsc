{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Foreign.Rupee.Types where

#include "ruby.h"
#include "shim.h"

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Data.Data
import Data.Maybe
import Data.Word
import Data.Typeable
import GHC.Generics (Generic)
import Data.Hashable

data RubyException = RubyException { rbExceptionObject :: ForeignPtr RValue } deriving (Show, Typeable)
instance Exception RubyException

newtype RValue = RValue (Ptr RValue) deriving (Eq, Ord, Show, Typeable, Data, Storable, Generic)
newtype RID    = RID (Ptr RID) deriving (Eq, Ord, Show, Typeable, Data, Storable, Generic)

data Dispatch = Dispatch (ForeignPtr RValue)  -- self
                         RID                  -- method id
                         CInt                 -- argc
                         (Ptr RValue)         -- argv
                         (ForeignPtr RValue)  -- block
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
