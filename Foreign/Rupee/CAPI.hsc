{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

#include "ruby.h"
#include "shim.h"

module Foreign.Rupee.CAPI where

import Foreign.Rupee.Builtins (rbObject, rbProc)
import Foreign.Rupee.Types
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe
import Data.IORef
import Data.Data
import Data.Maybe
import Data.Word
import Data.Typeable
import qualified Data.HashTable.IO as H
type HashTable k v = H.BasicHashTable k v

rbFalse, rbTrue, rbNil, rbUndef :: ForeignPtr (ForeignPtr RValue)
rbFalse = unsafePerformIO $ newForeignPtr_ $ intPtrToPtr #{const Qfalse}
rbTrue  = unsafePerformIO $ newForeignPtr_ $ intPtrToPtr #{const Qtrue}
rbNil   = unsafePerformIO $ newForeignPtr_ $ intPtrToPtr #{const Qnil}
rbUndef = unsafePerformIO $ newForeignPtr_ $ intPtrToPtr #{const Qundef}

rtype :: (ForeignPtr RValue) -> IO RType
rtype obj =
    withForeignPtr obj $ \objp ->
        do typ <- rb_type objp
           return $ case typ of
                        #{const RUBY_T_NONE}     -> RNone
                        #{const RUBY_T_OBJECT}   -> RObject
                        #{const RUBY_T_CLASS}    -> RClass
                        #{const RUBY_T_MODULE}   -> RModule
                        #{const RUBY_T_FLOAT}    -> RFloat
                        #{const RUBY_T_STRING}   -> RString
                        #{const RUBY_T_REGEXP}   -> RRegexp
                        #{const RUBY_T_ARRAY}    -> RArray
                        #{const RUBY_T_HASH}     -> RHash
                        #{const RUBY_T_STRUCT}   -> RStruct
                        #{const RUBY_T_BIGNUM}   -> RBignum
                        #{const RUBY_T_FILE}     -> RFile
                        #{const RUBY_T_DATA}     -> RData
                        #{const RUBY_T_MATCH}    -> RMatch
                        #{const RUBY_T_COMPLEX}  -> RComplex
                        #{const RUBY_T_RATIONAL} -> RRational
                        #{const RUBY_T_NIL}      -> RNil
                        #{const RUBY_T_TRUE}     -> RTrue
                        #{const RUBY_T_FALSE}    -> RFalse
                        #{const RUBY_T_SYMBOL}   -> RSymbol
                        #{const RUBY_T_FIXNUM}   -> RFixnum
                        #{const RUBY_T_UNDEF}    -> RUndef
                        #{const RUBY_T_NODE}     -> RNode
                        #{const RUBY_T_ICLASS}   -> RIClass
                        #{const RUBY_T_ZOMBIE}   -> RZombie
                        _                        -> undefined

--------------------------------------------------------------------------------
--- GC related crap


rupeeInit :: IO ()
rupeeInit =
    do ruby_init
       ruby_init_loadpath
       rupee_init hs_free_fun_ptr
       -- create our new GC root
       objectClass <- rbObject
       markerFun <- (mkMarkCB marker)
       foreignMarker <- rupee_wrap_struct objectClass markerFun nullFunPtr nullPtr
       registered <- malloc foreignMarker
       rb_gc_register_address registered
       return ()

-- this seems sketchy...
-- might want to just use a RWL.
marker :: Ptr () -> IO ()
marker ptr =
    atomicModifyIORef' keepAliveSet $ \h ->
        unsafePerformIO $
            do H.mapM_ (\(k,_) -> rb_gc_register_mark_object k) h
               return (h, ())

mkRegistered :: IO (ForeignPtr RValue)
mkRegistered =
    do ptr <- mallocForeignPtr
       addForeignPtrFinalizer rb_gc_unregister_address_funptr ptr
       rb_gc_register_address (unsafeForeignPtrToPtr ptr)
       return ptr

{-# NOINLINE keepAliveSet #-}
keepAliveSet :: IORef (HashTable RValue ())
keepAliveSet = unsafePerformIO $ newIORef H.new

addToKeepAlive :: RValue -> IO ()
addToKeepAlive ptr =
    atomicModifyIORef' keepAliveSet $ \h -> (H.insert ptr (), ())

removeFromKeepAlive :: RValue -> IO ()
removeFromKeepAlive ptr =
    atomicModifyIORef' keepAliveSet $ \h -> (H.delete ptr (), ())

--------------------------------------------------------------------------------

-- TODO: use (show $ typeOf ex) to get the exception type, and use custom Ruby
--       exception (instead of `raise <some-string>`.
defMethod :: (ForeignPtr RValue)
          -> String
          -> ((ForeignPtr RValue) -> [(ForeignPtr RValue)] -> Maybe (ForeignPtr RValue) -> IO (ForeignPtr RValue))
          -> IO ()
defMethod klass methodName fun =
    withCString methodName $ \cname ->
        do callback <- (mkCallback $ \argc argv self exc pinArray proc ->
                do args <- peekArray (fromIntegral argc) argv
                   let mproc = if (proc == rbNil) then Nothing else Just proc
                   res <- (fun self args mproc)
                            `catches` [Handler (\ (RubyException exceptionVal) ->
                                            do poke exc exceptionVal
                                               return rbNil),
                                       Handler (\ (SomeException ex) ->
                                            do msg <- toRubyString (show ex)
                                               poke exc msg
                                               return rbNil)]
                   return res)
           withForeignPtr klass $ \klassp ->
               rb_define_method klassp cname callback

defSingleton :: (ForeignPtr RValue)
             -> String
             -> ((ForeignPtr RValue) -> [(ForeignPtr RValue)] -> Maybe (ForeignPtr RValue) -> IO (ForeignPtr RValue))
             -> IO ()
defSingleton obj methodName fun =
    do singleton <- getSingleton obj
       withForeignPtr singleton $ \singletonp ->
           defMethod singletonp methodName fun

mkProc :: ((ForeignPtr RValue) -> [(ForeignPtr RValue)] -> Maybe (ForeignPtr RValue) -> IO (ForeignPtr RValue))
       -> IO (ForeignPtr RValue)
mkProc fun =
    do objectKlass <- rbObject
       procKlass <- rbProc
       constructor <- peek rupee_proc_constructor >>=
       callable <- rbCall objectKlass "new" [] Nothing
       defSingleton callable "call" $ \_ (self:args) blk -> fun self args blk
       rbCall constructor "call" [callable] Nothing

getSingleton :: (ForeignPtr RValue) -> IO (ForeignPtr RValue)
getSingleton obj = 
    rb_get_singleton obj rbNil

toSymbol :: String -> IO (ForeignPtr RValue)
toSymbol str =
    withCString str $ \cstr ->
        rb_str_to_symbol cstr (fromIntegral $ length str)

rbIntern :: String -> IO RID
rbIntern str = withCString str rb_intern 

rbCall :: (ForeignPtr RValue)
       -> String
       -> [(ForeignPtr RValue)]
       -> Maybe (ForeignPtr RValue)
       -> IO (ForeignPtr RValue)
rbCall obj method argv mblk = do
    mid <- withCString method rb_intern 
    let blk = if isJust mblk then fromJust mblk else rbNil
    withForeignPtr obj $ \objp ->
        withForeignPtr blk $ \blkp ->
            withArray argv $ \args ->
                with (Dispatch objp mid (fromIntegral . length $ argv) args blkp) $ \dispatch ->
                    alloca $ \state -> do
                        result <- rupee_funcall dispatch state rbNil
                        addToKeepAlive result
                        didFail <- (/= 0) <$> peek state 
                        when didFail $ do
                            exception <- rb_errinfo
                            throw $ RubyException exception
                        return result

toRubyString :: String -> IO (ForeignPtr RValue)
toRubyString str =
    withCString str $ \cstr ->
        do rbstring <- rb_str_new2 cstr
           addToKeepAlive rbstring
           return rbstring

toString :: (ForeignPtr RValue) -> IO String
toString obj =
    withForeignPtr obj $ \objp ->
        do t <- rtype objp
           if t == RString
             then do cstr <- rb2cstr objp
                     len  <- rb_str_len objp
                     peekCStringLen (cstr, fromIntegral len)
             else error "Object was not a string"

rbEval :: String -> IO (ForeignPtr RValue)
rbEval src =
    do (val, state) <- alloca $ \state ->
           withCString src $ \cstr -> do
               (,) <$> rupee_eval_string cstr state rbNil <*> peek state
       when (state /= 0) $ do
           exception <- rb_errinfo
           throw $ RubyException exception
       return val

rbGetError :: IO (ForeignPtr RValue)
rbGetError =
    do exception <- rb_errinfo
       addToKeepAlive exception
       throw $ RubyException exception

rvalueToForeign :: RValue -> IO (ForeignPtr (RValue))
rvalueToForeign obj =
    do newForeignPtr_
       newForeignPtr obj

foreign import ccall safe "ruby_init"                              ruby_init                           :: IO ()
foreign import ccall safe "ruby_finalize"                          ruby_finalize                       :: IO ()
foreign import ccall safe "ruby_init_loadpath"                     ruby_init_loadpath                  :: IO ()
foreign import ccall safe "rupee_eval_string"                      rupee_eval_string                   :: CString -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rb_intern"                              rb_intern                           :: CString -> IO RID
foreign import ccall safe "rb_gc_register_mark_object"             rb_gc_register_mark_object          :: RValue -> IO ()
foreign import ccall safe "rb_gc_register_address"                 rb_gc_register_address              :: Ptr RValue -> IO ()
foreign import ccall safe "rb_gc_unregister_address"               rb_gc_unregister_address            :: Ptr RValue -> IO ()
foreign import ccall safe "rb_errinfo"                             rb_errinfo                          :: IO RValue
foreign import ccall safe "intern.h rb_ary_entry"                  rb_ary_entry                        :: RValue -> CLong -> IO RValue
foreign import ccall safe "rupee_rb_ary_len"                       rb_ary_len                          :: RValue -> IO CUInt
foreign import ccall safe "rupee_rb2cstr"                          rb2cstr                             :: RValue -> IO CString
foreign import ccall safe "rupee_funcall"                          rupee_funcall                       :: Ptr Dispatch -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rupee_rb_type"                          rb_type                             :: RValue -> IO CChar
foreign import ccall safe "rupee_rb_str_len"                       rb_str_len                          :: RValue -> IO CLong
foreign import ccall safe "rb_str_new"                             rb_str_new                          :: CString -> CLong -> IO RValue
foreign import ccall safe "rb_str_new2"                            rb_str_new2                         :: CString -> IO RValue
foreign import ccall safe "rupee_define_method"                    rb_define_method                    :: RValue -> CString -> FunPtr (HSCallback) -> IO ()
foreign import ccall safe "rupee_rb_get_singleton"                 rb_get_singleton                    :: RValue -> RValue -> IO RValue
foreign import ccall safe "rupee_init"                             rupee_init                          :: (FunPtr (FunPtr a -> IO ())) -> IO ()
foreign import ccall safe "rupee_rb_str_to_symbol"                 rb_str_to_symbol                    :: CString -> CLong -> IO RValue
foreign import ccall safe "&rb_gc_register_mark_object"            rb_gc_register_mark_object_funptr   :: FunPtr (RValue -> IO ())
foreign import ccall safe "&rb_gc_unregister_address"              rb_gc_unregister_address_funptr     :: FunPtr (RValue -> IO ())

foreign import ccall safe "wrapper"                                mkCallback                          :: (HSCallback) -> IO (FunPtr (HSCallback))
foreign import ccall safe "wrapper"                                mkMarkCB                            :: (FunPtr a) -> IO (FunPtr (a))
foreign import ccall safe "wrapper"                                mkDecRef                            :: ((Ptr RValue) -> IO ()) -> IO (FunPtr ((Ptr RValue) -> IO ()))

foreign import ccall safe "&rupee_proc_constructor"                rupee_proc_constructor              :: Ptr RValue
foreign import ccall safe "&hs_free_fun_ptr"                       hs_free_fun_ptr                     :: FunPtr (FunPtr a -> IO ())
foreign import ccall safe "rupee_wrap_struct"                      rupee_wrap_struct                   :: RValue -> (FunPtr (Ptr a)) -> (FunPtr (Ptr a)) -> Ptr a -> IO RValue

{- VALUE rb_define_class(const char*,VALUE); -}
{- VALUE rb_define_module(const char*); -}
{- VALUE rb_define_class_under(VALUE, const char*, VALUE); -}
{- VALUE rb_define_module_under(VALUE, const char*); -}
{- void rb_undef_method(VALUE,const char*); -}
{- void rb_define_alias(VALUE,const char*,const char*); -}
{- void rb_define_attr(VALUE,const char*,int,int); -}
{- void rb_define_const(VALUE,const char*,VALUE); -}
{- void rb_include_module(VALUE,VALUE); -}
{- void rb_extend_object(VALUE,VALUE); -}
{- void rb_prepend_module(VALUE,VALUE); -}

