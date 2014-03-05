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
import Foreign.Storable
import Data.Data
import Data.Maybe
import Data.Word
import Data.Typeable

rbFalse, rbTrue, rbNil, rbUndef :: RValue
rbFalse = RValue $ intPtrToPtr #{const Qfalse}
rbTrue  = RValue $ intPtrToPtr #{const Qtrue}
rbNil   = RValue $ intPtrToPtr #{const Qnil}
rbUndef = RValue $ intPtrToPtr #{const Qundef}

rtype :: RValue -> IO RType
rtype v =
    do typ <- rb_type v
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

evalRBIO :: RValue -> RBIO a -> IO a
evalRBIO ary m = evalStateT (runRBIO m) ary

-- TODO: use (show $ typeOf ex) to get the exception type, and use custom Ruby
--       exception (instead of `raise <some-string>`.
defMethod :: RValue
          -> String
          -> (RValue -> [RValue] -> Maybe RValue -> RBIO RValue)
          -> RBIO ()
defMethod klass methodName fun =
    liftIO $ withCString methodName $ \cname ->
        do callback <- (mkCallback $ \argc argv self exc pinArray proc ->
                do args <- peekArray (fromIntegral argc) argv
                   let mproc = if (proc == rbNil) then Nothing else Just proc
                   res <- evalRBIO pinArray (fun self args mproc)
                                `catches` [Handler (\ (RubyException exceptionVal) ->
                                                do poke exc exceptionVal
                                                   return rbNil),
                                           Handler (\ (SomeException ex) ->
                                                do msg <- evalRBIO pinArray $ toRubyString (show ex)
                                                   poke exc msg
                                                   return rbNil)]
                   return res)
           rb_define_method klass cname callback

defSingleton :: RValue
             -> String
             -> (RValue -> [RValue] -> Maybe RValue -> RBIO RValue)
             -> RBIO ()
defSingleton obj methodName fun =
    do singleton <- getSingleton obj
       defMethod singleton methodName fun

mkProc :: (RValue -> [RValue] -> Maybe RValue -> RBIO RValue)
       -> RBIO RValue
mkProc fun =
    do objectKlass <- rbObject
       procKlass <- rbProc
       constructor <- liftIO $ peek rupee_proc_constructor
       callable <- rbCall objectKlass "new" [] Nothing
       defSingleton callable "call" $ \_ (self:args) blk -> fun self args blk
       rbCall constructor "call" [callable] Nothing

getSingleton :: RValue -> RBIO RValue
getSingleton obj = 
    RBIO $ do
        pinningArray <- get
        lift $ rb_get_singleton obj pinningArray

toSymbol :: String -> RBIO RValue
toSymbol str =
    RBIO $ lift $ withCString str $ \cstr -> rb_str_to_symbol cstr (fromIntegral $ length str)

rbIntern :: String -> RBIO RID
rbIntern str = RBIO $ lift $ withCString str rb_intern 

rbCall :: RValue
       -> String
       -> [RValue]
       -> Maybe RValue
       -> RBIO RValue
rbCall obj method argv mblk =
    RBIO $ do
        pinningArray <- get
        lift $ withArray argv $ \ary -> do
            mid <- withCString method rb_intern 
            let blk = if isJust mblk then fromJust mblk else rbNil
            with (Dispatch obj mid (fromIntegral . length $ argv) ary blk) $ \dispatch ->
                alloca $ \state -> do
                    result <- rb_funcall2 dispatch state pinningArray
                    didFail <- (/= 0) <$> peek state 
                    when didFail $ do
                        exception <- rb_errinfo
                        throw $ RubyException exception
                    return result

toRubyString :: String -> RBIO RValue
toRubyString str =
    RBIO $ lift $ withCString str $ \cstr ->
        rb_str_new2 cstr

toString :: RValue -> IO String
toString obj =
    do t <- rtype obj
       if t == RString
         then do cstr <- rb2cstr obj
                 len  <- rb_str_len obj
                 peekCStringLen (cstr, fromIntegral len)
         else error "Object was not a string"

rbEval :: String -> RBIO RValue
rbEval src =
    RBIO $ do
        pinningArray <- get
        (val, state) <- lift $ alloca $ \state ->
            withCString src $ \cstr -> do
                (,) <$> rupee_eval_string cstr state pinningArray <*> peek state
        when (state /= 0) $ lift $ do
            exception <- rb_errinfo
            throw $ RubyException exception
        return val

rupeeInit :: IO ()
rupeeInit =
    do ruby_init
       ruby_init_loadpath
       rupee_init hs_free_fun_ptr

foreign import ccall safe "ruby_init"                      ruby_init                       :: IO ()
foreign import ccall safe "ruby_finalize"                  ruby_finalize                   :: IO ()
foreign import ccall safe "ruby_init_loadpath"             ruby_init_loadpath              :: IO ()
foreign import ccall safe "rupee_eval_string"              rupee_eval_string               :: CString -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rb_protect"                     rb_protect                      :: FunPtr (RValue -> RValue) -> RValue -> Ptr CInt -> IO RValue
foreign import ccall safe "rb_intern"                      rb_intern                       :: CString -> IO RID
foreign import ccall safe "rb_gc_register_mark_object"     rb_gc_register_mark_object      :: RValue -> IO ()
foreign import ccall safe "rb_gc_register_address"         rb_gc_register_address          :: Ptr RValue -> IO ()
foreign import ccall safe "rb_gc_unregister_address"       rb_gc_unregister_address        :: Ptr RValue -> IO ()
foreign import ccall safe "rb_errinfo"                     rb_errinfo                      :: IO RValue
foreign import ccall safe "intern.h rb_ary_entry"          rb_ary_entry                    :: RValue -> CLong -> IO RValue
foreign import ccall safe "rupee_rb_ary_len"               rb_ary_len                      :: RValue -> IO CUInt
foreign import ccall safe "rupee_rb2cstr"                  rb2cstr                         :: RValue -> IO CString
foreign import ccall safe "rupee_rb_funcall2"              rb_funcall2                     :: Ptr Dispatch -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rupee_rb_type"                  rb_type                         :: RValue -> IO CChar
foreign import ccall safe "rupee_rb_str_len"               rb_str_len                      :: RValue -> IO CLong
foreign import ccall safe "rb_str_new"                     rb_str_new                      :: CString -> CLong -> IO RValue
foreign import ccall safe "rb_str_new2"                    rb_str_new2                     :: CString -> IO RValue
foreign import ccall safe "rupee_define_method"            rb_define_method                :: RValue -> CString -> FunPtr (HSCallback) -> IO ()
foreign import ccall safe "rupee_rb_get_singleton"         rb_get_singleton                :: RValue -> RValue -> IO RValue
foreign import ccall safe "rupee_init"                     rupee_init                      :: (FunPtr (FunPtr a -> IO ())) -> IO ()
foreign import ccall safe "rupee_rb_str_to_symbol"         rb_str_to_symbol                :: CString -> CLong -> IO RValue

foreign import ccall      "&rupee_proc_constructor"        rupee_proc_constructor          :: Ptr RValue

foreign import ccall safe "wrapper"                        mkCallback                      :: (HSCallback) -> IO (FunPtr (HSCallback))

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

foreign import ccall safe "&" hs_free_fun_ptr   :: FunPtr (FunPtr a -> IO ())
