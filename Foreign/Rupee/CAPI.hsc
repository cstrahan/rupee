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
import Control.Monad.Reader
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

rtype :: RValue -> Ruby RType
rtype v =
    do typ <- lift $ rb_type v
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

evalRubyT :: RValue -> RubyT IO a -> IO a
evalRubyT ary m = runReaderT (runRubyT m) ary

runRuby :: Ruby a -> IO ()
runRuby m =
    do initRupee
       withRegisteredArray (flip evalRubyT (m >> return ()))

withRegisteredArray :: (RValue -> IO a) -> IO a
withRegisteredArray f =
    alloca $ \aryptr -> do
        bracket_
            (rb_gc_register_address aryptr)
            (rb_gc_unregister_address aryptr)
            (peek aryptr >>= f)

-- TODO: use (show $ typeOf ex) to get the exception type, and use custom Ruby
--       exception (instead of `raise <some-string>`.
defMethod :: RValue
          -> String
          -> (RValue -> [RValue] -> Maybe RValue -> Ruby RValue)
          -> Ruby ()
defMethod klass methodName fun =
    RubyT $ lift $ withCString methodName $ \cname ->
        do callback <- (mkCallback $ \argc argv self exc pinArray proc ->
                do args <- peekArray (fromIntegral argc) argv
                   let mproc = if (proc == rbNil) then Nothing else Just proc
                   res <- evalRubyT pinArray (fun self args mproc)
                                `catches` [Handler (\ (RubyException exceptionVal) ->
                                                do poke exc exceptionVal
                                                   return rbNil),
                                           Handler (\ (SomeException ex) ->
                                                do msg <- evalRubyT pinArray $ toRubyString (show ex)
                                                   poke exc msg
                                                   return rbNil)]
                   return res)
           rb_define_method klass cname callback

defMethodWithSuper :: RValue
                   -> String
                   -> (RValue -> Maybe ([RValue] -> Maybe RValue -> Ruby RValue) -> [RValue] -> Maybe RValue -> Ruby RValue)
                   -> Ruby ()
defMethodWithSuper klass methodName fun =
    do name <- toRubyString methodName
       def <- mkProcWithSuper fun
       rbCall klass "define_method" [name] (Just def)
       return ()

defSingleton :: RValue
             -> String
             -> (RValue -> [RValue] -> Maybe RValue -> Ruby RValue)
             -> Ruby ()
defSingleton obj methodName fun =
    do singleton <- getSingleton obj
       defMethod singleton methodName fun

mkProc :: (RValue -> [RValue] -> Maybe RValue -> Ruby RValue)
       -> Ruby RValue
mkProc fun =
    do objectKlass <- rbObject
       constructor <- lift $ peek rupee_proc_constructor
       callable <- rbCall objectKlass "new" [] Nothing
       defSingleton callable "call" $ \_ (self:args) blk -> fun self args blk
       rbCall constructor "call" [callable] Nothing

mkProcWithSuper :: (RValue -> Maybe ([RValue] -> Maybe RValue -> Ruby RValue) -> [RValue] -> Maybe RValue -> Ruby RValue)
                -> Ruby RValue
mkProcWithSuper fun =
    do objectKlass <- rbObject
       constructor <- lift $ peek rupee_proc_constructor_with_super
       callable <- rbCall objectKlass "new" [] Nothing
       defSingleton callable "call" $ \_ (self:superProc:args) blk ->
           do let super = if superProc == rbNil
                            then Nothing
                            else Just $ \sargs sblk -> rbCall superProc "call" sargs sblk
              fun self super args blk
       rbCall constructor "call" [callable] Nothing

getSingleton :: RValue -> Ruby RValue
getSingleton obj = 
    RubyT $ do
        pinningArray <- ask
        lift $ rb_get_singleton obj pinningArray

toSymbol :: String -> Ruby RValue
toSymbol str =
    lift $ withCString str $ \cstr -> rb_str_to_symbol cstr (fromIntegral $ length str)

rbIntern :: String -> Ruby RID
rbIntern str = lift $ withCString str rb_intern 

rbCall :: RValue
       -> String
       -> [RValue]
       -> Maybe RValue
       -> Ruby RValue
rbCall obj method argv mblk =
    RubyT $ do
        pinningArray <- ask
        lift $ withArray argv $ \ary -> do
            mid <- withCString method rb_intern 
            let blk = if isJust mblk then fromJust mblk else rbNil
            with (Dispatch obj mid (fromIntegral . length $ argv) ary blk) $ \dispatch ->
                alloca $ \state -> do
                    result <- rupee_funcall dispatch state pinningArray
                    didFail <- (/= 0) <$> peek state 
                    when didFail $ do
                        exception <- rb_errinfo
                        throw $ RubyException exception
                    return result

toRubyString :: String -> Ruby RValue
toRubyString str =
    lift $ withCString str $ \cstr ->
        rb_str_new2 cstr

toString :: RValue -> Ruby String
toString obj =
    do t <- rtype obj
       if t == RString
         then lift $ do
             cstr <- rb2cstr obj
             len  <- rb_str_len obj
             peekCStringLen (cstr, fromIntegral len)
         else lift $ error "Object was not a string"

rbEval :: String -> Ruby RValue
rbEval src =
    RubyT $ do
        pinningArray <- ask
        (val, state) <- lift $ alloca $ \state ->
            withCString src $ \cstr -> do
                (,) <$> rupee_eval_string cstr state pinningArray <*> peek state
        when (state /= 0) $ lift $ do
            exception <- rb_errinfo
            throw $ RubyException exception
        return val

initRupee :: IO ()
initRupee =
    do ruby_init
       ruby_init_loadpath
       rupee_init hs_free_fun_ptr

foreign import ccall safe "ruby_init"                             ruby_init                           :: IO ()
foreign import ccall safe "ruby_finalize"                         ruby_finalize                       :: IO ()
foreign import ccall safe "ruby_init_loadpath"                    ruby_init_loadpath                  :: IO ()
foreign import ccall safe "rupee_eval_string"                     rupee_eval_string                   :: CString -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rb_protect"                            rb_protect                          :: FunPtr (RValue -> RValue) -> RValue -> Ptr CInt -> IO RValue
foreign import ccall safe "rb_intern"                             rb_intern                           :: CString -> IO RID
foreign import ccall safe "rb_gc_register_mark_object"            rb_gc_register_mark_object          :: RValue -> IO ()
foreign import ccall safe "rb_gc_register_address"                rb_gc_register_address              :: Ptr RValue -> IO ()
foreign import ccall safe "rb_gc_unregister_address"              rb_gc_unregister_address            :: Ptr RValue -> IO ()
foreign import ccall safe "rb_errinfo"                            rb_errinfo                          :: IO RValue
foreign import ccall safe "intern.h rb_ary_entry"                 rb_ary_entry                        :: RValue -> CLong -> IO RValue
foreign import ccall safe "rupee_rb_ary_len"                      rb_ary_len                          :: RValue -> IO CUInt
foreign import ccall safe "rb_ary_new"                            rb_ary_new                          :: IO RValue
foreign import ccall safe "rupee_rb2cstr"                         rb2cstr                             :: RValue -> IO CString
foreign import ccall safe "rupee_funcall"                         rupee_funcall                       :: Ptr Dispatch -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rupee_rb_type"                         rb_type                             :: RValue -> IO CChar
foreign import ccall safe "rupee_rb_str_len"                      rb_str_len                          :: RValue -> IO CLong
foreign import ccall safe "rb_str_new"                            rb_str_new                          :: CString -> CLong -> IO RValue
foreign import ccall safe "rb_str_new2"                           rb_str_new2                         :: CString -> IO RValue
foreign import ccall safe "rupee_define_method"                   rb_define_method                    :: RValue -> CString -> FunPtr (HSCallback) -> IO ()
foreign import ccall safe "rupee_rb_get_singleton"                rb_get_singleton                    :: RValue -> RValue -> IO RValue
foreign import ccall safe "rupee_init"                            rupee_init                          :: (FunPtr (FunPtr a -> IO ())) -> IO ()
foreign import ccall safe "rupee_rb_str_to_symbol"                rb_str_to_symbol                    :: CString -> CLong -> IO RValue
foreign import ccall safe "rb_define_class_under"                 rb_define_class_under               :: RValue -> CString -> RValue -> IO RValue -- outer, name, super
foreign import ccall safe "rb_define_alloc_func"                  rb_define_alloc_func                :: RValue -> FunPtr (RValue -> IO RValue) -> IO ()
foreign import ccall safe "rb_data_object_alloc"                  rb_data_object_alloc                :: RValue -> Ptr () -> FunPtr (Ptr () -> IO ()) -> FunPtr (Ptr () -> IO ()) -> IO RValue -- klass, sval, mark, free

foreign import ccall safe "wrapper"                               mkCallback                          :: (HSCallback) -> IO (FunPtr (HSCallback))

foreign import ccall safe "&rupee_proc_constructor"               rupee_proc_constructor              :: Ptr RValue
foreign import ccall safe "&rupee_proc_constructor_with_super"    rupee_proc_constructor_with_super   :: Ptr RValue
foreign import ccall safe "&hs_free_fun_ptr"                      hs_free_fun_ptr                     :: FunPtr (FunPtr a -> IO ())

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
