{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

#include "ruby.h"
#include "shim.h"

module Foreign.Rupee.CAPI where

import Foreign.Rupee.Builtins (rbObject, rbProc)
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

data RubyException = RubyException { rbException :: RValue } deriving (Show, Typeable)
instance Exception RubyException

newtype RValue = RValue (Ptr RValue) deriving (Eq, Ord, Show, Typeable, Data, Storable)
newtype RID    = RID (Ptr RID) deriving (Eq, Ord, Show, Typeable, Data, Storable)

newtype RBIO a = RBIO { runRBIO :: StateT RValue IO a }
                 deriving (Monad, MonadIO)

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

type HSCallback = CInt        -- argc
               -> Ptr RValue  -- argv
               -> RValue      -- self
               -> Ptr RValue  -- exc ptr, for indicating failure
               -> RValue      -- Array for registering Ruby objects
               -> RValue      -- Proc (Qnil if not given)
               -> IO RValue   -- result

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
                                                do msg <- toRubyString (show ex)
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
       -> RBIO 
mkProc fun =
    do objectKlass <- rbObject
       procKlass <- rbProc
       object <- rbCall objectKlass "new" [] Nothing
       defSingleton object "call" fun
       callSym <- toSymbol "call"
       method <- rbCall object "method" [callSym] Nothing
       rbCall procKlass "new" [] (Just method)

getSingleton :: RValue -> RBIO RValue
getSingleton obj = 
    RBIO $ do
        pinningArray <- get
        lift $ rb_get_singleton obj pinningArray

toSymbol :: String -> RBIO RValue
toSymbol str = lift $ withCString str rb_intern 

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

toRubyString :: String -> IO RValue
toRubyString str =
    withCString str $ \cstr ->
        rb_str_new2 cstr

toString :: RValue -> IO String
toString obj =
    do t <- rtype obj
       if t == RString
         then do cstr <- rb2cstr obj
                 len  <- rb_str_len obj
                 peekCStringLen (cstr, fromIntegral len)
         else error "Object was not a string"

rbEval :: String -> IO RValue
rbEval src = do
    (val, state) <- alloca $ \state ->
        withCString src $ \cstr -> do
            (,) <$> rb_eval_string_protect cstr state <*> peek state
    when (state /= 0) $ do
        exception <- rb_errinfo
        throw $ RubyException exception
    return val

foreign import ccall safe "ruby_init"                   ruby_init                    :: IO ()
foreign import ccall safe "ruby_finalize"               ruby_finalize                :: IO ()
foreign import ccall safe "ruby_init_loadpath"          ruby_init_loadpath           :: IO ()
foreign import ccall safe "rb_eval_string_protect"      rb_eval_string_protect       :: CString -> Ptr CInt -> IO RValue
foreign import ccall safe "rb_protect"                  rb_protect                   :: FunPtr (RValue -> RValue) -> RValue -> Ptr CInt -> IO RValue
foreign import ccall safe "rb_intern"                   rb_intern                    :: CString -> IO RID
foreign import ccall safe "rb_gc_register_mark_object"  rb_gc_register_mark_object   :: RValue -> IO ()
foreign import ccall safe "rb_gc_register_address"      rb_gc_register_address       :: Ptr RValue -> IO ()
foreign import ccall safe "rb_gc_unregister_address"    rb_gc_unregister_address     :: Ptr RValue -> IO ()
foreign import ccall safe "rb_errinfo"                  rb_errinfo                   :: IO RValue
foreign import ccall safe "intern.h rb_ary_entry"       rb_ary_entry                 :: RValue -> CLong -> IO RValue
foreign import ccall safe "rupee_rb_ary_len"            rb_ary_len                   :: RValue -> IO CUInt
foreign import ccall safe "rupee_rb2cstr"               rb2cstr                      :: RValue -> IO CString
foreign import ccall safe "rupee_rb_funcall2"           rb_funcall2                  :: Ptr Dispatch -> Ptr CInt -> RValue -> IO RValue
foreign import ccall safe "rupee_rb_type"               rb_type                      :: RValue -> IO CChar
foreign import ccall safe "rupee_rb_str_len"            rb_str_len                   :: RValue -> IO CLong
foreign import ccall safe "rb_str_new"                  rb_str_new                   :: CString -> CLong -> IO RValue
foreign import ccall safe "rb_str_new2"                 rb_str_new2                  :: CString -> IO RValue
foreign import ccall safe "rupee_define_method"         rb_define_method             :: RValue -> CString -> FunPtr (HSCallback) -> IO ()
foreign import ccall safe "rupee_rb_get_singleton"      rb_get_singleton             :: RValue -> IO RValue
foreign import ccall safe "rupee_register_funptr_free"  register_funptr_free         :: (FunPtr (FunPtr a -> IO ())) -> IO ()
foreign import ccall safe "wrapper"                     mkCallback                   :: (HSCallback) -> IO (FunPtr (HSCallback))

foreign import ccall safe "&" hs_free_fun_ptr   :: FunPtr (FunPtr a -> IO ())
