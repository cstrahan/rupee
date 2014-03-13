{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans
import Foreign.C.Types
import Foreign.C.String
import Data.Text

import Foreign.Rupee.CAPI
import Foreign.Rupee.Types
import Foreign.Rupee.Builtins

main :: IO ()
main =
    runRuby $ do
        demo `catch` \(RubyException val) ->
            do liftIO $ putStrLn "Caught Ruby exception: "
               msg <- rbCall val "to_s" [] Nothing
               str <- toString msg
               lift $ putStrLn str

demo :: Ruby ()
demo =
    do kernel <- rbKernel
       ints <- rbEval "[1,2,3,4,5]"
       proc <- mkProc $ \self (x:_) blk -> do puts [x]
       rbCall ints "each" [] (Just proc)
       liftIO $ putStrLn "DONE!!!!"
       return ()

puts :: [RValue] -> Ruby RValue
puts vals =
    do kernel <- rbKernel
       rbCall kernel "puts" vals Nothing
       return rbNil
