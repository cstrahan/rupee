{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Foreign.C.Types
import Foreign.C.String
import Foreign.Rupee.CAPI
import Foreign.Marshal
import Foreign.Storable
import Data.Text

{- main :: IO () -}
{- main = -}
    {- do ruby_init -}
       {- ruby_init_loadpath -}
       {- ex <- rbEval "'THIS IS A BIG ASS STRING'" -}
       {- rbCall rbTrue "raise" [ex] -}

       {- putStrLn "DONE!!!!" -}
       {- return () -}
       {- forever $ rbEval "puts '=====> this is ruby!'" -}
main :: IO ()
main = main2 `catch`  \(RubyException val) ->
    do putStrLn "Caught Ruby exception: "
       msg <- evalRBIO rbNil $ rbCall val "to_s" [] Nothing
       str <- toString msg
       putStrLn str

main2 :: IO ()
main2 =
    do ruby_init
       ruby_init_loadpath
       register_funptr_free hs_free_fun_ptr

       hello <- rbEval "'hello'"
       world <- rbEval "'world'"
       string_class <- rbEval "String"
       block <- rbEval "Proc.new {|x| puts self.class }"
       {- evalRBIO rbNil $ defMethod obj "hello" (\self args Nothing -> -}
       {- evalRBIO rbNil $ defMethod string_class "hello" (\self [arg1] (Just blk) -> -}
                {- rbCall hello "instance_eval" [] (Just blk) -}
                {- rbCall blk "call" [] (Just blk) -}
                {- rbCall rbTrue "puts" [hello] Nothing -}
           {- ) -}
       evalRBIO rbNil $ rbCall hello "puts" [rb_cString] Nothing

       putStrLn "DONE!!!!"
       return ()
