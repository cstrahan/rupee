{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Data.Text

import Foreign.Rupee.CAPI
import Foreign.Rupee.Types
import Foreign.Rupee.Builtins

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
main =
    do rupeeInit
       demo `catch`  \(RubyException val) ->
           do putStrLn "Caught Ruby exception: "
              msg <- evalRBIO rbNil $ rbCall val "to_s" [] Nothing
              str <- toString msg
              putStrLn str

demo :: IO ()
demo = do
    evalRBIO rbNil $
        do hello <- rbEval "'hello'"
           world <- rbEval "'world'"
           string_class <- rbString
           {- block <- rbEval "Proc.new {|x| puts self.class }" -}
           block <- mkProc $ \self args blk -> rbCall hello "puts" [self] Nothing
           {- rbCall block "call" [] Nothing -}
           rbCall hello "instance_eval" [] (Just block)
           {- evalRBIO rbNil $ defMethod obj "hello" (\self args Nothing -> -}
           {- evalRBIO rbNil $ defMethod string_class "hello" (\self [arg1] (Just blk) -> -}
                    {- rbCall hello "instance_eval" [] (Just blk) -}
                    {- rbCall blk "call" [] (Just blk) -}
                    {- rbCall rbTrue "puts" [hello] Nothing -}
               {- ) -}
           {- rbCall hello "puts" [string_class] Nothing -}

           liftIO $ putStrLn "DONE!!!!"
           return ()
