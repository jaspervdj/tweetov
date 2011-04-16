{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Snap.Types (Snap)
import Snap.Http.Server (httpServe) 
import Snap.Http.Server.Config ( Config, defaultConfig
                               , setAccessLog, setErrorLog
                               )

import Tweetov.Application

-- | Main function
--
main :: IO ()
main = httpServe config application

-- | Site config
--
config :: Config Snap a
config = setAccessLog Nothing
       $ setErrorLog Nothing
       $ defaultConfig
