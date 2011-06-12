{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Snap.Http.Server (httpServe) 
import Snap.Http.Server.Config ( ConfigListen (..)
                               , emptyConfig, addListen
                               , setAccessLog, setErrorLog
                               )
import System.Environment (getArgs, getProgName)
import qualified Data.ByteString.Char8 as B

import Tweetov.Application

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [address, port] -> httpServe (config address port) application
        _               -> putStrLn $
            "Usage: " ++ progName ++ " <listen address> <port>"
  where
    config address port =
        addListen (ListenHttp (B.pack address) (read port)) $
        setAccessLog Nothing $
        setErrorLog Nothing $
        emptyConfig
