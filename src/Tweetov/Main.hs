{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import System.Environment (getArgs, getProgName)
import qualified Data.ByteString.Char8 as B
import qualified Snap.Http.Server as Snap

import Tweetov.Application

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [address, port] -> Snap.httpServe (config address port) application
        _               -> putStrLn $
            "Usage: " ++ progName ++ " <listen address> <port>"
  where
    config address port =
		Snap.setBind (B.pack address) $
		Snap.setPort (read port) $
        Snap.setAccessLog Snap.ConfigNoLog $
        Snap.setErrorLog Snap.ConfigNoLog $
        Snap.emptyConfig
