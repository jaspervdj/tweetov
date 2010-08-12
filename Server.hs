{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import System.Random (RandomGen, newStdGen, randoms)
import System.Environment (getArgs)

import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString as SB
import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)

import Twitter.Parse
import Twitter.Markov
import Templates

-- | Site root
--
root :: Snap ()
root = setBlaze rootTemplate

-- | Request a tweet
--
tweet :: RandomGen g => g -> Snap ()
tweet gen = getParam "data" >>= \i -> case i of
    Nothing -> addBlaze "Params incomplete."
    Just json -> do
        let ut = getUserTweets $ decode $ SB.unpack json
        case ut of
            Nothing -> addBlaze "Parse error."
            Just tweets ->
                addBlaze $ tweetSection $ markovTweet tweets $ randoms gen

-- | Request a user
--
user :: Snap ()
user = getParam "data" >>= \i -> case i of
    Nothing -> addBlaze "Params incomplete."
    Just json -> do
        let ui = getUserInfo $ decode $ SB.unpack json
        case ui of
            Nothing -> addBlaze "Parse error."
            Just userInfo -> addBlaze $ userSection userInfo

-- | Site handler
--
site :: Snap ()
site = do
    gen <- liftIO newStdGen
    route [ ("", ifTop root)
          , ("tweet/:id", tweet gen)
          , ("user/", user)
          ] <|> fileServe "static"

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    let port = case args of [p] -> read p
                            _   -> 8000
    httpServe "*" port "tweetov" (Just "access.log") (Just "error.log") site
