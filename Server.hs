{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import System.Random (RandomGen, newStdGen, randoms)
import System.Environment (getArgs)

import qualified Data.Text.Encoding as T
import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Util.FileServe (fileServe)

import Twitter.Http
import Twitter.Markov
import Templates

-- | Site root
--
root :: Snap ()
root = setBlaze rootTemplate

-- | Request a tweet
--
tweet :: RandomGen g => g -> Snap ()
tweet gen = getParam "id" >>= \i -> case i of
    Nothing -> addBlaze "User not specified."
    Just userName -> do
        ut <- liftIO $ getUserTweets $ T.decodeUtf8 userName
        case ut of
            Nothing -> addBlaze "Twitter error."
            Just tweets -> do
                let model = fromSamples $ map fromTweet tweets
                    t = fromSample $ sentence model $ randoms gen
                addBlaze $ tweetSection t

-- | Request a user
--
user :: Snap ()
user = getParam "id" >>= \i -> case i of
    Nothing -> addBlaze "User not specified."
    Just userName -> do
        ui <- liftIO $ getUserInfo $ T.decodeUtf8 userName
        case ui of
            Nothing -> addBlaze "Twitter error."
            Just userInfo -> addBlaze $ userSection userInfo

-- | Site handler
--
site :: Snap ()
site = do
    gen <- liftIO newStdGen
    route [ ("", ifTop root)
          , ("tweet/:id", tweet gen)
          , ("user/:id", user)
          ] <|> fileServe "static"

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    let port = case args of [p] -> read p
                            _   -> 8000
    httpServe "*" port "tweetov" (Just "access.log") (Just "error.log") site
