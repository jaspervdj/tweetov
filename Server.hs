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
import qualified Data.Text.Encoding as T
import Text.Blaze (unsafeByteString)

import Twitter.Parse
import Twitter.Markov
import Templates

-- | Site root
--
root :: Snap ()
root = setBlaze rootTemplate

-- | Progress using a parameter
--
withParam :: SB.ByteString               -- ^ Param name
          -> (SB.ByteString -> Snap ())  -- ^ Handler
          -> Snap ()                     -- ^ Result
withParam name handler = do
    value <- getParam name
    case value of
        Just v  -> handler v
        Nothing -> setBlaze $ do
            "Error: param "
            unsafeByteString name
            " not set"

-- | Request a tweet
--
tweet :: RandomGen g => g -> Snap ()
tweet gen = withParam "data" $ \json -> withParam "user" $ \user' -> do
    let ut = getUserTweets $ decode $ SB.unpack json
        user = T.decodeUtf8 user'
        r = randoms gen
    case ut of
        Nothing -> addBlaze "Parse error."
        Just tweets ->
            addBlaze $ tweetSection $ markovTweet user tweets r

-- | Request a user
--
user :: Snap ()
user = withParam "data" $ \json ->
    let ui = getUserInfo $ decode $ SB.unpack json
    in case ui of
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
    httpServe "*" port "tweetov" Nothing Nothing site
