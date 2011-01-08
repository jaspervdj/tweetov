{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import System.Random (RandomGen, newStdGen, randoms)
import Data.Monoid (mempty, mempty)

import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString as SB
import Snap.Types
import Snap.Http.Server (httpServe) 
import Snap.Http.Server.Config ( Config, defaultConfig
                               , setAccessLog, setErrorLog
                               )
import Snap.Util.FileServe (fileServe)
import qualified Data.Text.Encoding as T
import Text.Blaze (unsafeByteString)

import Twitter
import Twitter.Parse
import Twitter.Markov
import Twitter.Redis
import Templates

-- | Site root
--
root :: Snap ()
root = setBlaze $ rootTemplate mempty mempty

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
tweet gen = withParam "data" $ \json -> withParam "user" $ \u -> do
    let ut = getUserTweets $ decode $ SB.unpack json
        user' = T.decodeUtf8 u
        r = randoms gen
    case ut of
        Nothing -> addBlaze "Parse error."
        Just tweets -> do
            let tweet' = markovTweet user' tweets r
            id' <- liftIO $ storeTweet tweet'
            addBlaze $ tweetSection tweet' id'

-- | Request a user
--
user :: Snap ()
user = withParam "data" $ \json ->
    let ui = getUserInfo $ decode $ SB.unpack json
    in case ui of
        Nothing -> addBlaze "Parse error."
        Just userInfo -> addBlaze $ userSection userInfo

-- | Link to a tweet
--
perma :: Snap ()
perma = withParam "id" $ \id' -> do
    let nid = read $ decode $ SB.unpack id'
    t <- liftIO $ getTweet nid
    case t of
        Nothing -> setBlaze "Tweet not found."
        Just tweet' -> setBlaze $ rootTemplate (tweetSection tweet' nid)
                                               (setUser $ tweetAuthor tweet')

-- | Site handler
--
site :: Snap ()
site = do
    gen <- liftIO newStdGen
    fileServe "static" <|> route [ ("", ifTop root)
                                 , ("tweet/:id", tweet gen)
                                 , ("user/", user)
                                 , (":id", perma)
                                 ] 

-- | Main function
--
main :: IO ()
main = httpServe config site

-- | Site config
--
config :: Config Snap a
config = setAccessLog Nothing
       $ setErrorLog Nothing
       $ defaultConfig
