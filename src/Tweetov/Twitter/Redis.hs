{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Twitter.Redis
    ( withRedis
    , storeTweet
    , getTweet
    , getNumberOfTweets
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Binary (encode, decode)
import Database.Redis.Redis
import Data.Monoid (mappend)

import Tweetov.Twitter

-- | Run an action with a redis connection
--
withRedis :: (Redis -> IO a) -> IO a
withRedis f = do
    redis <- connect "127.0.0.1" defaultPort
    x <- f redis
    disconnect redis
    return x

-- | Get an item from the redis database
--
getItem :: Redis -> SB.ByteString -> IO (Maybe LB.ByteString)
getItem redis key = do
    reply <- get redis key
    return $ case reply of RBulk (Just r) -> Just r
                           _              -> Nothing

-- | Generate a new tweet ID, atomically
--
getNewTweetID :: Redis -> IO Int
getNewTweetID redis = do
    reply <- incr redis ("next-id" :: SB.ByteString)
    return $ case reply of (RInt x) -> x
                           _        -> 0

-- | Store an item in the redis database
--
setItem :: Redis -> SB.ByteString -> LB.ByteString -> IO ()
setItem redis key item = do
    _ <- set redis key item
    return ()

-- | Store a tweet in the database
--
storeTweet :: Redis -> Tweet -> IO Int
storeTweet redis tweet = do
    id' <- getNewTweetID redis
    setItem redis (getTweetKey id') $ encode tweet
    return id'

-- | Get the number of tweets
--
getNumberOfTweets :: Redis -> IO Int
getNumberOfTweets redis = do
    id' <- getItem redis "next-id"
    return $ fromMaybe 0 $ fmap (read . LBC.unpack) id'

-- | Get a tweet from the database
--
getTweet :: Redis -> Int -> IO (Maybe Tweet)
getTweet redis id' = do
    item <- getItem redis $ getTweetKey id'
    return $ decode <$> item

-- | Get an id for a tweet
--
getTweetKey :: Int -> SB.ByteString
getTweetKey id' = "tweet:" `mappend` SBC.pack (show id')
