{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Twitter.Redis
    ( storeTweet
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
    redis <- connect localhost defaultPort
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

-- | Store an item in the redis database
--
setItem :: Redis -> SB.ByteString -> LB.ByteString -> IO ()
setItem redis key item = do
    _ <- set redis key item
    return ()

-- | Store a tweet in the database
--
storeTweet :: TweetInfo -> IO Integer
storeTweet tweet = withRedis $ \redis -> do
    id' <- read . LBC.unpack . fromMaybe "1" <$> getItem redis "next-id"
    setItem redis (getTweetKey id') $ encode tweet
    setItem redis "next-id" $ LBC.pack $ show $ id' + 1
    return id'

-- | Get the number of tweets
--
getNumberOfTweets :: IO Integer
getNumberOfTweets = withRedis $ \redis -> do
    id' <- getItem redis "next-id"
    return $ fromMaybe 0 $ fmap (read . LBC.unpack) id'

-- | Get a tweet from the database
--
getTweet :: Integer -> IO (Maybe TweetInfo)
getTweet id' = withRedis $ \redis -> do
    item <- getItem redis $ getTweetKey id'
    return $ decode <$> item

-- | Get an id for a tweet
--
getTweetKey :: Integer -> SB.ByteString
getTweetKey id' = "tweet:" `mappend` SBC.pack (show id')
