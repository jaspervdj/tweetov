-- | Twitter-related data structuresh
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Twitter
    ( User (..)
    , Tweet (..)
    ) where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Data.Binary (Binary, get, put)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson

-- | Information about a twitter user
--
data User = User
    { userName      :: Text
    , userFollowers :: Int
    , userFollowing :: Int
    , userTweets    :: Int
    , userImageUrl  :: Text
    , userRealName  :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "screen_name"
        <*> o .: "followers_count"
        <*> o .: "friends_count"
        <*> o .: "statuses_count"
        <*> o .: "profile_image_url"
        <*> o .: "name"
    parseJSON _ = mzero

-- | A tweet
--
data Tweet = Tweet
    { tweetWords  :: [Text]
    , tweetAuthor :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON Tweet where
    parseJSON (Object o) = Tweet
        <$> (fmap T.words $ o .: "text")
        <*> pure "unknown"
    parseJSON _ = mzero

instance Binary Tweet where
    put t = do
        put $ T.encodeUtf8 $ T.unwords $ tweetWords t
        put $ T.encodeUtf8 $ tweetAuthor t
    get = do
        w <- T.words . T.decodeUtf8 <$> get
        a <- T.decodeUtf8 <$> get
        return $ Tweet w a
