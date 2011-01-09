-- | Analysis of the tweetov data
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Analysis
    ( perUserMap
    , perUserNumber
    ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid (mappend)
import qualified Data.Text as T

import Tweetov.Twitter
import Tweetov.Twitter.Redis

-- | Create a map with the tweets per user
--
perUserMap :: IO (Map Text (Vector TweetInfo))
perUserMap = do
    n <- getNumberOfTweets
    foldM addTweet M.empty [0 .. n]
  where
    addTweet map' n = getTweet n >>= \r -> return $ case r of
        Nothing -> map'
        Just ti -> M.insertWith mappend (author ti) (V.singleton ti) map'

    author = dropPrefix "@" . T.toLower . tweetAuthor

    dropPrefix p t | p `T.isPrefixOf` t = T.drop (T.length p) t
                   | otherwise          = t

-- | Create a map with the number of tweets per user
--
perUserNumber :: IO (Map Text Int)
perUserNumber = M.map V.length <$> perUserMap
