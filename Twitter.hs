-- | Twitter-related data structures.
--
module Twitter
    ( UserInfo (..)
    , TweetInfo (..)
    ) where

import Data.Text (Text)

-- | Information about a twitter user
--
data UserInfo = UserInfo
    { userName      :: Text
    , userFollowers :: Int
    , userFollowing :: Int
    , userTweets    :: Int
    , userImageUrl  :: Text
    , userRealName  :: Text
    } deriving (Show, Eq, Ord)

-- | A tweet
--
data TweetInfo = TweetInfo
    { tweetWords  :: [Text]
    , tweetAuthor :: Text
    } deriving (Show, Eq, Ord)
