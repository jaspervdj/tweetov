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
    { tweetBody :: Text
    , tweetId   :: Integer
    } deriving (Show, Eq, Ord)
