-- | Twitter-related data structures.
--
module Twitter
    ( UserInfo (..)
    , TweetInfo (..)
    ) where

import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.Binary (Binary, get, put)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

instance Binary TweetInfo where
    put t = do
        put $ T.encodeUtf8 $ T.unwords $ tweetWords t
        put $ T.encodeUtf8 $ tweetAuthor t
    get = do
        w <- T.words . T.decodeUtf8 <$> get
        a <- T.decodeUtf8 <$> get
        return $ TweetInfo w a
