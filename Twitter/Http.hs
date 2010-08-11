-- | Retrieve twitter info over HTTP.
--
module Twitter.Http
    ( getAccount
    , getUserInfo
    , getUserTweets
    ) where

import Control.Applicative ((<$>))
import Control.Monad (ap, liftM, liftM2)
import Data.Maybe (mapMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.JSON
import Text.Printf

import Twitter (Account (..), UserInfo (..), TweetInfo (..))

-- | Get a twitter account.
--
getAccount :: Text -> IO (Maybe Account)
getAccount name = do
    accountUser' <- getUserInfo name
    accountTweets' <- getUserTweets name
    return $ liftM2 Account accountUser' accountTweets'

-- | Load information about a twitter user
--
getUserInfo :: Text                 -- ^ Username
            -> IO (Maybe UserInfo)  -- ^ Result
getUserInfo name = do
    body <- getResponseBody =<< simpleHTTP (getRequest url)
    return $ do
        result <- resultToMaybe (decode body)
        case result of
            JSObject object -> liftM UserInfo
                (T.pack <$> getField "screen_name" object)
                    `ap` getField "followers_count" object
                    `ap` getField "friends_count" object
                    `ap` getField "statuses_count" object
                    `ap` (T.pack <$> getField "profile_image_url" object)
                    `ap` (T.pack <$> getField "name" object)
            _ -> Nothing
  where
    url = printf "http://api.twitter.com/1/users/show.json?screen_name=%s"
                 (T.unpack name)

-- | Get the tweets of a user
--
getUserTweets :: Text                    -- ^ Username
              -> IO (Maybe [TweetInfo])  -- ^ List of tweets
getUserTweets name = do
    body <- getResponseBody =<< simpleHTTP (getRequest url)
    return $ do
        result <- resultToMaybe $ decode body
        list <- case result of JSArray l -> return l
                               _         -> Nothing
        return $ mapMaybe getTweet list
  where
    url = printf
        "http://api.twitter.com/1/statuses/user_timeline.json?screen_name=%s&count=200"
        (T.unpack name)

-- | Pure function to parse in a tweet
--
getTweet :: JSValue -> Maybe TweetInfo
getTweet value = case value of
    JSObject object -> liftM TweetInfo (T.pack <$> getField "text" object)
                           `ap` getField "id" object
    _ -> Nothing 

-- | Auxiliary function: turn a 'Result' into a 'Maybe'.
--
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok x) = Just x
resultToMaybe (Error _) = Nothing

-- | Get a field from a JSON object into the 'Maybe' monad.
--
getField :: JSON a => String -> JSObject JSValue -> Maybe a
getField key = resultToMaybe . valFromObj key
