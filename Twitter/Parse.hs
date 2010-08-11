-- | Parse Twitter data.
--
module Twitter.Parse
    ( getUserInfo
    , getUserTweets
    ) where

import Control.Applicative ((<$>))
import Control.Monad (ap, liftM)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import Text.JSON

import Twitter (UserInfo (..), TweetInfo (..))

-- | Load information about a twitter user
--
getUserInfo :: String          -- ^ JSON data
            -> Maybe UserInfo  -- ^ Result
getUserInfo body = do
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

-- | Get the tweets of a user
--
getUserTweets :: String             -- ^ JSON
              -> Maybe [TweetInfo]  -- ^ List of tweets
getUserTweets body = do
    result <- resultToMaybe $ decode body
    list <- case result of JSArray l -> return l
                           _         -> Nothing
    return $ mapMaybe getTweet list

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
