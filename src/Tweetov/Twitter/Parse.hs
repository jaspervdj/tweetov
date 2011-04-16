-- | Parse Twitter data.
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Twitter.Parse
    ( parseUser
    , parseTweets
    ) where

import Control.Applicative ((<$>))
import Control.Monad (ap, liftM)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import Text.JSON

import Tweetov.Twitter (User (..), Tweet (..))

-- | Load information about a twitter user
--
parseUser :: String      -- ^ JSON data
          -> Maybe User  -- ^ Result
parseUser body = do
    result <- resultToMaybe (decode body)
    case result of
        JSObject object -> liftM User
            (T.pack <$> getField "screen_name" object)
                `ap` getField "followers_count" object
                `ap` getField "friends_count" object
                `ap` getField "statuses_count" object
                `ap` (T.pack <$> getField "profile_image_url" object)
                `ap` (T.pack <$> getField "name" object)
        _ -> Nothing

-- | Get the tweets of a user
--
parseTweets :: String         -- ^ JSON
            -> Maybe [Tweet]  -- ^ List of tweets
parseTweets body = do
    result <- resultToMaybe $ decode body
    list <- case result of JSArray l -> return l
                           _         -> Nothing
    return $ mapMaybe parseTweet list

-- | Pure function to parse in a tweet
--
parseTweet :: JSValue -> Maybe Tweet
parseTweet value = case value of
    JSObject object ->
        liftM Tweet (T.words . T.pack <$> getField "text" object)
            -- Do not parse user for now
            `ap` return "unknown"
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
