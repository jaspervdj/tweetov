-- | Web application
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Application
    ( application
    ) where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import System.Random (RandomGen, newStdGen, randoms)
import Data.Monoid (mempty, mempty)

import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString as SB
import Snap.Types
import Snap.Util.FileServe (serveDirectory)
import qualified Data.Text.Encoding as T
import Text.Blaze (Html, unsafeByteString)
import Text.Blaze.Renderer.Utf8 (renderHtml)

import Tweetov.Twitter
import Tweetov.Twitter.Parse
import Tweetov.Twitter.Markov
import Tweetov.Twitter.Redis
import qualified Tweetov.Views as Views

-- | Send blaze output to snap.
--
setBlaze :: Html -> Snap ()
setBlaze response = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml response

-- | Site root
--
root :: Snap ()
root = setBlaze $ Views.root mempty mempty

-- | Progress using a parameter
--
withParam :: SB.ByteString               -- ^ Param name
          -> (SB.ByteString -> Snap ())  -- ^ Handler
          -> Snap ()                     -- ^ Result
withParam name handler = do
    value <- getParam name
    case value of
        Just v  -> handler v
        Nothing -> setBlaze $ do
            "Error: param "
            unsafeByteString name
            " not set"

-- | Request a tweet
--
generateTweet :: RandomGen g => g -> Snap ()
generateTweet gen = withParam "data" $ \json -> withParam "user" $ \u -> do
    let ut = parseTweets $ decode $ SB.unpack json
        user' = T.decodeUtf8 u
        randoms' = randoms gen
    case ut of
        Nothing -> setBlaze "Parse error."
        Just tweets -> do
            let tweet' = markovTweet user' tweets randoms'
            id' <- liftIO $ withRedis $ \r -> storeTweet r tweet'
            setBlaze $ Views.tweet tweet' id'

-- | Link to a tweet
--
tweet :: Snap ()
tweet = withParam "id" $ \id' -> do
    let nid = read $ decode $ SB.unpack id'
    t <- liftIO $ withRedis $ \r -> getTweet r nid
    case t of
        Nothing -> setBlaze "Tweet not found."
        Just tweet' -> setBlaze $ Views.root
            (Views.tweet tweet' nid)
            (Views.userAjax $ tweetAuthor tweet')

-- | Request a user
--
user :: Snap ()
user = withParam "data" $ \json ->
    let ui = parseUser $ decode $ SB.unpack json
    in case ui of
        Nothing ->       setBlaze "Parse error."
        Just userInfo -> setBlaze $ Views.user userInfo

-- | Site handler
--
application :: Snap ()
application = do
    gen <- liftIO newStdGen
    serveDirectory "static" <|> route [ ("", ifTop root)
                                      , ("tweet/:id", generateTweet gen)
                                      , ("user/", user)
                                      , (":id", tweet)
                                      ] 
