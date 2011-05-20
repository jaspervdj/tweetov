-- | Web application
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Application
    ( application
    ) where

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>), (<|>))
import System.Random (RandomGen, newStdGen, randoms)
import Data.Monoid (mempty)
import Control.Monad (join)

import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString as SB
import Snap.Types
import Snap.Util.FileServe (serveDirectory)
import qualified Data.Text.Encoding as T
import Text.Blaze (Html)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Data.Aeson (FromJSON (..), json)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec (parseOnly)

import Tweetov.Twitter
import Tweetov.Twitter.Markov
import Tweetov.Twitter.Redis
import qualified Tweetov.Views as Views

-- | Utility: parse JSON from a strict bytestring
--
parseJson :: FromJSON a => SB.ByteString -> Maybe a
parseJson bs = case parseOnly json bs >>= parseEither parseJSON of
    Left _  -> Nothing
    Right x -> x

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

-- | Request a tweet
--
generateTweet :: RandomGen g => g -> Snap ()
generateTweet gen = do
    Just d <- getParam "data"
    liftIO $ SB.writeFile "data.json" d
    Just tweets <- join . fmap parseJson <$> getParam "data"
    Just user' <- fmap T.decodeUtf8 <$> getParam "user"
    let randoms' = randoms gen
        tweet' = markovTweet user' tweets randoms'
    id' <- liftIO $ withRedis $ \r -> storeTweet r tweet'
    setBlaze $ Views.tweet tweet' id'

-- | Link to a tweet
--
tweet :: Snap ()
tweet = do
    Just id' <- fmap (read . decode . SB.unpack) <$> getParam "id"
    Just tweet' <- liftIO $ withRedis $ \r -> getTweet r id'
    setBlaze $ Views.root
        (Views.tweet tweet' id')
        (Views.userAjax $ tweetAuthor tweet')

-- | Request a user
--
user :: Snap ()
user = do
    Just userInfo <- join . fmap parseJson <$> getParam "data"
    setBlaze $ Views.user userInfo

-- | Site handler
--
application :: Snap ()
application = do
    gen <- liftIO newStdGen
    serveDirectory "static" <|> route [ ("", ifTop root)
                                      , ("tweet/", generateTweet gen)
                                      , ("user/", user)
                                      , (":id", tweet)
                                      ] 
