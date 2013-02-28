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

import Data.Aeson (FromJSON (..), json)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec (parseOnly)
import Snap.Blaze (blaze)
import Snap.Core
import Snap.Util.FileServe (serveDirectory)
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as T

import Tweetov.Twitter.Markov
import qualified Tweetov.Views as Views

-- | Utility: parse JSON from a strict bytestring
--
parseJson :: FromJSON a => SB.ByteString -> Maybe a
parseJson bs = case parseOnly json bs >>= parseEither parseJSON of
    Left _  -> Nothing
    Right x -> x

-- | Site root
--
root :: Snap ()
root = blaze $ Views.root mempty mempty

-- | Request a tweet
--
generateTweet :: RandomGen g => g -> Snap ()
generateTweet gen = do
    Just tweets <- join . fmap parseJson <$> getParam "data"
    Just user' <- fmap T.decodeUtf8 <$> getParam "user"
    let randoms' = randoms gen
        tweet' = markovTweet user' tweets randoms'
    blaze $ Views.tweet tweet'

-- | Request a user
--
user :: Snap ()
user = do
    Just userInfo <- join . fmap parseJson <$> getParam "data"
    blaze $ Views.user userInfo

-- | Site handler
--
application :: Snap ()
application = do
    gen <- liftIO newStdGen
    serveDirectory "static" <|> route [ ("", ifTop root)
                                      , ("tweet/", generateTweet gen)
                                      , ("user/", user)
                                      ]
