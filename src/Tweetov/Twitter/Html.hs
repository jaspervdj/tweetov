-- | Produce HTML from Twitter data structures.
--
{-# LANGUAGE OverloadedStrings #-}
module Tweetov.Twitter.Html
    ( viewTweet
    ) where

import Data.List (intersperse)
import Data.Monoid (mconcat, mappend, mempty)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html
import Text.Blaze.Html5 (a)
import Text.Blaze.Html5.Attributes (href)

import Tweetov.Twitter (Tweet, tweetWords)

-- | Produce a link (if necessary) from a word.
--
linkWord :: Text -> Html
linkWord word
    -- Empty word (should not happen)
    | T.null word = mempty

    -- @reply
    | T.head word == '@' =
        a ! href ("http://twitter.com/" `mappend` toValue tail') $ toHtml word

    -- Hashtag
    | T.head word == '#' =
        a ! href ("http://search.twitter.com/search?q=%23"
                    `mappend` toValue tail')
          $ preEscapedToHtml word

    -- Link
    | "http://" `T.isPrefixOf` word =
        a ! href (toValue word) $ preEscapedToHtml word

    -- Regular word
    | otherwise = preEscapedToHtml word
  where
    tail' = T.tail word

-- | Produce a pretty HTML tweet
--
viewTweet :: Tweet -> Html
viewTweet = mconcat . intersperse " " . map linkWord . tweetWords
