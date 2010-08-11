-- | Produce HTML from Twitter data structures.
--
{-# LANGUAGE OverloadedStrings #-}
module Twitter.Html
    ( linkTweet
    ) where

import Data.List (intersperse)
import Data.Monoid (mconcat, mappend, mempty)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html5 (a)
import Text.Blaze.Html5.Attributes (href)

-- | Produce a link (if necessary) from a word.
--
linkWord :: Text -> Html
linkWord word 
    -- Empty word (should not happen)
    | T.null word = mempty

    -- @reply
    | T.head word == '@' =
        a ! href ("http://twitter.com/" `mappend` textValue tail')
          $ text word

    -- Hashtag
    | T.head word == '#' =
        a ! href ("http://search.twitter.com/search?q=%23"
                    `mappend` textValue tail')
          $ text word

    -- Link
    | "http://" `T.isPrefixOf` word =
        a ! href (textValue word) $ text word

    -- Regular word
    | otherwise = text word
  where
    tail' = T.tail word

-- | Link all words in a tweet.
--
linkTweet :: Text -> Html
linkTweet = mconcat . intersperse " " . map linkWord . T.words
