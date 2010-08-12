-- | Generate tweets based on markov chains.
--
{-# LANGUAGE OverloadedStrings #-}
module Twitter.Markov
    ( markovTweet
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Twitter (TweetInfo (..))
import Data.Markov

-- | Extract a 'Sample' from a tweet.
--
sampleFromTweet :: TweetInfo -> Sample Text
sampleFromTweet = Sample . filter (not . T.null)
                . map (T.filter (`notElem` "()\""))
                . tweetWords

-- | Generate a random tweet
--
markovTweet :: [TweetInfo] -> [Int] -> TweetInfo
markovTweet tweets seeds =
    let samples = map sampleFromTweet tweets
        model = fromSamples True samples
    in TweetInfo { tweetWords = sentence tooLarge model seeds
                 , tweetId    = 0
                 }

-- | Check if a tweet is already too large
--
tooLarge :: [Text] -> Bool
tooLarge ls = length ls + sum (map T.length ls) > 140
