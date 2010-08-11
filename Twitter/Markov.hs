-- | Generate tweets based on markov chaings.
--
{-# LANGUAGE OverloadedStrings #-}
module Twitter.Markov
    ( Sample
    , fromTweet
    , fromSample
    , Distribution
    , Model
    , fromSamples
    , sentence
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Twitter (TweetInfo (..))

-- | A sample to build a model. In our case, this is simply a tweet.
--
newtype Sample = Sample { unSample :: [Text] }
               deriving (Eq, Ord, Show)

-- | Extract a 'Sample' from a tweet.
--
fromTweet :: TweetInfo -> Sample
fromTweet = Sample . T.words . T.filter (`notElem` "()\"") . tweetBody

-- | Get the 'Text' from a 'Sample'.
--
fromSample :: Sample -> Text
fromSample = T.unwords . unSample

-- | This represents an element in the markov chain. 'End' is a symbolic
-- representation of the virtual last element.
--
data Element = Element Text
             | End
             deriving (Eq, Ord, Show)

-- | A 'Distribution' represents the odds for taking an element as next element.
-- It contains the total size of possibilities, and then for every possible next
-- element a count.
--
-- @count / size@ gives us the chance we take that element as next element.
-- 
data Distribution = Distribution Int (Map Element Int)                        
                  deriving (Show)

instance Monoid Distribution where
    mempty = Distribution 0 mempty
    mappend (Distribution s1 m1) (Distribution s2 m2) =
        Distribution (s1 + s2) $ M.unionWith (+) m1 m2

-- | Create a distribution with a single element in it. This will, of course,
-- always result in this one element.
--
singletonDistribution :: Element -> Distribution
singletonDistribution = Distribution 1 . flip M.singleton 1

-- | Add an element to the distribution. This decreases the chance to pick the
-- other elements.
--
addElement :: Distribution -> Element -> Distribution
addElement (Distribution s m) e =
    Distribution (s + 1) $ M.insertWith (+) e 1 m

-- | Take an item from the distribution, based on a random 'Int'. This 'Int'
-- should not be limited by the distribution's size: it can be anything in
-- @[minBound .. maxBound]@.
--
fromDistribution :: Distribution -> Int -> Element
fromDistribution (Distribution s m) i =
    fromDistribution' (i `mod` s) $ M.toList m
  where
    fromDistribution' _ [] = End  -- Error!
    fromDistribution' _ [(k, _)] = k
    fromDistribution' x ((k, v) : xs) | x > v = fromDistribution' (x - v) xs
                                      | otherwise = k

-- | A model is a markov distribution. It contains an distribution for the start
-- element of a tweet, and a distribution for every word, to determine the next
-- word.
--
data Model = Model Distribution (Map Text Distribution)
           deriving (Show)

instance Monoid Model where
    mempty = Model mempty mempty
    mappend (Model i1 m1) (Model i2 m2) =
        Model (mappend i1 i2) $ M.unionWith mappend m1 m2

-- | Add a 'Sample' to a 'Model', making the 'Model' more realistic.
--
addSample :: Model -> Sample -> Model
addSample model (Sample []) = model
addSample (Model heads distributions) (Sample ls@(x : _)) =
    Model heads' $ addSample' distributions ls
  where
    heads' = addElement heads $ Element x
    addSample' d [] = d
    addSample' d (y : []) = M.insertWith mappend y (singletonDistribution End) d
    addSample' d (y : z : xs) =
        let d' = M.insertWith mappend y (singletonDistribution $ Element z) d
        in addSample' d' $ z : xs

-- | Construct a 'Model' from a 'Sample' list.
--
fromSamples :: [Sample] -> Model
fromSamples = foldl addSample mempty

-- | Create a random 'Sample' from a 'Model', based on a seed list.
--
sentence :: Model -> [Int] -> Sample
sentence _ [] = Sample mempty
sentence (Model heads distributions) (x : samples) =
    let head' = fromDistribution heads x
    in Sample $ sentence' 0 head' samples
  where
    sentence' _ End _ = []
    sentence' _ _ [] = []
    sentence' l (Element e) (y : ys) =
        case M.lookup e distributions of
            Nothing -> []
            Just d ->
                -- The +1 here is the added space
                let l' = l + T.length e + 1
                in if l + T.length e > maxLength
                    then []
                    else e : sentence' l' (fromDistribution d y) ys
    maxLength = 140
