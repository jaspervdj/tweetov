-- | Markov chain data structure
--
module Tweetov.Data.Markov
    ( Sample (..)
    , Distribution
    , Model
    , fromSamples
    , sentence
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid

-- | A sample to build a model. In our case, this is simply a tweet.
--
newtype Sample a = Sample { unSample :: [a] }
                 deriving (Eq, Ord, Show)

-- | This represents an element in the markov chain. 'End' is a symbolic
-- representation of the virtual last element.
--
data Element a = Element a
               | End
               deriving (Eq, Ord, Show)

-- | A 'Distribution' represents the odds for taking an element as next element.
-- It contains the total size of possibilities, and then for every possible next
-- element a count.
--
-- @count / size@ gives us the chance we take that element as next element.
-- 
data Distribution a = Distribution Int (Map (Element a) Int)                        
                    deriving (Show)

instance Ord a => Monoid (Distribution a) where
    mempty = Distribution 0 mempty
    mappend (Distribution s1 m1) (Distribution s2 m2) =
        Distribution (s1 + s2) $ M.unionWith (+) m1 m2

-- | Create a distribution with a single element in it. This will, of course,
-- always result in this one element.
--
singletonDistribution :: Element a -> Distribution a
singletonDistribution = Distribution 1 . flip M.singleton 1

-- | Add an element to the distribution. This decreases the chance to pick the
-- other elements.
--
addElement :: Ord a => Distribution a -> Element a -> Distribution a
addElement (Distribution s m) e =
    Distribution (s + 1) $ M.insertWith (+) e 1 m

-- | Take an item from the distribution, based on a random 'Int'. This 'Int'
-- should not be limited by the distribution's size: it can be anything in
-- @[minBound .. maxBound]@.
--
fromDistribution :: Distribution a -> Int -> Element a
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
data Model a = Model (Distribution a) (Map a (Distribution a))
             deriving (Show)

instance Ord a => Monoid (Model a) where
    mempty = Model mempty mempty
    mappend (Model i1 m1) (Model i2 m2) =
        Model (mappend i1 i2) $ M.unionWith mappend m1 m2

-- | Add a 'Sample' to a 'Model', making the 'Model' more realistic.
--
addSample :: Ord a
          => Bool      -- ^ Should we insert end elements
          -> Model a   -- ^ Model to expand
          -> Sample a  -- ^ Sample to expand the model with
          -> Model a   -- ^ Resulting model
addSample _ model (Sample []) = model
addSample insertEnd (Model heads distributions) (Sample ls@(x : _)) =
    Model heads' $ addSample' distributions ls
  where
    heads' = addElement heads $ Element x
    -- This should not happen, actually
    addSample' d [] = d
    -- Insert the end only if that flag is set
    addSample' d (y : []) = if insertEnd
        then M.insertWith mappend y (singletonDistribution End) d else d
    -- Insert one sequence
    addSample' d (y : z : xs) =
        let d' = M.insertWith mappend y (singletonDistribution $ Element z) d
        in addSample' d' $ z : xs

-- | Construct a 'Model' from a 'Sample' list.
--
fromSamples :: Ord a
            => Bool        -- ^ Should we insert end elements
            -> [Sample a]  -- ^ Samples to build a model from
            -> Model a     -- ^ Resulting model
fromSamples insertEnd = foldl (addSample insertEnd) mempty

-- | Create a random 'Sample' from a 'Model', based on a seed list.
--
sentence :: Ord a
         => ([a] -> Bool)  -- ^ Check if a given sequence is satisfactory
         -> Model a        -- ^ Model to build the sequence with
         -> [Int]          -- ^ Random int pool
         -> [a]            -- ^ Resulting sequence
sentence _ _ [] = []
sentence tooLarge (Model heads distributions) (x : seeds) =
    let head' = fromDistribution heads x
    in reverse $ sentence' [] head' seeds
  where
    sentence' l End _ = l
    sentence' l (Element e) [] = e : l
    sentence' l (Element e) (y : ys) = if tooLarge (e : l)
        then l
        else case M.lookup e distributions of
            Nothing -> e : l
            Just d -> let n = fromDistribution d y
                      in sentence' (e : l) n ys
