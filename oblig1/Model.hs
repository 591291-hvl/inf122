module Model (TextModel
             ,createModel
             ,nextDistribution) where
import Control.Monad
import Data.List
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

import NGram

-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight , Weight)

-- The empty model with no n-grams.
emptyModel :: TextModel
emptyModel = Map.empty

-- Update a model with a new n-gram followed by a character.
increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next model
    |model == emptyModel = Map.fromList [(ngram,(Map.fromList [(next,1)],1))]
    |Map.lookup ngram model == Nothing = Map.insert ngram (Map.fromList [(next,1)],1) model
    |otherwise = Map.insert ngram ((\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model), sum $ Map.elems ((\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model))) model
        where insidemap = (\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model)

-- The distribution of next n-grams after a given one.
nextDistribution :: TextModel -> NGram -> Maybe ([(NGram, Weight)],Weight)
nextDistribution model current
    |Map.lookup current model == Nothing = Nothing
    |otherwise = (\(Just (a,b)) -> Just (map (\(x,y) -> ((tail current ++ [x]),y)) (Map.toList a),b)) (Map.lookup current model)

-- Create an n-gram model from a string.
helpfunc :: TextModel -> (NGram,Char) -> TextModel
helpfunc model (gram,char) = increaseWeight gram char model

createModel :: Integer -> String -> TextModel 
createModel n string = foldl' helpfunc emptyModel (gramsWithNext n string)



