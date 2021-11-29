module Nano where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.Utils (charAt)
import Effect (Effect)
import Effect.Random (randomInt)

--Alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz-"
-- Get alphabet. Either default or specified by user
--let alphabet = Alphabet
-- Get random number between 0 and size of (string - 1) input to the function
alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz-"
random_index :: String -> Effect Int
random_index s = length s # randomInt 0
-- get the element at the specified index.
-- this function is from Data.Char.Utils
get_elem_at :: String -> Int -> Maybe String
get_elem_at s i = charAt i s

-- Function for getting a random element from a string
get_random_elem_from_string :: String -> Effect (Maybe String)
get_random_elem_from_string s = do
  i <- random_index s
  el <- pure $ get_elem_at s i
  pure el

-- Do it recursively until size of NanoID is equal to the desired size
-- create :: String
-- create = create 21

-- create :: Int -> String
-- create i = (create (Just alphabet) i) <> create (i-1)

-- create :: String -> Int -> String
-- create s i | i <= 0 = ""
--            | i > 0  = (get_random_elem_from_string s) <> create s (i-1)

create :: Maybe String -> Int -> Effect (Maybe String)
create ms i | i > 0  = do 
                let s = fromMaybe alphabet ms
                mes <- get_random_elem_from_string s
                xs <- create ms (i-1)
                pure (mes <> xs)
            | otherwise = pure Nothing
-- create :: Int -> String
-- create i = if i > 0
--            then (get_random_elem_from_string i) <> create (i-1)
--            else ""