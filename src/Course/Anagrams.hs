{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> Filename -> IO (List Chars)
anagrams s filename = readFile filename >>= (\d ->
    let dictionary = map (map toLower) $ lines d
    in pure $ filter (flip elem dictionary) $ permutations $ map toLower s)

split :: Eq a => List a -> a -> List (List a)
split Nil _ = Nil
split xs d = f xs Nil Nil
    where
        f Nil Nil acc = reverse acc
        f Nil r acc = reverse $ (reverse r) :. acc
        f (y :. ys) Nil acc = if y == d then f ys Nil acc else f ys (y :. Nil) acc
        f (y :. ys) r acc = if y == d then f ys Nil (reverse r :. acc) else f ys (y :. r) acc
        

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: Chars -> Chars -> Bool
equalIgnoringCase s1 s2 =
    let s1' = map toLower s1
        s2' = map toLower s2
    in s1' == s2'
