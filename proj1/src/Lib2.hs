{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( prefixes
    ) where

prefixes :: [a] -> [[a]]
prefixes = foldl (\x acc -> x : acc) [[]]