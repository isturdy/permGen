{-# LANGUAGE BangPatterns #-}

module Util where

import           Data.List
import qualified Data.Vector.Generic as V
import           Data.Vector.Unboxed (Vector)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
{-# INLINABLE if' #-}

facts :: Vector Int
facts = V.generate 15 (\n -> product [1..n])
{-# NOINLINE facts #-}

fact :: Int -> Int
fact = (facts V.!)
{-# INLINABLE fact #-}

place :: Int -> a -> [a] -> [a]
place p e l = h ++ (e:t)
  where (h,t) = splitAt p l
