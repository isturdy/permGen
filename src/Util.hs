{-# LANGUAGE BangPatterns #-}

module Util where

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
