{-# LANGUAGE BangPatterns #-}

module VPerm where

import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Word

import           Util

type Perm = U.Vector Word8

idPerm :: Word8 -> Perm
idPerm n = V.fromList [1..n]
{-# INLINABLE idPerm #-}

swap :: Perm -> Perm
swap v = U.create $ do
  v' <- V.thaw v
  MU.swap v' 0 1
  return v'
{-# INLINABLE swap #-}

unrot :: Perm -> Perm
unrot v = U.create $ do
  let end = (U.length v) - 1
  let h = v U.! 0
  v' <- U.thaw v
  let hv = MU.unsafeSlice 0 end v'
  let tv = MU.unsafeSlice 1 end v'
  MU.move hv tv
  MU.write v' end h
  return v'
{-# INLINABLE unrot #-}

-- Exact integer encoding of permutations of up to 20 items
pIndex :: Perm -> Int
pIndex v = step (V.length v)
  where step !1 = 0
        step !n = (step (n-1)) + ((fact (n-1)) * (indexLess 0 0))
          where indexLess !a !i = case v V.! i of
                  x | x == n' -> a
                  x | x < n'  -> indexLess (a+1) (i+1)
                  _           -> indexLess a     (i+1)
                n' = fromIntegral n :: Word8
{-# INLINABLE pIndex #-}
