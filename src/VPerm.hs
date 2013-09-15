{-# LANGUAGE BangPatterns #-}

module VPerm where

import           Data.List
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Word

import           Util

newtype Perm = Perm (U.Vector Word8)

instance Show Perm where
  show (Perm v) = "Perm \"(" ++
                 (V.foldr ((\s a -> s ++ (' ':a)).show) [] v) ++ ")\""

withinPerm :: (U.Vector Word8 -> U.Vector Word8) -> Perm -> Perm
withinPerm f (Perm v) = Perm $ f v

idPerm :: Word8 -> Perm
idPerm n = Perm $ V.fromList [1..n]
{-# INLINABLE idPerm #-}

swap :: Perm -> Perm
swap = withinPerm mut
  where mut v = U.create $ do
          v' <- V.thaw v
          MU.swap v' 0 1
          return v'
{-# INLINABLE swap #-}

unrot :: Perm -> Perm
unrot = withinPerm mut
  where mut v = U.create $ do
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
-- The index is the number of positions to the left of each n with value
-- less than n times $(n-1)!$ for n up to the length of the permutation.
--
-- We can reverse this recursively; separate the index into its factorial
-- components and then insert a new number into the permutation positioned
-- according to the multiple of the corresponding factorial.
pIndex :: Perm -> Int
pIndex (Perm v) = step (V.length v)
  where step !1 = 0
        step !n = (step (n-1)) + ((fact (n-1)) * (indexLess 0 0))
          where indexLess !a !i = case v V.! i of
                  x | x == n' -> a
                  x | x < n'  -> indexLess (a+1) (i+1)
                  _           -> indexLess a     (i+1)
                n' = fromIntegral n :: Word8
{-# INLINABLE pIndex #-}

pUnIndex :: Int -> Int -> Perm
pUnIndex l n = Perm . V.fromList . foldl' (flip $ uncurry place) [1]
               $ zip (factorials (l-1) n []) ([2..fromIntegral l])
  where factorials 0 _ a = a
        factorials f i a = let (m,r) = i `divMod` (fact f)
                           in factorials (f-1) r (m:a)
{-# INLINABLE pUnIndex #-}
