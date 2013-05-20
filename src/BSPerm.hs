{-# LANGUAGE BangPatterns #-}

module BSPerm where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.List       hiding (map)
import           GHC.Word

import           Util

newtype Perm = Perm ByteString
             deriving (Eq,Ord)
instance Show Perm where
  show (Perm bs) = "Perm \"(" ++
                 (intersperse ' ' $ B.foldr ((:).head.show) [] bs) ++ ")\""

idPerm :: Word8 -> Perm
idPerm n = Perm $ B.pack [1..n]
{-# INLINABLE idPerm #-}

swap :: Perm -> Perm
swap (Perm s) = Perm $ B.append (B.reverse h) t
  where (h,t) = B.splitAt 2 s
{-# INLINABLE swap #-}

unrot :: Perm -> Perm
unrot (Perm s) = Perm $ B.snoc t h
  where Just (h,t) = B.uncons s
{-# INLINABLE unrot #-}

-- Exact integer encoding of permutations of up to 20 items
pIndex :: Perm -> Int
pIndex (Perm p) = step (B.length p)
  where step !1 = 0
        step !n = (step (n-1)) + ((fact (n-1)) * (indexLess 0 0))
          where indexLess !a !i = case B.index p i of
                  x | x == n' -> a
                  x | x < n'  -> indexLess (a+1) (i+1)
                  _           -> indexLess a     (i+1)
                n' = fromIntegral n :: Word8
{-# INLINABLE pIndex #-}
