{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}

module Array where

import           Control.Lens
import           Data.Queue.Simple
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Word

import           Util
import           VPerm

--Representation of steps taken
data Step = S
          | U
          deriving (Show)

-- Word64 works for n <= 10
-- Because of sharing, this gains little--a list has (higher) constant cost
newtype Steps = Steps Word64
              deriving (Eq, V.Vector Vector, MV.MVector MU.MVector, MU.Unbox)

instance Show Steps where
  show (Steps s) = go s
    where go 1 = ""
          go n = let (q,r) = quotRem n 2
                 in case r of
                   0 -> 'S':(go q)
                   1 -> 'U':(go q)
                   _ -> error "The impossible happened."

newSteps :: Steps
newSteps = Steps 1

nullSteps :: Steps
nullSteps = Steps 0

(<:) :: Steps -> Step -> Steps
(Steps s) <: S = Steps $ s*2
(Steps s) <: U = Steps $ s*2 + 1
infixl 3 <:

slength :: Steps -> Integer
slength (Steps s) = floor . logBase (2::Double) . fromIntegral $ s

-- Algorithm

search :: Word8 -> Vector Steps
search n = U.create $ do
  a <- MU.replicate (fact $ fromIntegral n) nullSteps
  MU.unsafeWrite a (pIndex $ idPerm n) newSteps
  qrecM step a initq
    where initq :: SimpleQueue (Perm,Steps,Step)
          initq = fromList [(idPerm n,newSteps,S), (idPerm n,newSteps,U)]
          step !a (p,s,mv) = do
            e <- MU.unsafeRead a newi
            if' (e==nullSteps) (MU.unsafeWrite a newi news >> return (a,newq))
                       (return (a,[]))
            where newp = move mv p
                  newi = pIndex newp
                  news = s <: mv
                  newq = case mv of
                    S -> [(newp,news,U)]
                    U -> [(newp,news,U), (newp,news,S)]

longest :: Int -> Vector Steps -> (Perm,Integer,Steps)
longest len = (_1 %~ (pUnIndex len)) . V.ifoldl' smax (0,0,newSteps)
  where smax (p,l,ss) i s = let sl = slength s
                            in if sl < l then (p,l,ss) else (i,sl,s)

move :: Step -> Perm -> Perm
move S = swap
move U = unrot

showLongest :: Word8 -> String
showLongest l = show . (longest (fromIntegral l)) $ search l
