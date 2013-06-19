{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module ArraySimple where

import           Control.Arrow
import           Data.Queue.Simple
import qualified Data.Vector.Generic         as V
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
type Steps = Word8

newSteps :: Steps
newSteps = 0

(<:) :: Steps -> Step -> Steps
s <: _ = s+1
infixl 3 <:

slength :: Steps -> Integer
slength = fromIntegral

-- Algorithm

search :: Word8 -> Vector Steps
search n = U.create $ do
  a <- MU.replicate (fact $ fromIntegral n) 0
  MU.unsafeWrite a 0 newSteps
  qrecM step a initq
    where initq :: SimpleQueue (Perm,Steps,Step)
          initq = fromList [(idPerm n,newSteps,S), (idPerm n,newSteps,U)]
          step !a (p,s,mv) = do
            e <- MU.unsafeRead a newi
            if' (e==0) (MU.unsafeWrite a newi news >> return (a,newq))
                       (return (a,[]))
            where newp = move mv p
                  newi = pIndex newp
                  news = s <: mv
                  newq = case mv of
                    S -> [(newp,news,U)]
                    U -> [(newp,news,U), (newp,news,S)]

longest :: Int -> Vector Steps -> (Perm,Integer)
longest len = first (pUnIndex len) . V.ifoldl' smax (0,0)
  where smax (p,l) i s = let sl = slength s
                         in if sl < l then (p,l) else (i,sl)

move :: Step -> Perm -> Perm
move S = swap
move U = unrot

showLongest :: Word8 -> String
showLongest l = show . (longest (fromIntegral l)) $ search l
