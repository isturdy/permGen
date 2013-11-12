{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}

module ArrayFast where

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
          | R
          deriving (Show)

-- The last bit shows the last step taken+1 (0=S), the rest shows the length
newtype Steps = Steps Word16
              deriving (Eq, V.Vector Vector, MV.MVector MU.MVector, MU.Unbox,
                        Show)

newSteps :: Steps
newSteps = Steps 5

nullSteps :: Steps
nullSteps = Steps 0

splitSteps :: Steps -> (Word16, Step)
splitSteps (Steps ss) = let (l,s) = quotRem ss 4
                        in (l, case s of 0 -> S; 1 -> U; 2 -> R; _ -> undefined)

getStep :: Steps -> Step
getStep = (^._2) . splitSteps

(<:) :: Steps -> Step -> Steps
ss <: s = let (l,_) = splitSteps ss
          in Steps ((l+1)*4+case s of S -> 0; U -> 1; R -> 2)
infixl 3 <:

slength :: Steps -> Integer
slength s = if s == nullSteps then -1
            else fromIntegral $ ((splitSteps s)^._1) - 1

-- Algorithm

search :: Word8 -> Vector Steps
search n = U.create $ do
  a <- MU.replicate (fact $ fromIntegral n) nullSteps
  MU.unsafeWrite a (pIndex $ idPerm n) newSteps
  qrecM step a initq
    where initq :: SimpleQueue (Perm,Steps)
          initq = fromList [(idPerm n,newSteps <: S),
                            (idPerm n,newSteps <: U),
                            (idPerm n,newSteps <: R)]
          step !a (p,s) = do
            e <- MU.unsafeRead a newi
            if' (e==nullSteps) (MU.unsafeWrite a newi s >> return (a,newq))
                       (return (a,[]))
            where (_len,mv) = splitSteps s
                  newp = move mv p
                  newi = pIndex newp
                  newq = case mv of
                    S -> [(newp,s <: U), (newp,s <: R)]
                    U -> [(newp,s <: U), (newp,s <: S)]
                    R -> [(newp,s <: R), (newp,s <: S)]

longest :: Int -> Vector Steps -> (Perm,Integer,String)
longest len v = let (p,ss) = V.ifoldl' smax (0,nullSteps) v
                in (pUnIndex len p, slength ss, getSteps len v p)
  where smax (p,l) i s = if slength(l) > slength(s) then (p,l) else (i,s)

getSteps :: Int -> Vector Steps -> Int -> String
getSteps len v i
 | i == (fact len) - 1 = []
 | True = let x = V.unsafeIndex v i
              s = getStep x
              i' = pIndex . invMove s $ pUnIndex len i
          in (getSteps len v i')++(show s)

move :: Step -> Perm -> Perm
move S = swap
move U = unrot
move R = rot

invMove :: Step -> Perm -> Perm
invMove S = swap
invMove U = rot
invMove R = unrot

showLongest :: Word8 -> String
showLongest l = show . (longest (fromIntegral l)) $ search l
