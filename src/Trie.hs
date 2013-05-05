{-# LANGUAGE TupleSections #-}

module Trie where

import           Control.Arrow
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as B
import           Data.List         hiding (insert)
import           Data.Queue.Simple hiding (toList)
import           Data.Trie         hiding (fromList)
import           GHC.Word

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

--Representation of permutations
type Perm = ByteString

idPerm :: Word8 -> Perm
idPerm n = B.pack [1..n]

swap :: Perm -> Perm
swap s = B.append (B.reverse h) t
  where (h,t) = B.splitAt 2 s

unrot :: Perm -> Perm
unrot s = B.snoc t h
  where Just (h,t) = B.uncons s

--Representation of steps taken
data Step = S
          | U
          deriving (Show)

-- Word64 works for n <= 10
-- Because of sharing, this gains little--a list has (higher) constant cost
newtype Steps = Steps Integer
instance Show Steps where
  show (Steps i) = "Steps \"" ++ (reverse $ unfoldr step i) ++ "\""
    where unenc 0 = 'S'
          unenc 1 = 'U'
          unenc _ = error "strange division."
          step 1 = Nothing
          step k = Just (unenc (k `mod` 2), k `div` 2)

newSteps :: Steps
newSteps = Steps 1

(<:) :: Steps -> Step -> Steps
(Steps s) <: S = Steps (s*2)
(Steps s) <: U = Steps (s*2 + 1)
infixl 3 <:

slength :: Steps -> Integer
slength (Steps s) = floor . logBase (2::Double) . fromIntegral $ s

-- Algorithm

search :: Word8 -> Trie Steps
search n = qrec step initm initq
  where initm = singleton (idPerm n) newSteps
        initq = fromList [(idPerm n,newSteps,S), (idPerm n,newSteps,U)] :: SimpleQueue (Perm,Steps,Step)
        step t (p,s,mv) = if' (member newp t) (t,[]) (newt,newq)
          where newp = move mv p
                news = s <: mv
                newt = insert newp news t
                newq = case mv of
                  S -> [(newp,news,U)]
                  U -> [(newp,news,U), (newp,news,S)]

longest :: Trie Steps -> (Perm,Integer)
longest = foldl' longerp (idPerm 1,-1) .
          fmap (second slength) . toList
  where longerp (ap,al) (_,l) | l <= al = (ap,al)
        longerp _ t = t

showLongest :: Word8 -> String
showLongest = show . longest . search

move :: Step -> Perm -> Perm
move S = swap
move U = unrot
