{-# LANGUAGE TupleSections #-}

module Array where

import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import           Data.List
import           Data.Queue.Simple
import           GHC.Word

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

--Representation of permutations
-- newtype Perm = Perm Word64
-- deriving (Eq,Ord)

-- idPerm :: Perm
-- idPerm = Perm 0

-- swap

newtype Perm = Perm ByteString
             deriving (Eq,Ord)
instance Show Perm where
  show (Perm bs) = "Perm \"(" ++
                 (intersperse ' ' $ B.foldr ((:).head.show) [] bs) ++ ")\""

idPerm :: Word8 -> Perm
idPerm n = Perm $ B.pack [1..n]

swap :: Perm -> Perm
swap (Perm s) = Perm $ B.append (B.reverse h) t
  where (h,t) = B.splitAt 2 s

unrot :: Perm -> Perm
unrot (Perm s) = Perm $ B.snoc t h
  where Just (h,t) = B.uncons s

-- Exact integer encoding of permutations of up to 20 items
pIndex :: Perm -> Word64
pIndex (Perm p) = step p (fromIntegral $ B.length p)
  where step _  1 = 0
        step p' n = (((fromIntegral h)-1)*(fact (n-1))) +
                   step (adjust h t) (n-1)
          where Just (h,t) = B.uncons p'
        adjust e = B.map (\x -> if' (x>e) (x-1) x)

--Representation of steps taken
data Step = S
          | U
          deriving (Show)

-- Word64 works for n <= 10
-- Because of sharing, this gains little--a list has (higher) constant cost
type Steps = Word64

newSteps :: Steps
newSteps = 1

(<:) :: Steps -> Step -> Steps
s <: S = s*2
s <: U = s*2 + 1
infixl 3 <:

slength :: Steps -> Integer
slength s = floor . logBase (2::Double) . fromIntegral $ s

-- Algorithm

fact :: (Integral a) => a -> a
fact n = product [1..n]

-- Queue (Perm,Steps,Step)
search :: Word8 -> ST s (STUArray s Word64 Steps)
search n = do
  a <- newArray (0,(fact $ fromIntegral n) - 1) 0
  writeArray a 0 newSteps
  qrecM step a initq
    where initq = fromList [(idPerm n,newSteps,S), (idPerm n,newSteps,U)] :: SimpleQueue (Perm,Steps,Step)
          step a (p,s,mv) = do
            e <- readArray a newi
            if' (e==0) (writeArray a newi news) (return ())
            return $ if' (e==0) (a,newq) (a,[])
            where newp = move mv p
                  newi = pIndex newp
                  news = s <: mv
                  newq = case mv of
                    S -> [(newp,news,U)]
                    U -> [(newp,news,U), (newp,news,S)]

longest :: UArray Word64 Steps -> (Perm,Integer)
longest = (idPerm 0,) . foldl' max 0 . fmap (slength . snd) . assocs

move :: Step -> Perm -> Perm
move S = swap
move U = unrot

showLongest :: Word8 -> String
showLongest n = show . longest $ runSTUArray (search n)
