module Map where

import           Control.Monad
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as B
import           Data.List
import           Data.Map          hiding (empty, foldl', fromList)
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Queue.Simple
import           GHC.Word

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

--Representation of permutations
newtype Perm = Perm ByteString
             deriving (Eq,Ord)
instance Show Perm where
  show (Perm bs) = "Perm \"(" ++ (intersperse ' ' $ B.foldr ((:).head.show) [] bs) ++ ")\""

idPerm :: Word8 -> Perm
idPerm n = Perm $ B.pack [1..n]

swap :: Perm -> Perm
swap (Perm s) = Perm $ B.append (B.reverse h) t
  where (h,t) = B.splitAt 2 s

unrot :: Perm -> Perm
unrot (Perm s) = Perm $ B.snoc t h
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
          step 1 = Nothing
          step k = Just (unenc (k `mod` 2), k `div` 2)

newSteps :: Steps
newSteps = Steps 1

(<:) :: Steps -> Step -> Steps
(Steps s) <: S = Steps (s*2)
(Steps s) <: U = Steps (s*2 + 1)
infixl 3 <:

slength :: Steps -> Integer
slength (Steps s) = floor . logBase 2 . fromIntegral $ s

-- Algorithm

search :: Word8 -> Map Perm Steps
search n = qrec step initm initq
  where initm = singleton (idPerm n) newSteps
        initq = fromList [(idPerm n,newSteps,S), (idPerm n,newSteps,U)] :: SimpleQueue (Perm,Steps,Step)
        step map (p,s,mv) = if' (member newp map) (map,[]) (newm,newq)
          where newp = move mv p
                news = s <: mv
                newm = M.insert newp news map
                newq = case mv of
                  S -> [(newp,news,U)]
                  U -> [(newp,news,U), (newp,news,S)]

longest :: Map Perm Steps -> (Perm,Integer)
longest = foldrWithKey longerp (idPerm 1,-1)
  where longerp p l (op,on) | slength l <= on = (op,on)
        longerp p l _ = (p,slength l)

showLongest :: Word8 -> String
showLongest = show . longest . search

move :: Step -> Perm -> Perm
move S = swap
move U = unrot
