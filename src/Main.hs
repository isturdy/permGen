module Main where

import           GHC.Word
import           System.Environment

import qualified Array              as A
import qualified CritBit            as C
import qualified Map                as M

handlers :: [(String, Word8 -> String)]
handlers = [
    ("map",   M.showLongest)
  , ("array", A.showLongest)
  , ("critbit", C.showLongest)
  ]

main :: IO ()
main = do
  [pn,ns] <- getArgs
  let p = lookup pn handlers
  let n = read ns :: Word8
  case p of
    Nothing -> error "Unsupported handler"
    Just f  -> putStrLn . show $ f n
